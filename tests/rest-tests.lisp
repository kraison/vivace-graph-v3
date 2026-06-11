;;;; Tests for the REST layer (rest.lisp): the json-encode serialization
;;;; contract and the request handlers (rest-get/post/put/delete-vertex|edge,
;;;; rest-list-edges, rest-get-graph) exercised IN-PROCESS.
;;;;
;;;; We do not stand up a real HTTP server or the htpasswd/openssl-backed
;;;; auth-rest-user (which shells out to external binaries).  Instead each
;;;; handler is called directly with a params alist shaped exactly as ningle
;;;; delivers it -- route captures (:graph-name, :node-id, :type) as KEYWORD
;;;; keys, query/body params ("username", "name", "from", ...) as STRING keys.
;;;; WITH-REST-ENV stubs auth-rest-user to a fixed verdict and binds a fresh
;;;; ningle:*response* so the 401/404 branches can set a status.
;;;;
;;;; Reuses the g-person / g-knows schema from graph-tests.lisp and the
;;;; WITH-TEST-GRAPH fixture (which registers the graph under
;;;; *integration-graph-name*, so with-rest-graph's lookup-graph resolves it).

(in-package #:graph-db/test)

(def-suite rest-suite
  :description "REST layer: json-encode contract + in-process request handlers."
  :in graph-db-suite)

(in-suite rest-suite)

(defun rest-graph-name ()
  "The graphName route param that with-rest-graph maps back to the test graph."
  (json:lisp-to-camel-case (symbol-name *integration-graph-name*)))

(defun rest-decode (json)
  "Decode a handler's JSON string into a cl-json alist (object keys -> keywords)."
  (json:decode-json-from-string json))

(defmacro with-rest-env ((&key (auth t)) &body body)
  "Run BODY with graph-db::auth-rest-user stubbed to always return AUTH and a
fresh ningle:*response* bound (status 200).  Restores auth-rest-user after."
  (let ((saved (gensym "AUTH")))
    `(let ((,saved (symbol-function 'graph-db::auth-rest-user)))
       (unwind-protect
            (progn
              (setf (symbol-function 'graph-db::auth-rest-user)
                    (lambda (u p) (declare (ignore u p)) ,auth))
              (let ((ningle:*response* (lack/response:make-response 200)))
                ,@body))
         (setf (symbol-function 'graph-db::auth-rest-user) ,saved)))))

(defun rest-params (&rest extra)
  "An auth+graph params alist (the shape ningle hands a handler), with EXTRA
conses appended.  Route-capture keys must be keywords; the rest strings."
  (append (list (cons "username" "u")
                (cons "password" "p")
                (cons :graph-name (rest-graph-name)))
          extra))

(defun rest-status ()
  (lack/response:response-status ningle:*response*))

;;; ---------------------------------------------------------------------------
;;; json-encode -- the serialization contract (no auth / server needed)
;;; ---------------------------------------------------------------------------

(test json-encode-vertex-has-id-type-and-slots
  "json-encode of a vertex emits its string id, camelCased type, and data slots."
  (with-test-graph (g)
    (let (v)
      (with-transaction () (setq v (make-g-person :name "Alice" :age 30)))
      (let ((j (rest-decode (graph-db::json-encode v))))
        (is (string= (string-id v) (cdr (assoc :id j))))
        (is (string= "gPerson" (cdr (assoc :type j))))
        (is (string= "Alice" (cdr (assoc :name j))))
        (is (= 30 (cdr (assoc :age j))))))))

(test json-encode-edge-has-endpoints-and-slots
  "json-encode of an edge emits id, type, from, to and its data slots."
  (with-test-graph (g)
    (let (a b e)
      (with-transaction ()
        (setq a (make-g-person :name "A") b (make-g-person :name "B"))
        (make-g-knows :from a :to b :since "2020"))
      (setq e (first (outgoing-edges a)))
      (let ((j (rest-decode (graph-db::json-encode e))))
        (is (string= (string-id e) (cdr (assoc :id j))))
        (is (string= "gKnows" (cdr (assoc :type j))))
        (is (string= (string-id a) (cdr (assoc :from j))))
        (is (string= (string-id b) (cdr (assoc :to j))))
        (is (string= "2020" (cdr (assoc :since j))))))))

(test json-encode-edge-list-is-an-array
  "json-encode-edge-list emits a JSON array with one object per edge."
  (with-test-graph (g)
    (let (a b c)
      (with-transaction ()
        (setq a (make-g-person :name "A") b (make-g-person :name "B")
              c (make-g-person :name "C"))
        (make-g-knows :from a :to b)
        (make-g-knows :from a :to c))
      (let ((arr (rest-decode (graph-db::json-encode-edge-list (outgoing-edges a)))))
        (is (listp arr))
        (is (= 2 (length arr)))
        (is (every (lambda (o) (string= "gKnows" (cdr (assoc :type o)))) arr))))))

(test json-encode-graph-describes-schema
  "json-encode of a graph emits its name, read/write mode and the vertex/edge
type schema."
  (with-test-graph (g)
    (let ((j (rest-decode (graph-db::json-encode g))))
      (is (string= (rest-graph-name) (cdr (assoc :name j))))
      (is (string= "graph" (cdr (assoc :type j))))
      (is (string= "readWrite" (cdr (assoc :mode j))))
      (let* ((vtypes (cdr (assoc :vertex-types j)))
             (names (mapcar (lambda (vt) (cdr (assoc :name vt))) vtypes)))
        (is-true (member "gPerson" names :test #'string=)
                 "graph schema should list the gPerson vertex type")
        ;; gPerson lists its declared slots
        (let* ((gp (find "gPerson" vtypes
                         :key (lambda (vt) (cdr (assoc :name vt))) :test #'string=))
               (slot-names (mapcar (lambda (s) (cdr (assoc :name s)))
                                   (cdr (assoc :slots gp)))))
          (is-true (member "name" slot-names :test #'string=))
          (is-true (member "age" slot-names :test #'string=))))
      (let ((etypes (mapcar (lambda (et) (cdr (assoc :name et)))
                            (cdr (assoc :edge-types j)))))
        (is-true (member "gKnows" etypes :test #'string=)
                 "graph schema should list the gKnows edge type")))))

;;; ---------------------------------------------------------------------------
;;; Request handlers -- CRUD round trips (auth stubbed, in-process)
;;; ---------------------------------------------------------------------------

(test rest-get-graph-returns-schema-json
  (with-test-graph (g)
    (with-rest-env ()
      (let ((j (rest-decode (graph-db::rest-get-graph (rest-params)))))
        (is (string= (rest-graph-name) (cdr (assoc :name j))))
        (is (string= "readWrite" (cdr (assoc :mode j))))))))

(test rest-post-vertex-creates-and-persists
  "POST vertex creates the vertex in the store and echoes it as JSON."
  (with-test-graph (g)
    (with-rest-env ()
      (let* ((out (graph-db::rest-post-vertex
                   (rest-params (cons :type "gPerson")
                                (cons "name" "Zoe") (cons "age" 99))))
             (j (rest-decode out))
             (id (cdr (assoc :id j))))
        (is (string= "gPerson" (cdr (assoc :type j))))
        (is (string= "Zoe" (cdr (assoc :name j))))
        (is (= 99 (cdr (assoc :age j))))
        ;; the vertex is actually in the store
        (is (= 1 (length (map-vertices #'identity g :collect-p t
                                                 :vertex-type 'g-person))))
        (let ((v (lookup-vertex id)))
          (is-true v "posted vertex should be retrievable by its returned id")
          (is (string= "Zoe" (slot-value v 'name))))))))

(test rest-get-vertex-returns-the-vertex
  (with-test-graph (g)
    (let (id)
      (with-transaction () (setq id (string-id (make-g-person :name "Gettable" :age 7))))
      (with-rest-env ()
        (let ((j (rest-decode
                  (graph-db::rest-get-vertex (rest-params (cons :node-id id))))))
          (is (string= id (cdr (assoc :id j))))
          (is (string= "Gettable" (cdr (assoc :name j))))
          (is (= 7 (cdr (assoc :age j)))))))))

(test rest-put-vertex-updates-slots
  "PUT vertex updates the named slots and persists the change."
  (with-test-graph (g)
    (let (id)
      (with-transaction () (setq id (string-id (make-g-person :name "Before" :age 1))))
      (with-rest-env ()
        (let ((j (rest-decode
                  (graph-db::rest-put-vertex
                   (rest-params (cons :node-id id) (cons "name" "After"))))))
          (is (string= "After" (cdr (assoc :name j))))))
      ;; persisted
      (is (string= "After" (slot-value (lookup-vertex id) 'name))))))

(test rest-delete-vertex-soft-deletes
  "DELETE vertex marks it deleted and removes it from live type scans.  (NB:
lookup-vertex still resolves a soft-deleted node, so a GET by id returns it
flagged rather than 404 -- this asserts the documented current behavior.)"
  (with-test-graph (g)
    (let (id)
      (with-transaction () (setq id (string-id (make-g-person :name "Doomed"))))
      (with-rest-env ()
        (graph-db::rest-delete-vertex (rest-params (cons :node-id id)))
        (let ((v (lookup-vertex id)))
          (is-true (or (null v) (deleted-p v))
                   "deleted vertex must be nil or deleted-p")))
      ;; gone from a live type scan
      (is (null (map-vertices #'identity g :collect-p t :vertex-type 'g-person))
          "soft-deleted vertex should not appear in a live scan"))))

(test rest-post-get-and-list-edges
  "POST edge between two vertices, GET it, and list a vertex's edges."
  (with-test-graph (g)
    (let (aid bid)
      (with-transaction ()
        (setq aid (string-id (make-g-person :name "From"))
              bid (string-id (make-g-person :name "To"))))
      (with-rest-env ()
        (let* ((out (graph-db::rest-post-edge
                     (rest-params (cons :type "gKnows")
                                  (cons "from" aid) (cons "to" bid))))
               (j (rest-decode out))
               (eid (cdr (assoc :id j))))
          (is (string= "gKnows" (cdr (assoc :type j))))
          (is (string= aid (cdr (assoc :from j))))
          (is (string= bid (cdr (assoc :to j))))
          ;; GET the edge back
          (let ((gj (rest-decode
                     (graph-db::rest-get-edge (rest-params (cons :node-id eid))))))
            (is (string= eid (cdr (assoc :id gj)))))
          ;; list edges for the source vertex
          (let ((arr (rest-decode
                      (graph-db::rest-list-edges (rest-params (cons :node-id aid))))))
            (is (= 1 (length arr)))
            (is (string= eid (cdr (assoc :id (first arr)))))))))))

;;; ---------------------------------------------------------------------------
;;; Error paths -- auth, unknown graph / vertex / type
;;; ---------------------------------------------------------------------------

(test rest-auth-failure-returns-401
  "A failed credential check yields HTTP 401 and an error body."
  (with-test-graph (g)
    (with-rest-env (:auth nil)
      (let ((j (rest-decode (graph-db::rest-get-graph (rest-params)))))
        (is (= 401 (rest-status)))
        (is-true (assoc :error j) "401 body should carry an :error")))))

(test rest-unknown-graph-returns-404
  (with-test-graph (g)
    (with-rest-env ()
      (let ((j (rest-decode
                (graph-db::rest-get-graph
                 (list (cons "username" "u") (cons "password" "p")
                       (cons :graph-name "noSuchGraph"))))))
        (is (= 404 (rest-status)))
        (is-true (assoc :error j))))))

(test rest-unknown-vertex-returns-404
  (with-test-graph (g)
    (with-rest-env ()
      (graph-db::rest-get-vertex
       (rest-params (cons :node-id "00000000000000000000000000000000")))
      (is (= 404 (rest-status))))))

(test rest-post-vertex-unknown-type-errors
  "POSTing an unknown vertex type returns an error body (not a crash)."
  (with-test-graph (g)
    (with-rest-env ()
      (let ((j (rest-decode
                (graph-db::rest-post-vertex
                 (rest-params (cons :type "noSuchType") (cons "name" "x"))))))
        (is-true (assoc :error j)
                 "unknown vertex type should yield an :error body")))))
