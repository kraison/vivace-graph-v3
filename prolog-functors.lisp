(in-package #:graph-db)

(defvar *prolog-global-functors*
  (make-hash-table :synchronized t :test 'equalp))

(defmacro def-global-prolog-functor (name lambda-list &body body)
  `(prog1
       (defun ,name ,lambda-list ,@body)
     (export ',name)
     (setf (gethash ',name *prolog-global-functors*) #',name)))

(defun default-functor-p (symbol)
  (gethash symbol *prolog-global-functors*))

(def-global-prolog-functor read/1 (exp cont)
  (if (unify exp (read)) (funcall cont)))

(def-global-prolog-functor write/1 (exp cont)
  (format t "~A" (deref-exp exp)) (funcall cont))

(def-global-prolog-functor nl/0 (cont)
  (terpri) (funcall cont))

(def-global-prolog-functor repeat/0 (cont)
  (loop (funcall cont)))

(def-global-prolog-functor fail/0 (cont)
  (declare (ignore cont))
  nil)

(def-global-prolog-functor =/2 (?arg1 ?arg2 cont)
  "Unifies two prolog variables."
  (if (unify ?arg1 ?arg2) (funcall cont)))

(def-global-prolog-functor ==/2 (?arg1 ?arg2 cont)
  "Checks equality of the values of two prolog variables."
  (if (deref-equal ?arg1 ?arg2) (funcall cont)))

(def-global-prolog-functor /=/2 (?arg1 ?arg2 cont)
  "Checks inequality of the values of two prolog variables."
  (if (not (deref-equal ?arg1 ?arg2)) (funcall cont)))

(def-global-prolog-functor >/2 (?arg1 ?arg2 cont)
  "Prolog greater than functor."
  (if (and (numberp (var-deref ?arg1)) (numberp (var-deref ?arg2))
           (> ?arg1 ?arg2))
      (funcall cont)))

(def-global-prolog-functor </2 (?arg1 ?arg2 cont)
  "Prolog less than functor."
  (if (and (numberp (var-deref ?arg1)) (numberp (var-deref ?arg2))
           (< ?arg1 ?arg2))
      (funcall cont)))

(def-global-prolog-functor >=/2 (?arg1 ?arg2 cont)
  "Prolog greater than or equal to functor."
  (if (and (numberp (var-deref ?arg1))
           (numberp (var-deref ?arg2))
           (>= ?arg1 ?arg2))
      (funcall cont)))

(def-global-prolog-functor <=/2 (?arg1 ?arg2 cont)
  "Prolog less than or equal to functor."
  (if (and (numberp (var-deref ?arg1))
           (numberp (var-deref ?arg2))
           (<= ?arg1 ?arg2))
      (funcall cont)))

(def-global-prolog-functor numberp/1 (x cont)
  (when (numberp (var-deref x))
    (funcall cont)))

(def-global-prolog-functor atom/1 (x cont)
  (when (atom (var-deref x))
    (funcall cont)))

(def-global-prolog-functor not-in-list/2 (item list cont)
  (when (null (find (var-deref item) (var-deref list) :test 'node-equal))
    (funcall cont)))

(def-global-prolog-functor lisp/2 (?result exp cont)
  "Call out to lisp from within a Prolog query.  Assigns result to the
 supplied Prolog var.  (lisp ?result (+ 1 2)).  Any lisp variables that you
 wish to access within a prolog query using the lisp functor should be
 declared special."
  (let ((exp (deref-exp exp)))
    (when *prolog-trace* (format t "TRACE: LISP/2 ?result <- ~A~%" exp))
    (cond ((consp exp)
	   (if (unify ?result (eval exp))
	   ;;(if (unify ?result (apply (first exp) (rest exp)))
	       (funcall cont)))
	  ((symbolp exp)
	   ;;(if (unify ?result (eval exp))
	   (if (unify ?result (funcall #'symbol-value exp))
	       (funcall cont)))
	  (t
	   (if (unify ?result exp)
	       (funcall cont))))))

(def-global-prolog-functor lispp/1 (exp cont)
  "Call out to lisp from within a Prolog query and throws away the result.
 Any lisp variables that you wish to access within a prolog query using the
 lisp functor should be declared special."
  (let ((exp (deref-exp exp)))
    (when *prolog-trace* (format t "TRACE: LISPP/1 ~A~%" exp))
    (cond ((consp exp)
	   ;;(format t "applying ~A to ~A~%" (first exp) (rest exp))
	   (eval exp))
	   ;;(apply (first exp) (rest exp)))
	  ((and (symbolp exp) (boundp exp)) (funcall #'identity exp))
	  (t exp))
    (funcall cont)))

(def-global-prolog-functor regex-match/2 (?arg1 ?arg2 cont)
  "Functor that treats first arg as a regex and uses cl-ppcre:scan to check
 for the pattern in the second arg."
  (if (and (stringp (var-deref ?arg1))
	   (stringp (var-deref ?arg2))
	   (cl-ppcre:scan ?arg1 ?arg2))
      (funcall cont)))

(def-global-prolog-functor var/1 (?arg1 cont)
  (if (unbound-var-p ?arg1) (funcall cont)))

(def-global-prolog-functor is/2 (var exp cont)
  "Similar to lisp/2, but unifies instead of assigns the lisp return value."
  (if (and (not (find-if-anywhere #'unbound-var-p exp))
	   (unify var (eval (deref-exp exp))))
      (funcall cont)))

(def-global-prolog-functor call/1 (goal cont)
  "Call a prolog form."
  (var-deref goal)
  (let* ((functor (make-functor-symbol (first goal) (length (args goal)))))
    (let ((fn (or (gethash functor *user-functors*)
		  (gethash functor *prolog-global-functors*))))
      (if (functionp fn)
	  (apply fn (append (args goal) (list cont)))
	  (error 'prolog-error
		 :reason
		 (format nil "Unknown Prolog functor in call/1 ~A"
                         functor))))))

(def-global-prolog-functor if/2 (?test ?then cont)
  (when *prolog-trace* (format t "TRACE: IF/2(~A ~A)~%" ?test ?then))
  (call/1 ?test #'(lambda () (call/1 ?then cont))))

(def-global-prolog-functor if/3 (?test ?then ?else cont)
  (when *prolog-trace* (format t "TRACE: IF/3(~A ~A ~A)~%" ?test ?then ?else))
  (call/1 ?test #'(lambda ()
		    (call/1 ?then
			    #'(lambda () (funcall cont) (return-from if/3)))))
  (call/1 ?else cont))

(let ((date-regex
       "^(19|20)\\d\\d[\-\ \/\.](0[1-9]|1[012])[\-\ \/\.](0[1-9]|[12][0-9]|3[01])$"))
  (def-global-prolog-functor valid-date-p/1 (date cont)
    "Date validation functor. FIXME: This needs to be fleshed out with a
 more comprehensive regex."
    (var-deref date)
    (if (and (stringp date)
             (cl-ppcre:scan date-regex date))
        (funcall cont))))

(def-global-prolog-functor trigger/1 (exp cont)
  "Call out to lisp ignoring the return value."
  (eval (deref-exp exp))
  ;;(let ((exp (deref-exp exp)))
    ;;(typecase exp
      ;;(cons   (apply (first exp) (rest exp)))
      ;;(symbol (symbol-value exp))))
  (funcall cont))

(def-global-prolog-functor not/1 (relation cont)
  "Prolog negation.  Does not retract, simply negates in the context of the
query."
  (with-undo-bindings
    (call/1 relation #'(lambda () (return-from not/1 nil)))
    (funcall cont)))

(def-global-prolog-functor or/2 (goal1 goal2 cont)
  (call/1 goal1 #'(lambda () (funcall cont) (return-from or/2 t)))
  (call/1 goal2 #'(lambda () (funcall cont) (return-from or/2 t))))

(def-global-prolog-functor bagof/3 (exp goal result cont)
  (let ((answers nil))
    (call/1 goal #'(lambda () (push (deref-exp exp) answers)))
    (if (and (not (null answers))
	     (unify result (nreverse answers)))
	(funcall cont))))

(def-global-prolog-functor setof/3 (exp goal result cont)
  "Find all unique solutions to GOAL, and for each solution,
  collect the value of EXP into the list RESULT."
  ;; Ex: Assume (p 1) (p 2) (p 3).  Then:
  ;;     (setof ?x (p ?x) ?l) ==> ?l = (1 2 3)
  (let ((answers nil))
    (call/1 goal #'(lambda ()
                     (push (deref-exp exp) answers)))
    (if (and (not (null answers))
             (unify result (delete-duplicates
                            answers
                            :test #'deref-equal)))
        (funcall cont))))

(def-global-prolog-functor show-prolog-vars/2 (var-names vars cont)
  (if (null vars)
      (format t "~&Yes")
      (loop for name in var-names
	 for var in vars do
	   (format t "~&~a = ~a" name (deref-exp var))))
  (if (continue-p)
      (funcall cont)
      (throw 'top-level-prove nil)))

(let ((graph-pkg (find-package :graph-db)))
#|
  (def-global-prolog-functor select/2 (var-names vars cont)
    (if (null vars)
	nil
	(push (loop for name in var-names
		 for var in vars
		 collect (let ((var (deref-exp var)))
			   (cond ((and (symbolp var)
				       (eq graph-pkg (symbol-package var)))
				  (symbol-name var))
				 ((and (consp var)
				       (eq (first var) name)
				       (symbolp (second var))
				       (eq graph-pkg
					   (symbol-package (second var))))
				  (list name (symbol-name (second var))))
				 (t var))))
	      *select-list*))
    (funcall cont))
|#

  (def-global-prolog-functor select/2 (var-names vars cont)
    (when *prolog-trace*
      (dbg "TRACE: SELECT/2(~A ~A)~%" var-names vars)
      (dbg "TRACE: SELECT/2 COUNT: ~A, SKIP: ~A" *select-current-count* *select-current-skip*)
      (dbg "TRACE: SELECT/2 SELECT-LIST: ~{~A~^ ~}" *select-list*))
    (when (or (null *select-limit*)
              (< *select-current-count* *select-limit*))
      (when (not (null vars))
        (let ((r (loop for name in var-names
                    for var in vars
                    collect (let ((var (deref-exp var)))
                              (cond ((and (symbolp var)
                                          (eq graph-pkg (symbol-package var)))
                                     (symbol-name var))
                                    ((and (consp var)
                                          (eq (first var) name)
                                          (symbolp (second var))
                                          (eq graph-pkg
                                              (symbol-package (second var))))
                                     (list name (symbol-name (second var))))
                                    (t var))))))
          (when (and *select-skip* (<= *select-current-skip* *select-skip*))
            (incf *select-current-skip*))
          (when (or (null *select-skip*)
                    (> *select-current-skip* *select-skip*))
            (incf *select-current-count*)
            (if *select-flat*
                (dolist (i r)
                  (push i *select-list*))
                (push r *select-list*))))))
    (if (or (null *select-limit*) (< *select-current-count* *select-limit*))
        (funcall cont)
        (throw :prolog-limit-reached nil)))

  (def-global-prolog-functor map-query/5
      (fn var-names vars collect-p remove-nulls-p cont)
    (when *prolog-trace*
      (format t "TRACE: MAP-QUERY/5 FN (~A) IS ~A~%COLLECT-P is ~A~%"
	      (type-of fn) fn collect-p))
    (if (null vars)
	nil
	(let ((new-vars
               (loop for name in var-names
                  for var in vars
                  collect (let ((var (deref-exp var)))
                            (cond ((and (symbolp var)
                                        (eq graph-pkg (symbol-package var)))
                                   (symbol-name var))
                                  ((and (consp var)
                                        (eq (first var) name)
                                        (symbolp (second var))
                                        (eq graph-pkg
                                            (symbol-package (second var))))
                                   (list name (symbol-name (second var))))
                                  (t var))))))
	  (let ((result (eval `(apply ,fn ',new-vars))))
	    (when collect-p
              (unless (and remove-nulls-p (null result))
                (push result *select-list*))))))
    (funcall cont)))

(def-global-prolog-functor invoke-view/5 (class-name view-name id key value cont)
  (when *prolog-trace*
    (format t "TRACE: INVOKE-VIEW/5(~A ~A ~A ~A ~A)~%" class-name view-name id key value))
  (setq value (var-deref value)
        class-name (var-deref class-name)
        view-name (var-deref view-name)
        id (var-deref id)
        key (var-deref key))
  (map-view (lambda (k i v)
              (let ((old-trail (fill-pointer *trail*)))
                (when (and (unify key k)
                           (unify id i)
                           (unify value v))
                  (funcall cont))
                (undo-bindings old-trail)))
            class-name view-name :key (if (not (var-p key)) key)))

(def-global-prolog-functor invoke-reduced-view/4 (class-name view-name key value cont)
  (when *prolog-trace*
    (format t "TRACE: INVOKE-REDUCED-VIEW/4(~A ~A ~A ~A)~%" class-name view-name key value))
  (setq value (var-deref value)
        class-name (var-deref class-name)
        view-name (var-deref view-name)
        key (var-deref key))
  (if (not (var-p key))
      (let ((alist (invoke-graph-view class-name view-name :key key :group-p t)))
        (when (and alist
                   (unify key (cdr (assoc :key alist)))
                   (unify value (cdr (assoc :value alist))))
          (funcall cont)))
      (map-reduced-view (lambda (k i v)
                          (declare (ignore i))
                          (let ((old-trail (fill-pointer *trail*)))
                            (when (and (unify key k)
                                       (unify value v))
                              (funcall cont))
                            (undo-bindings old-trail)))
                        class-name view-name)))

(def-global-prolog-functor outgoing-edges/2 (vertex var cont)
  (setq vertex (var-deref vertex)
        var (var-deref var))
  (when *prolog-trace*
    (format t "TRACE: outgoing-edges/2(~S ~S)~%" vertex var))
  (map-edges (lambda (edge)
              (let ((old-trail (fill-pointer *trail*)))
                (when (unify var edge)
                  (funcall cont))
                (undo-bindings old-trail)))
             *graph* :vertex vertex :direction :out))

(def-global-prolog-functor outgoing-edges/3 (vertex edge-type var cont)
  (setq vertex (var-deref vertex)
        edge-type (var-deref edge-type)
        var (var-deref var))
  (when *prolog-trace*
    (format t "TRACE: outgoing-edges/3(~S ~S ~S)~%" vertex edge-type var))
  (map-edges (lambda (edge)
              (let ((old-trail (fill-pointer *trail*)))
                (when (unify var edge)
                  (funcall cont))
                (undo-bindings old-trail)))
             *graph* :vertex vertex :direction :out :edge-type edge-type))

(def-global-prolog-functor incoming-edges/2 (vertex var cont)
  (setq vertex (var-deref vertex)
        var (var-deref var))
  (when *prolog-trace*
    (format t "TRACE: incoming-edges/2(~S ~S)~%" vertex var))
  (map-edges (lambda (edge)
              (let ((old-trail (fill-pointer *trail*)))
                (when (unify var edge)
                  (funcall cont))
                (undo-bindings old-trail)))
             *graph* :vertex vertex :direction :in))

(def-global-prolog-functor incoming-edges/3 (vertex edge-type var cont)
  (setq vertex (var-deref vertex)
        edge-type (var-deref edge-type)
        var (var-deref var))
  (when *prolog-trace*
    (format t "TRACE: incoming-edges/3(~S ~S ~S)~%" vertex edge-type var))
  (map-edges (lambda (edge)
               (let ((old-trail (fill-pointer *trail*)))
                 (when (unify var edge)
                   (funcall cont))
                 (undo-bindings old-trail)))
             *graph* :vertex vertex :direction :in :edge-type edge-type))

(def-global-prolog-functor incoming-edges/4 (vertex edge-type edge-var vertex-var
                                                    cont)
  (setq vertex (var-deref vertex)
        edge-type (var-deref edge-type)
        edge-var (var-deref edge-var)
        vertex-var (var-deref vertex-var))
  (when *prolog-trace*
    (format t "TRACE: incoming-edges/4(~S ~S ~S ~S)~%"
            vertex edge-type edge-var vertex-var))
    (map-edges (lambda (edge)
                 (let ((old-trail (fill-pointer *trail*)))
                   (when (unify edge-var edge)
                     (let ((v2 (lookup-vertex (from edge))))
                       (when (unify vertex-var v2)
                         (funcall cont))))
                   (undo-bindings old-trail)))
               *graph* :vertex vertex :direction :in :edge-type edge-type))

(def-global-prolog-functor outgoing-edges/4 (from edge-type edge-var to cont)
  (setq from (var-deref from)
        edge-type (var-deref edge-type)
        edge-var (var-deref edge-var)
        to (var-deref to))
  (when *prolog-trace*
    (format t "TRACE: outgoing-edges/4(~S ~S ~S ~S)~%"
            from edge-type edge-var to))
  (cond ((and (vertex-p from) (vertex-p to) (symbolp edge-type))
         (map-edges (lambda (edge)
                      (let ((old-trail (fill-pointer *trail*)))
                        (when (unify edge-var edge)
                          (funcall cont))
                        (undo-bindings old-trail)))
                    *graph*
                    :from-vertex from
                    :to-vertex to
                    :edge-type edge-type))
        ((and (vertex-p from) (vertex-p to))
         (map-edges (lambda (edge)
                      (let ((old-trail (fill-pointer *trail*)))
                        (when (unify edge-var edge)
                          (funcall cont))
                        (undo-bindings old-trail)))
                    *graph*
                    :from-vertex from
                    :to-vertex to))
        ((and (vertex-p from) (symbolp edge-type))
         (map-edges (lambda (edge)
                      (let ((old-trail (fill-pointer *trail*)))
                        (when (unify edge-var edge)
                          (let ((v2 (lookup-vertex (to edge))))
                            (when (unify to v2)
                              (funcall cont))))
                        (undo-bindings old-trail)))
                    *graph* :vertex from :direction :out :edge-type edge-type))
        ((and (vertex-p to) (symbolp edge-type))
         (map-edges (lambda (edge)
                      (let ((old-trail (fill-pointer *trail*)))
                        (when (unify edge-var edge)
                          (let ((v2 (lookup-vertex (from edge))))
                            (when (unify from v2)
                              (funcall cont))))
                        (undo-bindings old-trail)))
                    *graph* :vertex to :direction :in :edge-type edge-type))
        ((symbolp edge-type)
         (map-edges (lambda (edge)
                      (let ((old-trail (fill-pointer *trail*)))
                        (when (unify edge-var edge)
                          (let ((v1 (lookup-vertex (from edge)))
                                (v2 (lookup-vertex (to edge))))
                            (when (and (unify from v1) (unify to v2))
                              (funcall cont))))
                        (undo-bindings old-trail)))
                    *graph* :vertex from :direction :out :edge-type edge-type))))

(def-global-prolog-functor node-slot-value/3 (node slot var cont)
  (setq node (var-deref node)
        slot (var-deref slot)
        var (var-deref var))
  (when (unify var (node-slot-value node slot))
    (funcall cont)))

(def-global-prolog-functor weight/2 (edge var cont)
  (setq edge (var-deref edge)
        var (var-deref var))
  (when (and (typep edge 'edge)
             (unify var (weight edge)))
    (funcall cont)))

(def-global-prolog-functor unique/1 (node cont)
  (let ((var-name (var-name node)))
    (let ((table (or (gethash var-name *seen-table*)
                     (setf (gethash var-name *seen-table*)
                           (make-node-table)))))
      (setq node (var-deref node))
      (when (or (not (node-p node))
                (null (gethash node table)))
        (when (node-p node)
          (setf (gethash node table) node))
        (funcall cont)))))

(def-global-prolog-functor is-a/2 (node type cont)
  (setq node (var-deref node)
        type (var-deref type))
  (cond ((and (vertex-p node) (symbolp type))
         (when (typep node type)
           (funcall cont)))
        ((vertex-p node)
         (when (unify type (type-of node))
           (funcall cont)))
        ((and (symbolp type) (lookup-node-type-by-name type :vertex))
         (map-vertices
          (lambda (vertex)
            (let ((old-trail (fill-pointer *trail*)))
              (when (unify node vertex)
                (funcall cont))
              (undo-bindings old-trail)))
          *graph* :vertex-type type))
        (t
         (map-vertices
          (lambda (vertex)
            (let ((old-trail (fill-pointer *trail*)))
              (when (unify node vertex)
                (when (unify type (type-of vertex))
                  (funcall cont)))
              (undo-bindings old-trail)))
          *graph*))))

(def-global-prolog-functor retract/1 (node cont)
  (setq node (var-deref node))
  (when (node-p node)
    (mark-deleted node)
    (funcall cont)))

(def-global-prolog-functor retract/3 (edge-type from to cont)
  (setq edge-type (var-deref edge-type)
        from (var-deref from)
        to (var-deref to))
  (cond ((and (symbolp edge-type) (vertex-p from) (vertex-p to))
         ;; use vev-index and delete
         (map-edges (lambda (edge)
                      (let ((old-trail (fill-pointer *trail*)))
                        (dbg "1: retracting ~A" edge)
                        (mark-deleted edge)
                        (funcall cont)
                        (undo-bindings old-trail)))
                    *graph*
                    :edge-type edge-type
                    :from-vertex from
                    :to-vertex to))
        ((and (symbolp edge-type) (vertex-p from))
         ;; use ve-out-index and delete
         (map-edges (lambda (edge)
                      (let ((old-trail (fill-pointer *trail*)))
                        (when (unify to (lookup-vertex (to edge)))
                          (dbg "2: retracting ~A" edge)
                          (mark-deleted edge)
                          (funcall cont))
                        (undo-bindings old-trail)))
                    *graph*
                    :edge-type edge-type
                    :vertex from
                    :direction :out))
        ((and (symbolp edge-type) (vertex-p to))
         ;; use ve-in-index and delete
         (map-edges (lambda (edge)
                      (let ((old-trail (fill-pointer *trail*)))
                        (when (unify from (lookup-vertex (from edge)))
                          (dbg "3: retracting ~A" edge)
                          (mark-deleted edge)
                          (funcall cont))
                        (undo-bindings old-trail)))
                    *graph*
                    :edge-type edge-type
                    :vertex to
                    :direction :in))
        (t
         (error 'prolog-error
                :reason
                (format nil "retract/3: cowardly refusing to retract(~A ~A ~A)"
                        edge-type from to)))))





