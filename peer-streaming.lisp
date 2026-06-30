(in-package :graph-db)

;;; ===========================================================================
;;; Peer replication transport (WP-4 / WP-6 / WP-8).
;;;
;;; The network half of peer replication: a hub serves authority-scoped pulls to
;;; many devices; a device pulls its closed disclosable subgraph and applies it
;;; read-only.  Built ALONGSIDE -- never on top of -- the master/slave transport
;;; in transaction-streaming.lisp, which is left byte-for-byte as it is (design
;;; §1.3).  Reuses that file's framed-packet primitives verbatim
;;; (read-packet/write-packet, the 8-byte size framing, read/write-plist-packet,
;;; deserialize-tx-header-vector / deserialize-tx-write-vector).
;;;
;;; See docs/peer-replication-design.md (v2) and
;;; docs/peer-replication-branch-a-plan.md.  PT-n tags point at the §12
;;; pressure-test invariants.
;;;
;;; WIRE (peer-protocol v1).  Every entry is a self-size-framed packet; the type
;;; byte at offset 9 discriminates:
;;;   - peer-meta (#\M, 51 bytes, from transactions.lisp WP-0): origin-id, op-id,
;;;     lamport, op-class.  For op-class authored(0)/state-create(1) it is
;;;     followed by the ordinary tx packets ([tx-header][tx-write...]); the inner
;;;     tx machinery is reused unchanged.
;;;   - purge (#\U, below): a state-sync scope-exit -- a count then N*16-byte
;;;     node UUIDs (design §4/§7).
;;;   - plist (#\p): handshake, the :pull-end control marker, and the ack.
;;;
;;; OP CLASSES (design §4).  Branch A's pull ships only STATE-SYNC ops -- they
;;; assert current scope membership, are keyed by node UUID, are idempotent, are
;;; never deduped by op-id and never propagate back:
;;;   - state-create: a node currently in scope, shipped as a full create
;;;     (idempotent upsert by UUID -- so a re-pull simply refreshes it);
;;;   - purge: a node that LEFT scope, removed from the device (it still lives on
;;;     the hub).
;;; AUTHORED ops (op-id-deduped, cursor-advancing) are handled on the device
;;; apply side for forward-compat, but the cursor-resumed authored-op STREAM that
;;; emits them from the hub feed is a later increment (see PEER-PULL-PHASE);
;;; ongoing changes still converge here via membership reconciliation on each
;;; connection.
;;; ===========================================================================

(defvar *peer-protocol-version* 1
  "Peer wire protocol version, independent of *REPLICATION-PROTOCOL-VERSION*.")

;;; ---------------------------------------------------------------------------
;;; Small encodings: ids on the plist channel, and the purge packet.
;;; ---------------------------------------------------------------------------

(defun peer-id->hex (id)
  "A 16-byte node/origin id as a 32-char lowercase hex string (plist-safe)."
  (string-id id))

(defun peer-hex->id (hex)
  "Inverse of PEER-ID->HEX: a 32-char hex string back to a 16-byte id vector."
  (let ((v (make-byte-vector 16)))
    (dotimes (i 16 v)
      (setf (aref v i)
            (parse-integer hex :start (* 2 i) :end (+ 2 (* 2 i)) :radix 16)))))

(defun peer-ids->string (ids)
  "Encode a list of 16-byte ids as one 32-chars-per-id hex string.  The plist
control channel only carries scalars (a list value trips PLIST-TOO-FANCY-ERROR),
so id lists ride as a single concatenated string."
  (apply #'concatenate 'string (mapcar #'peer-id->hex ids)))

(defun peer-string->ids (string)
  "Inverse of PEER-IDS->STRING: split a concatenated 32-hex-per-id STRING into a
list of 16-byte ids."
  (loop for start from 0 below (length string) by 32
        collect (peer-hex->id (subseq string start (+ start 32)))))

(defun peer-portable-string (value)
  "Coerce a STRING VALUE to a general (CHARACTER) string; pass non-strings through.
The plist control channel serializes with *PRINT-READABLY* T (via
WITH-STANDARD-IO-SYNTAX), under which SBCL prints a BASE-STRING -- e.g.
(SYMBOL-NAME (GRAPH-NAME g)) -- as a specialized-array literal
#A((n) BASE-CHAR . \"...\"), which ECL's reader cannot parse (it CARs the
contents).  A (SIMPLE-ARRAY CHARACTER (*)) prints as a plain \"...\" on both, so
coercing makes the handshake/auth/ack plists cross SBCL<->ECL."
  (if (stringp value)
      (coerce value '(simple-array character (*)))
      value))

(defun peer-write-plist (plist socket)
  "WRITE-PLIST-PACKET with every string value coerced portable (PEER-PORTABLE-STRING),
so peer handshake/auth/ack plists survive a cross-implementation hop.  Keys are
keywords and pass through untouched."
  (write-plist-packet
   (loop for (k v) on plist by #'cddr collect k collect (peer-portable-string v))
   socket))

(alexandria:define-constant +peer-purge-type-code+ (char-code #\U))

(defun serialize-purge-packet (ids)
  "Build a self-framed purge packet for IDS (a list of 16-byte node UUIDs):
size(8) flags(1) type(1=#\\U) count(8) then count*16 raw uuid bytes."
  (let* ((n (length ids))
         (size (+ 8 1 1 8 (* 16 n)))
         (v (make-byte-vector size))
         (i 0))
    (serialize-uint64 v size i) (incf i 8)
    (setf (aref v i) 0) (incf i)                      ; flags (unused)
    (setf (aref v i) +peer-purge-type-code+) (incf i) ; type
    (serialize-uint64 v n i) (incf i 8)
    (dolist (id ids v)
      (replace v id :start1 i :end2 16)
      (incf i 16))))

(defun deserialize-purge-packet (packet)
  "Parse a purge packet, returning the list of 16-byte node UUIDs."
  (assert (= (aref packet 9) +peer-purge-type-code+))
  (let ((n (deserialize-uint64 packet 10))
        (i 18)
        (ids '()))
    (dotimes (k n (nreverse ids))
      (push (subseq packet i (+ i 16)) ids)
      (incf i 16))))

;;; ---------------------------------------------------------------------------
;;; Schema-version compatibility (WP-6 / PT-6 / design §14).
;;; The gate is SAME-MAJOR, not digest-equality: an additive (minor) drift still
;;; syncs degraded-safe; only a major mismatch rejects the pull.  schema-digest
;;; is still exchanged as a within-major integrity signal.
;;; ---------------------------------------------------------------------------

(define-condition peer-schema-incompatible-error (error)
  ((hub-version    :initarg :hub-version    :reader peer-schema-hub-version)
   (device-version :initarg :device-version :reader peer-schema-device-version))
  (:report (lambda (c s)
             (format s "Peer schema majors differ: hub ~S, device ~S"
                     (peer-schema-hub-version c) (peer-schema-device-version c)))))

(defun peer-schema-major (version) (first version))

(defun peer-schema-compatible-p (a b)
  "True iff schema versions A and B share a MAJOR (additive minor drift is fine)."
  (eql (peer-schema-major a) (peer-schema-major b)))

;;; ---------------------------------------------------------------------------
;;; Device registry (hub side).  Engine-side, app-populated entry holding the
;;; per-device clearance scope + cursors + manifest.  The app owns the policy
;;; (who maps to what scope); the engine only reads these to drive a pull.
;;; ---------------------------------------------------------------------------

(defstruct (peer-device (:constructor %make-peer-device))
  (origin-id nil)              ; 16-byte device origin UUID (registry key)
  (roots '())                  ; scope roots: list of vertices/ids for SCOPE-NODE-SET
  (scope nil)                  ; opaque device-scope handed to DISCLOSABLE-P
  (edge-types nil)             ; optional edge-type bound for the closure walk
  (manifest '())               ; node-ids the device is KNOWN to hold (PT-4)
  (pull-cursor 0)              ; highest hub authored-feed seq the device applied
  (push-ack 0)                 ; highest device-seq the hub acknowledged (Branch B)
  (apply-cursor 0)             ; highest device-seq the hub applied (Branch B)
  (last-pushed-schema-version nil) ; drain-and-update barrier signal (§14)
  (lock (make-recursive-lock "peer-device")))

(defun ensure-device-registry (hub)
  "Return HUB's device registry, creating an equalp hash-table if unset."
  (or (device-registry hub)
      (setf (device-registry hub) (make-hash-table :test 'equalp))))

(defun find-peer-device (hub origin-id)
  "The PEER-DEVICE registered under ORIGIN-ID (a 16-byte id), or NIL."
  (let ((reg (device-registry hub)))
    (and reg (gethash origin-id reg))))

(defun register-peer-device (hub &key origin-id roots scope edge-types
                                   (manifest '()))
  "Register (or replace) a device in HUB's registry.  The app calls this to grant
ORIGIN-ID a clearance SCOPE rooted at ROOTS; the engine consumes it on pull.
Returns the PEER-DEVICE."
  (let ((reg (ensure-device-registry hub))
        (dev (%make-peer-device :origin-id origin-id :roots roots :scope scope
                                :edge-types edge-types :manifest manifest)))
    (setf (gethash origin-id reg) dev)
    dev))

;;; ---------------------------------------------------------------------------
;;; Hub: send a node as a state-sync create, and send a purge op.
;;; ---------------------------------------------------------------------------

(defun write-node-as-create (socket graph node)
  "Ship NODE to a device as a state-sync membership-create: a peer-meta packet
(op-class=state-create, op-id=node-UUID since state-sync is UUID-keyed, lamport
slot carries the node's commit epoch) followed by a single-write create tx.  The
node's data is materialized + serialized so the wire copy is self-contained."
  (maybe-init-node-data node :graph graph)
  (setf (bytes node) (serialize (data node)))
  (let* ((origin (or (origin-id graph) +peer-null-origin+))
         (epoch (or (commit-epoch node) 0))
         (write (make-instance 'tx-create :node node))
         (txh (make-instance 'tx-header :transaction-id epoch
                                        :writes (list write) :graph graph)))
    (write-packet (serialize-peer-meta origin (id node) epoch +peer-op-state-create+)
                  socket)
    (write-packet (tx-header-vector txh epoch) socket)
    (write-packet (tx-write-vector write) socket)))

(defun write-purge-op (socket graph ids)
  "Ship a state-sync PURGE for IDS as a single self-describing purge packet (#\\U).
A purge needs no peer-meta envelope: it is node-UUID-keyed state-sync (design §4)
-- no origin/op-id/lamport applies -- and the distinct packet type already routes
it on the device side."
  (declare (ignore graph))
  (write-packet (serialize-purge-packet ids) socket))

;;; ---------------------------------------------------------------------------
;;; Hub: handshake + authority-scoped pull.
;;; ---------------------------------------------------------------------------

(defun peer-hub-handshake-plist (graph)
  "What the hub announces on a new connection (schema-version split into plist-
safe integers; schema-digest carried as a within-major integrity signal)."
  (list :peer-protocol-version *peer-protocol-version*
        :name (symbol-name (graph-name graph))
        :schema-major (first (peer-schema-version graph))
        :schema-minor (second (peer-schema-version graph))
        :schema-digest (schema-digest (schema graph))))

(defun peer-authenticate-device (graph auth)
  "Validate a device AUTH plist against GRAPH (the hub): replication-key, the
same-major schema gate (WP-6/PT-6), and a known origin in the device registry.
Returns the PEER-DEVICE, or signals.  Records the device's last-pushed schema
version for the drain-and-update barrier signal (§14)."
  (unless (equal (getf auth :replication-key) (replication-key graph))
    (error 'invalid-auth-data-error))
  (let ((hub-version (peer-schema-version graph))
        (dev-version (list (getf auth :schema-major) (getf auth :schema-minor))))
    (unless (peer-schema-compatible-p hub-version dev-version)
      (error 'peer-schema-incompatible-error
             :hub-version hub-version :device-version dev-version))
    (let* ((origin (peer-hex->id (getf auth :origin-id)))
           (device (find-peer-device graph origin)))
      (unless device
        (error 'invalid-auth-data-error))
      (setf (peer-device-last-pushed-schema-version device) dev-version)
      device)))

(defun peer-pull-phase (graph socket device)
  "Serve DEVICE one authority-scoped pull over SOCKET.  Under one read snapshot,
compute the device's closed disclosable subgraph (SCOPE-NODE-SET, WP-5) and ship
every in-scope node as a state-create (vertices BEFORE edges, so the device holds
both endpoints before the edge that indexes on them -- the closed rule), then a
purge for every manifest id no longer in scope (design §7).  Re-shipping the full
in-scope set is the design-blessed every-connection full-diff v1 (design §13): it
carries creates AND updates idempotently; the cursor-resumed authored-op stream
(volume optimization) lands later.  PT-4: the per-device manifest advances ONLY
to what the device acks -- so a dropped purge re-emits (fail-closed disclosure)."
  (with-read-snapshot (graph)
    (multiple-value-bind (vset eset)
        (scope-node-set graph (peer-device-roots device) (peer-device-scope device)
                        :edge-types (peer-device-edge-types device))
      (let ((scope-ids (make-id-table)))
        (flet ((mark (id node) (declare (ignore node)) (setf (gethash id scope-ids) t)))
          (maphash #'mark vset)
          (maphash #'mark eset))
        (maphash (lambda (id node) (declare (ignore id))
                   (write-node-as-create socket graph node))
                 vset)
        (maphash (lambda (id node) (declare (ignore id))
                   (write-node-as-create socket graph node))
                 eset)
        (let ((purges (loop for id in (peer-device-manifest device)
                            unless (gethash id scope-ids) collect id)))
          (when purges
            (write-purge-op socket graph purges))))))
  (peer-write-plist (list :peer-control :pull-end) socket)
  ;; Advance the manifest to exactly what the device ACKS holding (PT-4): never
  ;; to hub intent.  Over-emitting a purge next time is harmless (idempotent);
  ;; under-emitting would strand undisclosed data, so we only ever trust the ack.
  (let ((ack (read-plist-packet socket)))
    (with-recursive-lock-held ((peer-device-lock device))
      (setf (peer-device-manifest device)
            (peer-string->ids (or (getf ack :manifest-acked) "")))
      (when (getf ack :pull-cursor)
        (setf (peer-device-pull-cursor device) (getf ack :pull-cursor))))
    ack))

(defun make-peer-session-handler (graph socket)
  "Return a thunk handling one device connection on SOCKET (hub side): announce,
authenticate, serve one pull, close.  Models MAKE-SLAVE-SESSION-HANDLER but stays
distinct from it."
  (lambda ()
    (let ((*graph* graph))
      (handler-case
          (unwind-protect
               (progn
                 (peer-write-plist (peer-hub-handshake-plist graph) socket)
                 (let ((device (peer-authenticate-device graph (read-plist-packet socket))))
                   (peer-write-plist (list :peer-control :auth-ok) socket)
                   (peer-pull-phase graph socket device)))
            (ignore-errors (usocket:socket-close socket)))
        (error (c)
          (log:error "peer hub session error: ~A" c))))))

(defun peer-server-accept-loop (graph)
  "Hub accept loop: bind REPLICATION-PORT and hand each device connection to its
own session thread.  Mirrors SERVER-ACCEPT-LOOP (incl. the binary :element-type
on the listener so accepted sockets are binary on CCL)."
  (let ((port (replication-port graph))
        (address usocket:*wildcard-host*))
    (usocket:with-socket-listener (listener address port :reuse-address t
                                            :element-type '(unsigned-byte 8))
      (loop
        (when (stop-replication-p graph)
          (return))
        (when (usocket:wait-for-input listener :timeout 1 :ready-only t)
          (let ((socket (usocket:socket-accept listener
                                               :element-type '(unsigned-byte 8))))
            (log:debug "Peer hub got a connection: ~A" socket)
            (bordeaux-threads:make-thread
             (make-peer-session-handler graph socket)
             :name "peer hub session")))))))

;;; ---------------------------------------------------------------------------
;;; Device: the single-writer apply funnel (WP-8 / PT-5).
;;;
;;; The connection thread DECODES + ENQUEUES received ops; one writer thread
;;; drains the mailbox and performs every graph mutation.  A single writer means
;;; the device never contends on OCC (correct by construction on ECL, PT-5).
;;; ---------------------------------------------------------------------------

(defstruct (peer-op (:constructor make-peer-op))
  kind        ; :state-create :authored :purge :barrier :shutdown
  op-id lamport tx-id
  writes      ; list of tx-write (state-create / authored)
  ids         ; list of node-ids (purge)
  reply)      ; reply mailbox (barrier)

(defun apply-peer-create-writes (graph tx-id writes)
  "Apply state-sync create WRITES on the device with no cursor advance (state-sync
is out of the authored-feed band -- it must not move the pull cursor).  Idempotent
upsert by node UUID: *ADD-TO-INDEXES-UNLESS-PRESENT-P* makes a refresh re-pull
safe.  TX-ID is the shipped node's hub commit epoch, used as the MVCC stamp."
  (let ((*commit-epoch* tx-id)
        (*add-to-indexes-unless-present-p* t))
    (apply-tx-writes writes graph)
    (apply-tx-writes-to-views writes graph)
    (apply-tx-writes-to-spatial-index writes graph)
    (reap-old-versions writes graph)))

(defun apply-peer-authored-op (graph op)
  "Apply an AUTHORED op on the device: dedup by op-id (PT-1), apply, advance the
pull cursor, then record the op-id.  Returns T if applied, NIL if a duplicate.

NB (PT-3): full crash-atomicity of the op-id-index update with the apply lands
with the durable authored-op .txn path in the cursor-resumed-stream increment;
here state-sync (the only thing Branch A's pull emits) is UUID-idempotent and
needs no op-id dedup, so this path is exercised only once the hub emits authored
ops."
  (if (op-applied-p graph (peer-op-op-id op))
      nil
      (let ((*commit-epoch* (peer-op-tx-id op))
            (*add-to-indexes-unless-present-p* t)
            (writes (peer-op-writes op)))
        (apply-tx-writes writes graph)
        (apply-tx-writes-to-views writes graph)
        (apply-tx-writes-to-spatial-index writes graph)
        (reap-old-versions writes graph)
        (persist-highest-transaction-id (peer-op-tx-id op) graph)
        (record-applied-op graph (peer-op-op-id op) (peer-op-lamport op))
        t)))

(defun peer-purge-node (graph node)
  "Hard-remove NODE from the device: drop it from every index/view (and the
spatial index for a geometry vertex), then from its lhash table and the cache.
Unlike a tombstone this leaves NO trace -- a captured device must not even reveal
the existence/id of purged (undisclosed) work (design §7)."
  (etypecase node
    (edge
     (remove-from-ve-index node graph)
     (remove-from-vev-index node graph)
     (remove-from-type-index node graph)
     (remove-from-views graph node)
     (lhash-remove (edge-table graph) (id node)))
    (vertex
     (let ((idx (spatial-index graph)))
       (when idx
         (let ((geom (node-geometry node)))
           (when geom (spatial-index-remove idx (id node) geom)))))
     (remove-from-type-index node graph)
     (remove-from-views graph node)
     (lhash-remove (vertex-table graph) (id node))))
  (remhash (id node) (cache graph)))

(defun apply-peer-purge (graph ids)
  "Purge IDS from the device, edges before vertices (so an edge is gone before the
endpoint it indexes on).  Missing ids are skipped (idempotent)."
  (let ((nodes (loop for id in ids
                     for node = (or (lookup-edge id :graph graph)
                                    (lookup-vertex id :graph graph))
                     when node collect node)))
    (dolist (n nodes) (when (typep n 'edge)   (peer-purge-node graph n)))
    (dolist (n nodes) (when (typep n 'vertex) (peer-purge-node graph n)))))

(defun peer-writer-loop (graph)
  "The device's single writer (WP-8): drain the mailbox and apply each op.  All
device graph mutations funnel through here, so OCC never contends (PT-5).
Accumulates the ids it durably holds/removed for the pull ack; a :barrier op
reports + resets that accumulator, a :shutdown op ends the thread."
  (let ((*graph* graph)
        (mailbox (peer-writer-mailbox graph))
        (applied '())
        (purged '()))
    (loop
      (let ((op (receive-message mailbox :timeout 1)))
        (when op
          (ecase (peer-op-kind op)
            (:state-create
             (apply-peer-create-writes graph (peer-op-tx-id op) (peer-op-writes op))
             (dolist (w (peer-op-writes op))
               (pushnew (id (node w)) applied :test #'equalp)))
            (:authored
             (when (apply-peer-authored-op graph op)
               (dolist (w (peer-op-writes op))
                 (pushnew (id (node w)) applied :test #'equalp))))
            (:purge
             (apply-peer-purge graph (peer-op-ids op))
             (dolist (pid (peer-op-ids op))
               (push pid purged)
               (setf applied (remove pid applied :test #'equalp))))
            (:barrier
             (send-message (peer-op-reply op)
                           (list :applied (copy-list applied)
                                 :purged (copy-list purged)))
             (setf applied '() purged '()))
            (:shutdown
             (return))))))))

;;; ---------------------------------------------------------------------------
;;; Device: receive a pull and ack it.
;;; ---------------------------------------------------------------------------

(defun peer-read-and-enqueue (socket graph mailbox)
  "Read ONE framed packet from SOCKET and enqueue the decoded op to MAILBOX (the
single writer).  Returns :PULL-END on the hub's end-of-pull control packet, else
T.  Deserialization (which resolves node type-ids against the schema, so it needs
*GRAPH*) happens here on the receive side; the actual graph mutation happens on
the writer (WP-8)."
  (let* ((*graph* graph)
         (packet (read-packet socket))
         (type (aref packet 9)))
    (cond
      ((= type +peer-meta-type-code+)
       (multiple-value-bind (origin op-id lamport op-class)
           (deserialize-peer-meta packet)
         (declare (ignore origin))
         (let* ((txh (deserialize-tx-header-vector (read-packet socket)))
                (writes (loop repeat (write-count txh)
                              collect (deserialize-tx-write-vector (read-packet socket)))))
           (send-message
            mailbox
            (make-peer-op :kind (if (= op-class +peer-op-authored+) :authored :state-create)
                          :op-id op-id :lamport lamport
                          :tx-id (transaction-id txh) :writes writes))
           t)))
      ((= type +peer-purge-type-code+)
       (send-message mailbox (make-peer-op :kind :purge
                                           :ids (deserialize-purge-packet packet)))
       t)
      ((= type +plist-packet-type-code+)
       (if (eq (getf (deserialize-plist-packet packet) :peer-control) :pull-end)
           :pull-end
           t))
      (t
       (error "Unknown peer packet type ~A" type)))))

(defun peer-device-auth-plist (graph)
  "What a device sends to authenticate + drive a pull (cursors + same-major
schema gate inputs)."
  (list :origin-id (peer-id->hex (origin-id graph))
        :pull-cursor (load-highest-transaction-id graph)
        :push-ack 0
        :schema-major (first (peer-schema-version graph))
        :schema-minor (second (peer-schema-version graph))
        :replication-key (replication-key graph)))

(defun peer-sync (graph &key (attempts 10))
  "Device entry point: connect to the hub, handshake (same-major schema gate,
WP-6/PT-6), receive one authority-scoped pull, apply it through the single writer
(WP-8), and ACK exactly what the device now holds (PT-4).  One connection, one
pull -- a resync is just another call.  Returns the ack result plist
(:applied / :purged id lists)."
  (let ((*graph* graph)
        (socket nil))
    (unwind-protect
         (progn
           (setf socket (connect-slave-to-master (peer-host graph)
                                                 (replication-port graph)
                                                 :attempts attempts))
           (let ((hs (read-plist-packet socket)))
             (unless (eql (getf hs :peer-protocol-version) *peer-protocol-version*)
               (error "peer protocol version mismatch: hub ~S, device ~S"
                      (getf hs :peer-protocol-version) *peer-protocol-version*))
             (let ((hub-version (list (getf hs :schema-major) (getf hs :schema-minor)))
                   (my-version (peer-schema-version graph)))
               (unless (peer-schema-compatible-p hub-version my-version)
                 (error 'peer-schema-incompatible-error
                        :hub-version hub-version :device-version my-version))))
           (peer-write-plist (peer-device-auth-plist graph) socket)
           (let ((ctrl (read-plist-packet socket)))
             (unless (eq (getf ctrl :peer-control) :auth-ok)
               (error "peer auth rejected by hub: ~S" ctrl)))
           (let ((mailbox (peer-writer-mailbox graph)))
             (loop until (eq (peer-read-and-enqueue socket graph mailbox) :pull-end))
             ;; Barrier: wait for the writer to drain, then ack what it durably
             ;; holds.  manifest-acked is the COMPLETE set the device now holds in
             ;; scope, so a dropped purge (the device still holds it) keeps it in
             ;; the hub manifest and re-purges next time -- fail-closed (PT-4).
             (let ((reply (make-mailbox)))
               (send-message mailbox (make-peer-op :kind :barrier :reply reply))
               (let ((result (receive-message reply :timeout 30)))
                 (peer-write-plist
                  (list :manifest-acked (peer-ids->string (getf result :applied))
                        :pull-cursor (load-highest-transaction-id graph))
                  socket)
                 result))))
      (when socket (ignore-errors (usocket:socket-close socket))))))

;;; ---------------------------------------------------------------------------
;;; Lifecycle: start/stop-replication specialized on peer-graph.
;;;
;;; MAKE-GRAPH / OPEN-GRAPH already call (start-replication graph) and
;;; CLOSE-GRAPH calls (stop-replication graph) for every graph, so specializing
;;; the existing generics on peer-graph wires the peer transport into the normal
;;; lifecycle with no changes to graph.lisp.  Master/slave methods are untouched.
;;; ---------------------------------------------------------------------------

(defmethod start-replication ((graph peer-graph) &key package)
  (declare (ignore package))
  (setf (stop-replication-p graph) nil)
  (ecase (peer-role graph)
    (:hub
     (setf (peer-thread graph)
           (bordeaux-threads:make-thread
            (lambda () (peer-server-accept-loop graph))
            :name (format nil "~A-peer-hub" (graph-name graph)))))
    (:device
     ;; Start the single-writer funnel; pulls are driven explicitly by PEER-SYNC
     ;; (a Branch A device is read-only and pulls on demand, not a live stream).
     (setf (peer-writer-mailbox graph) (make-mailbox))
     (setf (peer-writer-thread graph)
           (bordeaux-threads:make-thread
            (lambda () (peer-writer-loop graph))
            :name (format nil "~A-peer-writer" (graph-name graph)))))))

(defmethod stop-replication ((graph peer-graph))
  (setf (stop-replication-p graph) t)
  (ecase (peer-role graph)
    (:hub
     (let ((thread (peer-thread graph)))
       (when (and (threadp thread) (thread-alive-p thread))
         (join-thread thread))))
    (:device
     (let ((mailbox (peer-writer-mailbox graph)))
       (when mailbox
         (send-message mailbox (make-peer-op :kind :shutdown))))
     (let ((thread (peer-writer-thread graph)))
       (when (and (threadp thread) (thread-alive-p thread))
         (join-thread thread))))))
