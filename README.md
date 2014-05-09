vivace-graph-v3
===============

VivaceGraph version 3

This is another redesign of VivaceGraph;  this version uses memory mapped files
and has dropped the RDF semantics of previous versions.  VG is now more akin to 
neo4j, as it implements a property graph model.  The Prolog interface has been 
greatly expanded, though it still suffers from a few minor issues (all of which 
are more issues of interface convenience as opposed to functional deficiencies).
CLOS integration was a goal with this version and works quite well at this 
stage.  Multi-node transactions are next on the agenda.

Documentation is forthcoming;  please stay tuned.  But to get you started, here
are some examples:

```
(ql:quickload :graph-db)
(use-package :graph-db)
(defvar *graph-name* :test-graph)
(defvar *graph-path* "/var/tmp/test-graph/")
(setq *graph* (make-graph *graph-name* *graph-path*))

(def-vertex person ()
  ((name :type string)
   (email :type email))
 :test-graph)

(def-edge likes ()
 ()
 :test-graph)

(let ((p1 (make-person :name "Joe" :email "joe@blow.com"))
      (p2 (make-person :name "Jill" :email "jill@hill.com")))
  (make-likes :from p1 :to p2 :weight 100.0)
  (make-likes :from p2 :to p1 :weight 50.0))

(values
 (select (:flat nil)
         (?liker ?likee)
         (likes ?liker ?likee))

 (select (:flat nil)
         (?liker ?likee ?how-much)
         (likes ?liker ?likee)
         (outgoing-edges ?liker likes ?edge ?likee)
         (weight ?edge ?how-much))

 (let ((person (select-one (?person) (is-a ?person person))))
   (declare (special person))
   (destructuring-bind (sweetheart love-qty)
       (select (:flat t :limit 1 :skip 0)
               (?sweetheart ?love-qty)
               (lisp ?person person)
               (outgoing-edges ?person likes ?edge ?sweetheart)
               (weight ?edge ?love-qty))
     (format nil "~A likes ~A with a degree of ~F"
             (name person)
             (name sweetheart)
             love-qty)))
 )

```
