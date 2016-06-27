;;;; knot-a-joke.lisp

(in-package #:knot-a-joke)

;;; "knot-a-joke" goes here. Hacks and glory await!

;; OK, now I have s-exp representation graph.
;; What I really want is to:
;;   * extract singlet, doublet, triplet, etc subgraphs.
;;   * for a given node, extract its path-subgraph

(defun direct-hashes (s-exp-graph)
  (let ((res (make-hash-table :test #'equal)))
    (iter (for (nil num edges) in s-exp-graph)
	  (setf (gethash num res)
		(let ((sub-res (make-hash-table :test #'equal)))
		  (iter (for (src-part . dst-parts) in edges)
			(setf (gethash src-part sub-res) dst-parts))
		  sub-res)))
    ;; we add an empty partition (to make all subsequent algorithms more comfortable)
    (when (and (not (gethash 0 res))
	       (equal 1 (hash-table-count (gethash 1 res))))
      (setf (gethash 0 res)
	    (let ((it (make-hash-table :test #'equal)))
	      (iter (for (key nil) in-hashtable (gethash 1 res))
		    (setf (gethash '() it) `((,key 1))))
	      it)))
    res))

(defun reverse-hashes (direct-hashes)
  (let ((res (make-hash-table :test #'equal)))
    (iter (for (num edges) in-hashtable direct-hashes)
	  ;; (format t "~a ~a~%" num edges)
	  (let ((sub-res (or (gethash (1+ num) res)
			     (setf (gethash (1+ num) res)
				   (make-hash-table :test #'equal)))))
	    (iter (for (src dests) in-hashtable edges)
		  (iter (for (dest multiplicity) in dests)
			(push (list src multiplicity)
			      (gethash dest sub-res))))))
    res))

(defun hash->assoc (hash)
  (iter (for (key val) in-hashtable hash)
	(collect (cons key val))))

(defun find-partition (reverse-hashes partition &optional layer)
  (if layer
      (or (gethash partition (gethash layer reverse-hashes))
	  (error "Partition ~a is not found in layer ~a" partition layer))
      (let ((sum (apply #'+ partition)))
	(block find-layer
	  (iter (for (key val) in-hashtable reverse-hashes)
		(iter (for (key1 val1) in-hashtable val)
		      (if (not (equal sum (apply #'+ key1)))
			  (terminate))
		      (if (equal partition key1)
			  (return-from find-layer key))))
	  (error "Partition ~a is not found on any of the layers" partition)))))
			  

(defun backpropagate-subgraph (reverse-hashes partition)
  (let ((res (make-hash-table :test #'equal))
	(reverse-hashes reverse-hashes))
    (declare (special res reverse-hashes))
    (let ((layer (find-partition reverse-hashes partition)))
      (declare (special layer))
      (when (not (gethash layer res))
	(setf (gethash layer res)
	      (make-hash-table :test #'equal)))
      (backpropagate-subgraph-elt partition))
    res))

(defun backpropagate-subgraph-elt (partition)
  (declare (special res layer reverse-hashes))
  (if (not (null partition))
      (let ((partition-edges (find-partition reverse-hashes partition layer)))
	(setf (gethash partition (gethash layer res))
	      partition-edges)
	(let ((layer (1- layer)))
	  (declare (special layer))
	  (when (and (not (zerop layer))
		     (not (gethash layer res)))
	    (setf (gethash layer res)
		  (make-hash-table :test #'equal)))
	  (iter (for (edge-src mult) in partition-edges)
		(backpropagate-subgraph-elt edge-src))))))
      
