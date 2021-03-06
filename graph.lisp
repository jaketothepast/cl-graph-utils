;;; Graph Library in Common Lisp
;;; AUTHOR: Jake Windle
;;; Provided free of charge, free of licenses, take at will.

(ql:quickload "cl-heap")

(defun node-adjacency-list (node list-edges)
  "Given a list of edges get the adjacency list for a node"
  (mapcar #'second (remove-if-not #'(lambda (x)
                                      (equalp node (first x)))
                                  list-edges)))

(defun is-connected (nodeA nodeB graph)
  "Check if nodeA and nodeB are connected in the given graph"
  (let* ((node-list (mapcar #'first graph))
         (nodeA (position nodeA node-list)))
    (if (not (null nodeA))
        (return-from is-connected
          (equalp nodeB
                  (find nodeB
                        (second
                         (nth nodeA graph)))))
        )
    )
  )

(defun make-graph-adjacency-list (list-nodes list-edges &optional undirected)
  "Make an adjacency list representation given V and E"
  ;; The adjacency list looks like this
  ;; ((Node (Node2 Node3 Node4)) ... )
  ;;
  ;; NOTE - Currently only makes directed graphs.
  (let ((tbl (make-hash-table :test #'equalp)))
    (dolist (node list-nodes)
      (setf
       (gethash node tbl)
       (node-adjacency-list node list-edges))
      )
    tbl
    )
  )

(defun valid-graph-p (list-nodes list-edges)
  "Predicate for testing if the graph specified is valid"
  ;; For every edge check that the edge is valid
  (dolist (x list-edges)
    (if (or
         (null
          ;; The intersection of the list of nodes and the edge should NOT
          ;; be null at all
          (intersection list-nodes x))
         ;; The count of nodes in any given edge NOT in the node list
         ;; should never be more than 0
         (not
          (zerop
           (count-if #'(lambda (y) (null (member y list-nodes))) x))))
        ;; Use return because we want to hop all the way out of calling scope
        (return nil)))
  t
  )


(defun breadth-first-search (graph qnode)
  "Perform a breadth first search on the graph"
  (let ((node-queue (make-instance 'cl-heap:priority-queue))
        (explored-nodes nil))
    (cl-heap:enqueue node-queue (first (first graph)) 1)
    )
  )

(setf *new-graph* (make-graph-adjacency-list '(A B C) '((A B) (B A) (A C))))
