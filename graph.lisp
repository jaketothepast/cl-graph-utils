;;; Graph Library in Common Lisp
;;; AUTHOR: Jake Windle
;;; Provided free of charge, free of licenses, take at will.

(ql:quickload "cl-heap")

(defun node-adjacency-list (node list-edges)
  "Given a list of edges get the adjacency list for a node"
  (list node
        (mapcar #'second (remove-if-not #'(lambda (x)
                                            (equalp node (first x)))
                                        list-edges)))
  )

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
  (mapcar #'(lambda (x)
              (node-adjacency-list x list-edges))
          list-nodes)
  )

(defun valid-graph-p (list-nodes list-edges)
  "Predicate for testing if the graph specified is valid"
  )

(defun make-graph-adjacency-matrix (list-nodes list-edges)
  "Make a graph adjacency matrix"
  )

(defun breadth-first-search (graph qnode)
  "Perform a breadth first search on the graph"
  (let ((node-queue (make-instance 'cl-heap:priority-queue))
        (explored-nodes nil))
    (cl-heap:enqueue node-queue (first (first graph)) 1)
    )
  )

(setf *new-graph* (make-graph-adjacency-list '(A B) '((A B) (B A))))
