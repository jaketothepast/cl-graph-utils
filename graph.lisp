;;; Graph Library in Common Lisp
;;; AUTHOR: Jake Windle
;;; Provided free of charge, free of licenses, take at will.

(defun node-adjacency-list (node list-edges)
  "Given a list of edges get the adjacency list for a node"
  (list node
        (remove-if-not #'(lambda (x)
                           (equalp node (first x)))
                       list-edges))
  )

(defun make-graph (list-nodes list-edges)
  "For each edge in list-edges, add the tail to the node in list nodes
adjacency list"
  (mapcar #'(lambda (x) (node-adj-list x list-edges)) list-nodes)
  )
