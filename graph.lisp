;;; Graph Library in Common Lisp
;;; AUTHOR: Jake Windle
;;; Provided free of charge, free of licenses, take at will.

(defun node-adjacency-list (node list-edges)
  "Given a list of edges get the adjacency list for a node"
  (list node
        (mapcar #'second (remove-if-not #'(lambda (x)
                                            (equalp node (first x)))
                                        list-edges)))
  )

(defun is-connected (nodeA nodeB graph)
  "Check if nodeA and nodeB are connected in the given graph"
  (let ((node-list (mapcar #'first graph)))
    ;; TODO - Use the result of find to check if connected
    (find nodeA node-list)
    )
  )

(defun make-graph-adjacency-list (list-nodes list-edges)
  "Make an adjacency list representation given V and E"
  ;;
  ;; The adjacency list looks like this
  ;; ((Node (Node2 Node3 Node4)) ... )
  (mapcar #'(lambda (x)
              (node-adjacency-list x list-edges))
          list-nodes)
  )
