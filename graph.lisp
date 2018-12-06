;;; Graph Library in Common Lisp
;;; AUTHOR: Jake Windle
;;; Provided free of charge, free of licenses, take at will.
(defpackage :cl-graph-utils
  (use :cl-user))

(in-package :cl-graph-utils)

(defun node-adjacency-list (node list-edges)
  "Given a list of edges get the adjacency list for a node"
  (list node
        (mapcar #'second (remove-if-not #'(lambda (x)
                                            (equalp node (first x)))
                                        list-edges)))
  )

(defun is-connected (nodeA nodeB graph)
  "Check if nodeA and nodeB are connected in the given graph"
  )

(defun make-graph-adjacency-list (list-nodes list-edges)
  "Make an adjacency list representation given V and E"
  ;;
  ;; The adjacency list looks like this
  ;; ((Node (Node2 Node3 Node4)) ... )
  (mapcar #'(lambda (x) (node-adjacency-list x list-edges)) list-nodes)
  )

(in-package :cl-user)
