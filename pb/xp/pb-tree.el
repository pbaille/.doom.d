;;; pb/xp/pb-tree.el -*- lexical-binding: t; -*-

(defun pb-tree_node (value &rest children)
  (km :value value
      :children (km* children)))

(defmacro pb-tree (value &rest children)
  "Create a tree structure from nested node expressions.
BODY should be a node expression, which can contain nested node expressions."
  (let ((child-nodes
         (cl-loop for (key val) on children by #'cddr
                  when (and key val)
                  collect key
                  and collect (if (and (listp val) (eq (car val) 'node))
                                  `(pb-tree ,(cadr val) ,@(cddr val))
                                val))))
    `(pb-tree_node ,value ,@child-nodes)))

(pb_comment
 (macroexpand '(pb-tree "root node"
                        :child1 (node "child1"
                                      :grandchild-1-1 "foo"
                                      :grandchild-1-2 "bar")
                        :child2 (node "child2"
                                      :grandchild-2-1 "foo2"
                                      :grandchild-2-2 "bar2")))

 (pb-tree_node "root node"
               :child1 (pb-tree "child1" :grandchild-1-1 "foo" :grandchild-1-2 "bar")
               :child2 (pb-tree "child2" :grandchild-2-1 "foo2" :grandchild-2-2 "bar2"))

 ;; expands to this wich is not good
 '(pb-tree_node "root node" (:child1 (pb-tree "child1" :grandchild-1-1 "foo" :grandchild-1-2 "bar")) (:child2 (pb-tree "child2" :grandchild-2-1 "foo2" :grandchild-2-2 "bar2")))

 ;; should be
 '(pb-tree_node "root node"
   :child1 (pb-tree "child1" :grandchild-1-1 "foo" :grandchild-1-2 "bar")
   :child2 (pb-tree "child2" :grandchild-2-1 "foo2" :grandchild-2-2 "bar2")))
