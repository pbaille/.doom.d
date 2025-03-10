;;; pb/xp/pb-tree.el -*- lexical-binding: t; -*-

;;; Code

;;; * constructor

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

;;; * accessors

(defun pb-tree? (x)
  (and (km? x)
       (km_contains? x :value)
       (km_contains? x :children)))

(defun pb-tree_value (x)
  (km_get x :value))

(defun pb-tree_children (tree)
  (km_get tree :children))

(defun pb-tree_path (path)
  "Generate a path for traversing a tree structure.
Interleaves :children with elements of PATH to create the path."
  (let ((path (cond ((keywordp path) (list path))
                    ((vectorp path) (append path ()))
                    (t path))))
    (sq_interleave (sq_repeat (length path) :children)
                   path)))

(defun pb-tree_get (tree path)
  "Get the node at PATH in TREE."
  (km_get tree (pb-tree_path path)))

(defun pb-tree_get-value (tree path)
  "Get the value at PATH in TREE."
  (km_get (pb-tree_get tree path) :value))

(defun pb-tree_get-children (tree path)
  "Get the children of the node at PATH in TREE."
  (km_get (pb-tree_get tree path) :children))

(defun pb-tree_get-path-values (tree path)
  "Traverse the TREE with PATH, accumulating intermediate values and returning a list of them."
  (pb_if (not path)
         (list (if (pb-tree? tree)
                   (pb-tree_value tree)
                 tree))
         [(km_keys values tree)
          (seq-reduce (pb_fn [(km_keys values tree) path-segment]
                             (pb_if [sub-tree (pb-tree_get tree path-segment)]
                                    (km :values (cons (km_get tree :value)
                                                      values)
                                        :tree sub-tree)))
                      path
                      (km :values () :tree tree))]
         (seq-reverse
          (cons (if (pb-tree? tree)
                    (pb-tree_value tree)
                  tree)
                values))))

;;; Testing

(defun pb-tree_run-tests ()
  "Run tests to verify the `pb-tree` macro and `pb-tree_node` function.
The tests check if the `pb-tree` macro correctly expands to the expected `pb-tree_node`
structure and that the resulting tree structure matches the desired nested format."
  (cl-assert
   (equal
    (macroexpand '(pb-tree "root node"
                           :child1 (node "child1"
                                         :grandchild-1-1 "foo"
                                         :grandchild-1-2 "bar")
                           :child2 (node "child2"
                                         :grandchild-2-1 "foo2"
                                         :grandchild-2-2 "bar2")))
    '(pb-tree_node "root node"
      :child1 (pb-tree "child1" :grandchild-1-1 "foo" :grandchild-1-2 "bar")
      :child2 (pb-tree "child2" :grandchild-2-1 "foo2" :grandchild-2-2 "bar2"))))

  (cl-assert
   (equal
    (pb-tree "root node"
             :child1 (node "child1"
                           :grandchild-1-1 "foo"
                           :grandchild-1-2 "bar")
             :child2 (node "child2"
                           :grandchild-2-1 "foo2"
                           :grandchild-2-2 "bar2"))

    '(:value "root node" :children
      (:child1
       (:value "child1" :children
               (:grandchild-1-1 "foo" :grandchild-1-2 "bar"))
       :child2
       (:value "child2" :children
               (:grandchild-2-1 "foo2" :grandchild-2-2 "bar2"))))))

  (cl-assert
   (let ((tree (pb-tree "root node"
                        :child1 (node "child1"
                                      :grandchild-1-1 "foo"
                                      :grandchild-1-2 "bar")
                        :child2 (node "child2"
                                      :grandchild-2-1 "foo2"
                                      :grandchild-2-2 "bar2"))))
     (and (equal
           (pb-tree_get tree '(:child2 :grandchild-2-1))
           "foo2")

          (equal
           (pb-tree_get tree '(:child1))
           (pb-tree "child1"
                    :grandchild-1-1 "foo"
                    :grandchild-1-2 "bar"))

          (equal
           (pb-tree_get tree ())
           tree))))

  (cl-assert
   (let ((tree (pb-tree "root node"
                        :child1 (node "child1"
                                      :grandchild-1-1 "foo"
                                      :grandchild-1-2 "bar")
                        :child2 (node "child2"
                                      :grandchild-2-1 "foo2"
                                      :grandchild-2-2 "bar2"))))
     (and

      (equal (list "root node")
             (pb-tree_get-path-values
              tree ()))

      (equal (list "root node" "child1" "foo")
             (pb-tree_get-path-values
              tree [:child1 :grandchild-1-1]))

      (not
       (or (pb-tree_get-path-values
            tree [:non-existant])

           (pb-tree_get-path-values
            tree [:child1 :non-existant])

           (pb-tree_get-path-values
            tree [:child1 :grandchild-1-1 :non-existant]))))))

  :ok)

(pb-tree_run-tests)
