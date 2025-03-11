;;; pb/xp/pb-tree.el -*- lexical-binding: t; -*-

;;; Code

;;; * constructor

(defun pb-tree* (value &optional children)
  (km :value value
      :children children))

(defmacro pb-tree (value &rest children)
  "Create a tree structure from nested node expressions.
BODY should be a node expression, which can contain nested node expressions."
  `(km
    :value ,value
    :children (km ,@(km_map-vals children
                                 (lambda (v)
                                   (if (and (listp v) (eq (car v) 'node))
                                       `(pb-tree ,@(cdr v))
                                     `(pb-tree* ,v)))))))

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

(defun pb-tree_contains? (tree path)
  "Check that TREE has a node at PATH."
  (km_contains? tree (pb-tree_path path)))

(defun pb-tree_get (tree path)
  "Get the node at PATH in TREE."
  (km_get tree (pb-tree_path path)))

(defun pb-tree_get-value (tree path)
  "Get the value at PATH in TREE."
  (km_get (pb-tree_get tree path) :value))

(defun pb-tree_get-children (tree path)
  "Get the children of the node at PATH in TREE."
  (km_get (pb-tree_get tree path) :children))

(defun pb-tree_traverse (tree path)
  "Traverse the TREE with PATH, accumulating intermediate node values."
  (seq-reduce (pb_fn [(km_keys values node) child-path]
                     (pb_if [child-node (pb-tree_get node child-path)]
                            (km :values (cons (pb-tree_value node)
                                              values)
                                :node child-node)))
              path
              (km :values () :node tree)))

(defun pb-tree_get-path-values (tree path)
  "Traverse the TREE with PATH, accumulating intermediate values and returning a list of them."
  (if (pb-tree_contains? tree path)
      (pb_let [(km_keys values node) (pb-tree_traverse tree path)]
          (seq-reverse
           (cons (pb-tree_value node)
                 values)))))

(defun pb-tree_merge (tree1 tree2)
  "Merge two trees TREE1 and TREE2 into a new tree.
Starts with the value of TREE2 as the root value and recursively
merges the children of TREE1 and TREE2 recursively.

TREE1 and TREE2 are expected to be trees created by `pb-tree` or `pb-tree_node`.

Returns a new tree with the merged structure."
  (pb_if
   (not (pb-tree? tree1)) tree2
   (not (pb-tree? tree2)) tree1
   (km :value
       (pb-tree_value tree2)
       :children
       (km_merge-with #'pb-tree_merge
                      (pb-tree_children tree1)
                      (pb-tree_children tree2)))))

(defun pb-tree_select (tree x)
  "Select a subset of TREE according to the selector X.
TREE is a tree created with `pb-tree` or `pb-tree*`.
X can be:
- A keyword: selects only the subtree at that key path
- A vector: selects a nested path through the tree
- A keyword map: for each key in the map, selects corresponding subtrees
  where values determine further selection in those subtrees
- nil or empty: returns the entire tree

Returns a new tree containing only the selected parts of the original tree."
  (pb_if
   (keywordp x) (pb-tree* (pb-tree_value tree)
                          (km x (pb-tree_get tree x)))

   (or (null x)
       (seq-empty-p x)
       (eq t x)) tree

   (km? x)
   (seq-reduce (pb_fn [ret (cons k v)]
                      (pb-tree_merge ret
                                     (km_upd (pb-tree_select tree k)
                                             (list :children k)
                                             (pb_fn [subtree] (pb-tree_select subtree v)))))
               (km_entries x) ())

   (vectorp x)
   (let ((k (aref x 0))
         (x (vconcat (cdr (append x nil)))))
     (km_upd (pb-tree_select tree k)
             (list :children k)
             (pb_fn [subtree] (pb-tree_select subtree x))))))

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
    '(km
      :value "root node"
      :children (km :child1 (pb-tree "child1" :grandchild-1-1 "foo" :grandchild-1-2 "bar")
                    :child2 (pb-tree "child2" :grandchild-2-1 "foo2" :grandchild-2-2 "bar2")))))

  (cl-assert
   (equal
    (pb-tree "root node"
             :child1 (node "child1"
                           :grandchild-1-1 "foo"
                           :grandchild-1-2 "bar")
             :child2 (node "child2"
                           :grandchild-2-1 "foo2"
                           :grandchild-2-2 "bar2"))
    '(:value "root node"
      :children (:child1
                 (:value "child1"
                  :children (:grandchild-1-1 (:value "foo" :children nil)
                             :grandchild-1-2 (:value "bar" :children nil)))

                 :child2
                 (:value "child2"
                  :children (:grandchild-2-1 (:value "foo2" :children nil)
                             :grandchild-2-2 (:value "bar2" :children nil)))))))

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
           (pb-tree* "foo2"))

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

  (progn :pb-tree_merge
         (cl-assert
          (let* ((tree1 (pb-tree "Root One"
                                 :child1 "A"
                                 :child2 "B"))
                 (tree2 (pb-tree "Root Two"
                                 :child2 "C"
                                 :child3 "D")))
            ;; Test merging two trees where tree2 has priority at the root level
            (km_eq
             (pb-tree_merge tree1 tree2)
             (pb-tree "Root Two"
                      :child1 "A"
                      :child2 "C"
                      :child3 "D"))))

         (cl-assert
          (let* ((tree1 (pb-tree "R1"
                                 :a (node "A1"
                                          :x "x1")
                                 :b "B"))
                 (tree2 (pb-tree "R2"
                                 :a (node "A2"
                                          :y "y1")
                                 :c "C")))
            ;; Test deeper merging where children are merged
            (km_eq
             (pb-tree_merge tree1 tree2)
             (pb-tree "R2"
                      :a (node "A2"
                               :x "x1"
                               :y "y1")
                      :b "B"
                      :c "C")))))

  (progn :pb-tree_select

         (defvar pb-tree_ex1
           (pb-tree "root node"
                    :child1 (node "child1"
                                  :grandchild-1-1 "foo"
                                  :grandchild-1-2 "bar")
                    :child2 (node "child2"
                                  :grandchild-2-1 "foo2"
                                  :grandchild-2-2 (node "bar2"
                                                        :nested "nested"))))

         (cl-assert
          (and (km_eq (pb-tree_select pb-tree_ex1
                                      ())
                      pb-tree_ex1)

               (km_eq (pb-tree_select pb-tree_ex1
                                      :child1)
                      '(:value "root node"
                        :children (:child1 (:value "child1"
                                            :children (:grandchild-1-1 (:value "foo" :children nil)
                                                       :grandchild-1-2 (:value "bar" :children nil))))))

               (km_eq (pb-tree_select pb-tree_ex1
                                      [:child1 :grandchild-1-1])
                      '(:value "root node"
                        :children (:child1 (:value "child1"
                                            :children (:grandchild-1-1 (:value "foo" :children nil))))))

               (km_eq (pb-tree_select pb-tree_ex1
                                      [:child2 :grandchild-2-2 :nested])
                      '(:value "root node"
                        :children (:child2 (:value "child2"
                                            :children (:grandchild-2-2 (:value "bar2"
                                                                        :children (:nested (:value "nested"
                                                                                            :children nil))))))))

               (km_eq (pb-tree_select pb-tree_ex1
                                      (km :child1 :grandchild-1-1
                                          :child2 [:grandchild-2-2 :nested]))
                      '(:value "root node"
                        :children (:child1 (:value "child1" :children (:grandchild-1-1 (:value "foo" :children nil)))
                                   :child2 (:value "child2"
                                            :children (:grandchild-2-2 (:value "bar2"
                                                                        :children (:nested (:value "nested" :children nil)))))))))))

  :ok)

(pb-tree_run-tests)
