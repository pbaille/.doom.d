;;; pb/xp/modes/tree-example.el -*- lexical-binding: t; -*-

(km :foo "bar"
    :child1 (km :a 1
                :b 2
                :c "hello"
                :d [1 2 3]
                :nested (km :x "deep value"
                            :y '(nested list)
                            :z (km :even "deeper"
                                   :level 3
                                   :data (list 1 2 3))))
    :child2 (km :name "example"
                :count 42
                :active t
                :properties (km :color "blue"
                                :size "medium"
                                :priority 'high
                                :tags '(:important :urgent)))
    :other (km :f (lambda () (interactive) (buffer-name))
               :g "another value"
               :h '(one two three)
               :i (km :function (lambda (x) (* x x))
                      :description "Utility functions"
                      :examples (km :ex1 '(1 2 3)
                                    :ex2 '(a b c)
                                    :ex3 (km :subex "example"
                                             :detail "More information"))))
    :metadata (km :created "2023-05-10"
                  :version 1.5
                  :tags '(:demo :example :test)
                  :author (km :name "Developer"
                              :email "dev@example.com"
                              :role "maintainer")
                  :history (km :v1 "Initial release"
                               :v2 "Added features"
                               :v3 "Bug fixes"
                               :changes (km :added '(feature1 feature2)
                                            :removed '(old-feature)
                                            :modified '(existing-feature))))
    :additional (km :purpose "Demonstration"
                    :complexity "Medium"
                    :performance (km :speed "fast"
                                     :memory "efficient"
                                     :metrics (km :benchmark1 0.5
                                                  :benchmark2 0.7
                                                  :comparison (km :baseline 1.0
                                                                  :improvement "30%")))))
