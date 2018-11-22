(def reparent
  (fn [pickup whole-tree]
    (let [node? symbol?
          tree? (fn tree? [tree] (and (seq? tree)
                                      (node? (first tree))
                                      (every? tree? (rest tree))))
          contained? (fn [item coll] {:pre[item #_not-for-null-false-or-nil]}
                       (some (partial = item) coll))
          subtree-seq (tree-seq seq? rest whole-tree)
          root (first whole-tree)
          node-subtrees-result? (fn [[node subtrees :as whole]]
                                  (and (or (= (count whole) 2) (empty? whole))
                                       (or (empty? whole) (and (node? node) (seq? subtrees)))
                                       (every? (some-fn tree? node?) subtrees))) ;every? works with coll being nil
          tree-or-node? (fn [item] (or (tree? item) (node? item)))
          subtree-seq-item? tree-or-node?
          node-subtrees (fn [seq-item] {:pre [(subtree-seq-item? seq-item)] :post [(node-subtrees-result? %)]}
                             (if (tree? seq-item)
                               [(first seq-item) (rest seq-item)]
                               []))
          subtrees (reduce (fn [res seq-item] ;subtrees is a map: node parent to a seq of subtrees
                             (let [pair (node-subtrees seq-item)]
                               (if (empty? pair)
                                 res
                                 (conj res pair))))
                           {} subtree-seq)
          _ (assert (= (subtrees (first whole-tree)) (rest whole-tree)))
          _ (assert (every? node? (keys subtrees)))
          _ (assert (every? seq?  (vals subtrees)))
          _ (assert (every? (partial every? (some-fn tree? node?)) (vals subtrees)))
          node-of (fn [tree-or-node] {:pre [(tree-or-node? tree-or-node)] :post [(node? %)]}
                    (if (node? tree-or-node)
                      tree-or-node
                      (first tree-or-node))
                    )
          node-parent? (fn [[node parent :as whole]] {:pre [(= (count whole) 2) (node? node)]}
                         (and (node? parent)
                              (some (partial = node) (apply node-of (subtrees parent)))))
          nodes-parents (fn [seq-item] {:pre [(subtree-seq-item? seq-item)] :post [(map? %) (every? node-parent? %)]}
                          (if (tree? seq-item)
                            (let [parent (first seq-item)]
                              (assert (node? parent))
                              (reduce (fn [res subtree] {:pre [(map? res) (subtree-seq-item? subtree)]}
                                        (assoc res (node-of subtree) parent))
                                      {} (rest seq-item)))
                            {})
                          )
          parents (reduce (fn [res seq-item] ;parents is a map: node child to node direct parent. No entry for root.
                            (into res (nodes-parents seq-item)))
                          {} subtree-seq)
          _ (assert (every? (fn [[child parent]] {:pre [(node? child) (node? parent)]}
                              (node-parent? child parent))
                            parents))
          direct-of? (fn [tree node] {:pre[(tree? tree) (node? node)]}
                       (or (contained? node tree)
                           (some (comp (partial = node) first) tree)))
          ancestors-result? (fn ancestors-result? [grandchild ancestors] ;ancestors' order: immediate first, elder second
                              (assert (= (nil? ancestors) (= grandchild root)))
                              (or (nil? ancestors)
                                  (and (= (parents grandchild) (first ancestors))
                                       (ancestors-result? (first ancestors) (next ancestors)))))
          ancestors (fn ancestors [grandchild] {:pre [(node? grandchild)] :post [(ancestors-result? grandchild %)]} ;Return a seq of nodes: immediate parent first, elder second; or nil
                      (let [parent (parents grandchild)]
                        (if parent
                          (cons parent (ancestors parent))
                          nil
                          )))
          remove-child (fn [tree child-node] {:pre  [(tree? tree) (node? child-node)]
                                               :post [(tree? %)]}
                         (filter (complement (comp (partial = child-node)) node-of) tree))
          pickup-ancestors (ancestors pickup)
          pickup-subtrees (subtrees pickup)
          ;pickup-tree (seq (conj [pickup]))
          pickup-tree-without-ancestors (concat (list pickup) pickup-subtrees)
          ancestors-underOBS (seq (reduce (fn [res ancestor-seq]
                                          )
                                          () ancestors))
          ; 1. (nil? ancestors) ==> nil
          ; 2. ancestors is
          _ '((parent-symb (other-child #_nieces) (child #_own-children #_to_remove)                              #_siblings)
              (grand-parent-symb (uncle #_cousins) (parent-symb #_etc #_to_remove)                       #_uncles)
              (great-grandparent-symb (grand-uncle #_etc) (grand-parent-symb #_extended-fam #_to_remove) #_grand-uncles)
              #_etc)
          ; =>
          _ '((parent-symb (other-child #_nieces) #_child-removed #_siblings
                           (grand-parent-symb (uncle #_etc) #_parent-removed #_uncles
                                              (great-grandparent-symb (grand-uncle #_etc) #_etc
                                                                      #_etc))))
              #_etc)
          ancestors-under ((fn upside-down [child ancestors] ;child is used only for removal, it won't be in the returned sub-structure. ancestors is non-empty, or nil (hence you can use outer map's value: (ancestors child)).
                             {:pre [(node? child) (or (nil? ancestors) (seq ancestors))]
                              :post [(tree? %)]}
                              (if-let [parent (first ancestors)]
                                (let [parent-separated (remove-child parent child)
                                      elders (next ancestors)]
                                  (if elders
                                    (concat parent-separated
                                            (list (upside-down (node-of parent) elders)))
                                    parent-separated)))))
                            ]

      )))


