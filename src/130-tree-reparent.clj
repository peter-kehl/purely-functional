(def reparent
  (fn [pickup whole-tree]
    (let [node? symbol?
          tree? seq?
          contained? (fn [item coll] {:pre[item #_not-for-null-false-or-nil]}
                       (some (partial = item) coll))
          subtree-seq (tree-seq seq? rest whole-tree)
          root (first whole-tree)
          node-subtrees-result? (fn [[node subtrees :as whole]]
                                  (and (or (= (count whole) 2) (empty? whole))
                                       (or (empty? whole) (and (node? node) (seq? subtrees)))
                                       (every? (some-fn tree? node?) subtrees))) ;every? works with coll being nil
          subtree-seq-item? (fn [item] (or (tree? item) (node? item)))
          node-subtrees (fn [seq-item] {:pre [(subtree-seq-item? seq-item)] :post [(node-subtrees-result? %)]}
                             (if (tree? seq-item)
                               [(first seq-item) (rest seq-item)]
                               []))
          subtrees (reduce (fn [res seq-item] ;subtrees is a map: node parent to a seq of subtrees
                             (let [pair (node-subtrees seq-item)]
                               (if (empty? pair) ;CLJ 1.4 doesn't have 1-arg (conj coll), hence can't (apply conj coll ...)
                                 res
                                 (conj res pair))))
                           {} subtree-seq)
          _ (assert (= (subtrees (first whole-tree)) (rest whole-tree)))
          _ (assert (every? node? (keys subtrees)))
          _ (assert (every? tree? (vals subtrees)))
          node-of (fn [tree-or-node] {:pre [(or (node? tree-or-node) (tree? tree-or-node))] :post [(node? %)]}
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
          pickup-ancestors (ancestors pickup)
          pickup-subtrees (subtrees pickup)
          pickup-tree (cons)]

      )))


