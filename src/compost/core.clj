(ns compost.core)


(defprotocol DomainImage
  (domain [this])
  (image [this]))

(extend-protocol DomainImage
  clojure.lang.PersistentVector
  (domain [this] (set (range 0 (count this))))
  (image [this] (set this))

  clojure.lang.PersistentHashSet
  (domain [this] this)
  (image [this] this)

  clojure.lang.PersistentArrayMap
  (domain [this] (keys this))
  (image [this] (vals this))

  clojure.lang.PersistentList$EmptyList
  (domain [this] #{})
  (image [this] #{}))

(defn comp
  ([& fns] (if (every? set? fns)
             (apply clojure.set/intersection fns)
             (let [dom (domain (last fns))
                   f   (apply comp fns)]
               (reduce #(if-let [v (f %2)]
                          (assoc %1 %2 v)
                          %1) {} dom)))))
