(ns compost.core
  (:require [clojure.set]))


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
  (domain [this] (set (keys this)))
  (image [this] (set (vals this)))

  clojure.lang.PersistentList$EmptyList
  (domain [this] #{})
  (image [this] #{}))

(defn comp'
  ([& fns] (if (every? set? fns)
             (apply clojure.set/intersection fns)
             (let [dom (domain (last fns))
                   f   (apply clojure.core/comp fns)]
               (reduce #(let [value (f %2)]
                          (if (nil? value)
                            %1
                            (assoc %1 %2 value)))
                       {}
                       dom)))))

(defn comp''
  ([& fns]
   (if (every? set? fns)
     (apply clojure.set/intersection fns)
     (let [dom (domain (last fns))
           f   (juxt identity (apply clojure.core/comp fns))]
       (into {}
             (clojure.core/comp
              (map f)
              (filter #(some? (second %))))
             dom)))))
