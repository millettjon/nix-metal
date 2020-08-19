(ns jam.box.util
  (:require [clojure.string :as str]))

(defn slurp-proc
  [path]
  (->> path
       (str "/proc/")
       java.io.FileReader.
       slurp))

(defn meminfo
  "Returns memory info as a map."
  []
  (->> "meminfo"
       slurp-proc
       str/split-lines
       (reduce (fn [m line]
                 (let [[_ k v] (re-matches #"([^\s]+):\s+(\d+).*" line)]
                   (assoc m (keyword k) (Long/parseLong v))))
               {})))

;; ([] [to] [to from] [to xform from])
;; what does an xform? look like? fn?
;; TODO: handle xform
;; TODO: handle multiple arguments?
#_ (defn into*
     "Modified into where from can be a value or collection."
     [to from]
     (let [from (if (coll? from)
                  from
                  [from])]
       (into to from)))
