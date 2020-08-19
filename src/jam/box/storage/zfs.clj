(ns jam.box.storage.zfs
  (:require [clojure.string :as str]
            [jam.sh :refer [$ $>]]
            [jam.box.storage.disk :refer [parse-node create-device]]))

(defn pools
  "Returns the list of zfs pools."
  []
  (let [pools ($> "zpool" "list" "-o" "name" "-H")]
    (when-not (empty? pools)
      (str/split-lines pools))))

(defn pool-destroy
  "Destroys zfs pool pool."
  [pool]
  ($ "zpool" "destroy" #_ "-f" pool))

(defn pools-destroy
  "Destroys all zfs pools."
  []
  (doseq [pool (pools)]
    (pool-destroy pool)))

(defn pool-devices
  "Returns the list of devices in a pool."
  [pool]
  (->> pool
       ($> "zpool" "status" "-L")
       str/split-lines
       (map #(re-matches #"^\t +([^\s]+).*" %))
       (filter identity)
       (map second)))

(defn pools-by-device
  "Returns a map of device to pool that is using it."
  []
  (reduce (fn [m pool]
            (reduce (fn [m dev]
                      (assoc m dev pool))
                    m (pool-devices pool)))
          {} (pools)))

(defn pool
  [name parts]
  (if (> 1 (count parts))
    [name "mirror"]
    name))

;; Ref: https://openzfs.github.io/openzfs-docs/Getting%20Started/Ubuntu/Ubuntu%2020.04%20Root%20on%20ZFS.html
(def zfs-rpool-options
  ["-o" "ashift=12"           ; ok
   "-O" "acltype=posixacl"    ; ok, for journald
   "-O" "compression=lz4"     ; ok,
   "-O" "xattr=sa"            ; ok, for journald
   "-O" "mountpoint=none"     ; ok
   "-O" "atime=off"           ; ok, better performance
   ])

(defmethod create-device :zfs/pool
  [node opts]
  (let [[{:keys [name]} child] (parse-node node)
        parts (create-device child (assoc opts :type "BF00"))]
    ($ "zpool" "create"
       zfs-rpool-options
       (pool name parts)
       parts)
    name))

;; TODO create data set
;; TODO create a reservation data set
;; TODO mount

#_ [:zfs/pool {:name "root_pool"
            :datasets [{:name "root" :mount "/"}
                       {:name "nix" :mount "/nix"}]}
 [:luks]]
