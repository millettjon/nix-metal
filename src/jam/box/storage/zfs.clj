(ns jam.box.storage.zfs
  (:require [clojure.string :as str]
            [jam.sh :refer [$ $>]]
            [jam.box.storage.util :refer [unmount-tree findmnt]]
            [jam.box.storage.disk :refer [parse-node create-device add-mount]]))

(defn pools
  "Returns the list of zfs pools."
  []
  (let [pools ($> "zpool" "list" "-o" "name" "-H")]
    (when-not (empty? pools)
      (str/split-lines pools))))

(defn pool-unmount
  "Unmounts any mounted filesytems under pool."
  [pool]
  (unmount-tree (findmnt "-t" "zfs") (re-pattern (str "^" pool "($|/.+)"))))

(defn pool-destroy
  "Destroys zfs pool pool."
  [pool]
  ;; Note: force unmounting with -f doesn't work.
  (pool-unmount pool)
  ($ "zpool" "destroy" #_"-f" pool))

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

;; Ref:
;; - https://nixos.wiki/wiki/NixOS_on_ZFS
;; - https://openzfs.github.io/openzfs-docs/Getting%20Started/Ubuntu/Ubuntu%2020.04%20Root%20on%20ZFS.html
(def zfs-rpool-options
  ["-o" "ashift=12"           ; ok
   "-O" "acltype=posixacl"    ; ok, for journald
   "-O" "compression=lz4"     ; ok,
   "-O" "xattr=sa"            ; ok, for journald
   "-O" "mountpoint=none"     ; ok
   "-O" "atime=off"           ; ok, better performance
   ])

(defmethod create-device :zfs/pool
  [node {:keys [install? mounts]:as opts}]
  (let [[{pool-name :name
          :keys     [datasets]} child] (parse-node node)
        parts                          (create-device child (assoc opts :type "BF00"))]
    ($ "zpool" "create"
       zfs-rpool-options
       (pool pool-name parts)
       parts)

    ;; Add dataset to reserve space to easily recover if zfs runs out of space.
    ($ "zfs" "create" "-o" "refreservation=1G" "-o" "mountpoint=none" (str pool-name "/reserved"))

    ;; Create actual datasets.
    (doseq [{:keys [name mount] :as _dataset} datasets]
      (let [dataset-name   (str pool-name "/" name)
            mountpoint-opt (str "mountpoint=" (if mount "legacy" "none"))
            mount-dir      (if install?
                             (str "/mnt" mount)
                             mount)]
        ($ "zfs" "create" "-o" mountpoint-opt dataset-name)
        (add-mount opts {:type :zfs
                         :source dataset-name
                         :target mount-dir})))
    pool-name))

;; TODO ? enable compression on zfs partitions?
