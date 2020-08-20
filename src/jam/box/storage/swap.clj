(ns jam.box.storage.swap
  (:require [jam.sh :refer [$]]
            [jam.box.storage.disk :refer [parse-node create-device]]
            [jam.box.util :as util]
            [jam.log :refer [die]]))

(defn calculate-MiB
  []
  (let [kiB      (:MemTotal (util/meminfo))
        MiB      (/ kiB 1024)]
    ;; Use 1/8 of total memory.
    ;;(-> MiB (/ 8) long)
    (-> MiB (/ 1) long)))

(defmethod create-device :swap
  [node {:keys [disks] :as opts}]
  (let [[_node-opts child] (parse-node node)
        swap-size (calculate-MiB)
        opts      (assoc opts
                         :type "8200" ; Linux Swap
                         :size (/ swap-size (count disks)))
        parts (create-device child opts)]
    (doseq [part parts]
      ($ "mkswap" "-L" "swap" part)
      ($ "swapon" part))
    parts))
