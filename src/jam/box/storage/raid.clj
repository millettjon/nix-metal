(ns jam.box.storage.raid
  (:require [jam.sh :refer [$]]
            [jam.box.storage.disk :refer [parse-node create-device]]
            [jam.log :refer [info]]))

(defn stop
  [path]
  ($ "mdadm" "--stop" path))

(defn disable-recovery
  "Disable RAID recovery. We don't want this to slow down machine provisioning
   in the rescue mode. It can run in normal operation after reboot."
  []
  ($ "sysctl" "-w" "dev.raid.speed_limit_max=0"))

(defmethod create-device :raid
  [node {:keys [disks] :as opts}]
  (let [[{:keys [name]} child] (parse-node node)

        raid? (> 1 (count disks))
        opts  (if raid?
                (assoc opts :type "FD00") ; Linux RAID
                opts)
        parts (create-device child opts)]
    (if raid?
      (do
        (disable-recovery)
        (let [dev   (str "/dev/md/" name)
              level (case (count parts)
                      2 "1"
                      4 "10")]
                ($ ["mdadm" "--create" "--run" "--verbose"
                    dev "--level" level
                    "--raid-devices" (count parts)
                    "--homehost=<none>"
                    "--name" name]
                   parts)
                dev))
      (do
        (info "not raiding single device")
        parts))))
