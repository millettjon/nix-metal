(ns jam.box.storage.efi
  (:require [jam.sh :refer [$]]
            [jam.box.storage.disk :refer [parse-node create-device]]))

(defmethod create-device :efi-system
  [node opts]
  (let [[_node-opts child] (parse-node node)

        ;; EFI system partition 512 MB
        opts  (assoc opts
                     :size 512
                     :type "EF00")
        parts (create-device child opts)]
    (doseq [part parts]
      ($ "mkfs.fat" "-F" "32" "-s" "1" "-n" "EFI" part))
    parts))
