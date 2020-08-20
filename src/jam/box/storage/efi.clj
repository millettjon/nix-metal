(ns jam.box.storage.efi
  (:require [jam.sh :refer [$]]
            [jam.box.storage.disk :refer [parse-node create-device add-mount]]))

(defmethod create-device :efi-system
  [node opts]
  (let [[{:keys [mount] :as _node-opts} child] (parse-node node)

        ;; EFI system partition 512 MB
        opts  (assoc opts
                     :size 512
                     :type "EF00")
        parts (create-device child opts)]
    (doseq [part parts]
      ($ "mkfs.fat" "-F" "32" "-s" "1" "-n" "EFI" part))
    (when mount
      (let [mount-dir (if (:install? opts)
                             (str "/mnt" mount)
                             mount)]
        ;; Just mounting the first one for now.
        ;; Can these be raided?
        (add-mount opts {:type :vfat
                         :source (first parts)
                         :target mount-dir})))
    parts))
