(ns jam.box
  (:require [jam.box.storage.disk :as disk]
            [jam.log :refer [info]]))

;; TODO validate w/ spec
(defn build
  [{:keys [disk-groups] :as machine}]
  (info "building" machine)
  (doseq [[{:keys [selector] :as _opts}
           & devices] disk-groups]
    (let [disks (disk/select selector)]
      (info "building disks:" disks)
      (doseq [disk disks]
        (disk/wipe disk))
      (info "creating devices")
      (doseq [device devices]
        (info "creating device" device)
        (disk/create-device device {:disks disks})))))
