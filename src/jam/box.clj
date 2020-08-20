(ns jam.box
  (:require [jam.path :as path]
            [jam.sh :refer [$]]
            [jam.box.storage.util :as util]
            [jam.log :refer [info]]))

(defn breadth-first
  [& roots]
  (when (seq roots)
    (concat (map first roots)
            (apply breadth-first (mapcat rest roots)))))

(defn breadth-first-mounts
  "Given a mount tree, returns a list of mounts in breadth first order."
  [node]
  (let [children (->> node
                     (map (fn [[k v]]
                            (when (string? k)
                              v)))
                     (filter identity))]
    (concat (when (:target node) [node])
            (mapcat breadth-first-mounts children))))

(defn mount-all
  [mounts]
  ;; Sort mounts into a tree.
  (let [mount-tree (reduce (fn [tree {:keys [target] :as mount}]
                             (update-in tree
                                        (path/split target)
                                        merge mount))
                           {} mounts)]
    ;; Mount in proper order.
    (doseq [{:keys [type source target] :as mount}
            (breadth-first-mounts mount-tree)]
      (util/mount type source target))))

(defn nix-install
  []
  ($ "nixos-generate-config" "--root" "/mnt")
)
;; edits to configuration.nix
;;   - boot.supportedFilesystems = [ "zfs" ];
;;   - networking.hostName
;;   - networking.hostId
;; nixos-install


;; TODO validate w/ spec
(defn build
  [{:keys [disk-groups] :as machine}]
  (info "building" machine)
  ;; The list of things to mount needs to be coordinated
  ;; across all disk groups so save mounts in an atom
  ;; so that correct mount order can be determined later.
  (let [mounts (atom [])]
    #_(doseq [[{:keys [selector] :as _opts}
             & devices] disk-groups]
      (let [disks (disk/select selector)]
        (info "building disks:" disks)
        (doseq [disk disks]
          (disk/wipe disk))
        (info "creating devices")
        (doseq [device devices]
          (info "creating device" device)
          (disk/create-device device {:disks disks
                                      :mounts mounts
                                      :install? true}))))
    ;; mounts
    #_(mount-all @mounts)

    ;; continue install
    (nix-install)
    ))
