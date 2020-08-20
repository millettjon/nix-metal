(ns jam.box.storage.util
  (:require [clojure.string :as str]
            [cheshire.core :as json]
            [jam.sh :refer [$ $> kw->opt]]
            [jam.log :refer [die]]))

;; Getting the model may be useful.
;; lsblk -io NAME,TYPE,SIZE,MOUNTPOINT,FSTYPE,MODEL,HOTPLUG
;; lsblk --help to list all options

(defn lsblk
  "Returns information about attached block devices.
  See: https://www.kernel.org/doc/Documentation/admin-guide/devices.txt"
  ([]
   (lsblk []))
  ([arg]
   (cond (string? arg) (lsblk [] arg)
         (coll? arg)   (lsblk arg nil)
         :else         (die "Unexpected argument to lsblk:" arg)))
  ([opts device & devices]
   (let [cmd   ["lsblk" "--json" "--bytes"]
         ;; add flags and options to argument list
         cmd   (reduce (fn [v opt]
                         (let [args (if (coll? opt)
                                      [(kw->opt (first opt)) (second opt)]
                                      [(kw->opt opt)])]
                         (concat v args)))
                     cmd
                     opts)
         cmd   (if device
               (concat cmd [device] devices)
               (concat cmd devices))
         disks (-> (apply $> cmd)
                   (json/parse-string true)
                   :blockdevices)
         ;; filter out removeable disks e.g., usb sticks
         disks (remove :rm disks)]
     ;; split :maj:min column into :maj and :min
     (reduce (fn [disks disk]
               (let [[maj min] (-> :maj:min disk (str/split #":"))
                     disk      (-> disk
                                   (dissoc :maj:min)
                                   (assoc :maj maj
                                          :min min))]
                 (conj disks disk)))
             []
             disks))))

(defn mount
  [type source target]
  ($ "mkdir" "-p" target)
  ($ "mount" "-t" (name type) source target))

(defn findmnt
  [& args]
  (let [cmd          (concat ["findmnt" "--bytes" "--json"] args)
        file-systems (-> (apply $> cmd)
                         (json/parse-string true)
                         :filesystems)]
    file-systems))

(defn unmount-tree
  "Walks a findmnt tree and unmounts any mounts matching pattern.
  Children are unmounted before parents."
  [nodes pattern]
  (doseq [{:keys [source
                  target
                  children] :as _node} nodes]
    (when (re-matches pattern source)
      ;; unmount children
      (when children
        (unmount-tree children pattern))
      ;; unmount self
      ;; Note:
      ;; - Use -R since there may be datasets from other zpools
      ;;   mounted onto this one.
      ;; - There is no way to unmount a mount that has been shadowed
      ;;   by a second mount without first unmounting the second
      ;;   mount.
      ;; - If all else fails, umount everything: e.g.: umount -t zfs -a
      ($ "umount" "-R" target))))
