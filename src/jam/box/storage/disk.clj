(ns jam.box.storage.disk
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [jam.log :refer [info die]]
            [jam.sh :refer [$ $>]]
            [jam.path :refer [exists? symlink? readlink basename]]
            [jam.box.unit :refer [GiB]]
            [jam.box.storage.util :refer [lsblk]]
            [jam.box.storage.udev :as udev]
            #_[clojure.pprint :refer [pprint]]))

(defn add-mount
  [opts mount]
  (swap! (:mounts opts) conj mount))

(defn parse-node
  [[_type & args]]
  (let [[args opts]  (if (map? (first args))
                       [(rest args) (first args)]
                       [args nil])
        [args child] [(rest args) (first args)]]
    (assert (empty? args) (str "extra arguments" (pr-str args)))
    [opts (if child child [:partition])]))

;; Signature for partition handler functions.
;; [conf] -> [partition ...]
(defmulti create-device
  (fn [[type] _opts]
    type))

(require '(jam.box.storage.efi))
(require '(jam.box.storage.swap))
(require '(jam.box.storage.luks))
(require '(jam.box.storage.raid :as raid))
(require '(jam.box.storage.zfs :as zfs))


(def disk-types
  {:hd "3"
   :sd "8"
   :nvme "259"})

(defn partitions
  "Returns the partitions on disk."
  [disk]
  (let [m (lsblk disk)]
    (->> m
         first
         :children
         (filter #(= "part" (:type %))))))

;; Define defaults for selecting disks.
(def DEFAULTS
  {:min-size (GiB 10) ; minimum size disk to consider
   :groups   {1 {:raid false}
              2 {:raid :5}
              4 {:raid :10}}})

(defn select
  "Retuns a map of disks that match selector."
  [{:keys [type] :as selector}]
  (let [type  (disk-types type)
        _     (assert type (str "bad disk selector: " (pr-str selector) "x"))
        disks (->> (lsblk [:nodeps ; limit to top level devices
                           [:include type]])
                   (filter #(>= (-> % :size)
                                (DEFAULTS :min-size))))]
    (and (empty? disks)
         (die "No suitables disks found."))
    (or (get-in DEFAULTS [:groups (count disks)])
        (die "No raid level configured for" (count disks) "disks"))
    (or (apply = (map :maj disks))
        (die "Can't mix diferent type devices."))
    (or (apply = (map :size disks))
        (die "Can't mix diferent size devices."))
    (let [disks (map :name disks)]
      (info "selected disks: " (pr-str disks))
      disks)))

;; Ref: https://github.com/xcp-ng/xcp/issues/107
;; These did not work as well:
;; ($ "wipefs" #_ "-q" "-a" dev)
;;   - ok, but doesn't handle raid
;; ($ "sgdisk" "--zap-all" dev)
;;   - breaks when raid is in use
(defn wipe-dev
  "Erase both raid and fs information if any. Uses dd to copy zeros
  over the first and last 512KiB of dev. Works on both disks and partitions."
  [dev]
  (let [sz-size (->> dev
                     ($> "blockdev" "--getsz")
                     Long/parseLong)
        args ["dd" "if=/dev/zero"
              (str "of=" dev) "bs=512"
              "count=1024" "status=none" "conv=fsync"]]
    (apply $> args)
    (apply $> (conj args (str "seek=" (- sz-size 1024))))))

(defn stop-all
  "Stop all block devices."
  ([]
   (info "stopping block-devices")
   (stop-all (lsblk [:output-all [:include (str/join "," (vals disk-types))]])))
  ([devices]
   ;; Walk device tree. Stop children first then stop self.
   (when-let [{:keys [path type children fstype mountpoint] :as _device} (first devices)]
     ;; Process children.
     (stop-all children)
     ;; Process self.
     ;; Check for existence as device may have already been closed.
     (when (exists? path)
       ;; Stop any zpools that use it.
       (when-let [pool (get (zfs/pools-by-device) (basename (or (readlink path) path)))]
         (zfs/pool-destroy pool))
       (info "stopping path" path)
       (when mountpoint
         (case fstype
           "swap" ($ "swapoff" path)
           ($ "umount" mountpoint)))
       (case type
         "crypt" ($ "cryptsetup" "luksClose" path)
         "raid1" (do
                   ;; Erases any signatures (e.g., swap)
                   (wipe-dev path)
                   (raid/stop path)
                   (udev/settle))
         "part"  :ok
         "disk"  :ok
         (die "unknown type" type)))
     ;; process siblings
     (stop-all (rest devices)))))

(defn wipe
  [disk]
  (let [dev (str "/dev/" disk)
        info  (partial info (str disk " - "))]
    (stop-all (lsblk [:output-all] dev))
    (info "wiping paritions")
    ;; Note: It is important to wipe these right away. Otherwise, if
    ;; the kernel finds raid metadata it will and try and use the
    ;; partition before we get a chance to wipe it.
    (doseq [{:keys [name]} (partitions dev)]
      (wipe-dev (str "/dev/" name)))
    (wipe-dev dev)))

(defn disk-by-id
  [disk]
  (->> "/dev/disk/by-id"
       io/file
       file-seq
       (filter symlink?)
       (some #(when (= disk (-> % readlink .getFileName str))
                #_ (.getName %)
                (.getPath %)))))

;; Note: Run "sgdisk -L" to list partition types
(defn new-part
  [disk size type]
  (let [part-num (-> disk partitions count inc)
        part     (str disk "-part" part-num) ; depends on device type ...
        ;; For first partition,
        ;; - leave 16k space at start of disk for partition table
        ;; - align to 1M boundary for performance
        start (if (= 1 part-num) "1M" "0")
        end (cond
              (string? size) (str "+" size)
              (number? size) (str "+" size "M")
              (nil? size) "0" ; largest available
              :else (die "unsupported size" size))
        n-arg (str "-n" part-num ":" start ":" end)

        ;; If type is left out, it will default to platform specific type.
        t-arg (when type
                (str "-t" part-num ":" type))]
    (apply $ (filter identity ["sgdisk" n-arg t-arg disk]))
    #_ ($ "sgdisk" n-arg t-arg disk)
    ($> "partprobe" disk)
    (udev/settle)
    (wipe-dev part)
    part))

(defmethod create-device :partition
  [_device {:keys [disks size type]}]
  (doall
   (map (fn [disk]
          (let [id (disk-by-id disk)]
            (new-part id size type)))
        disks)))
