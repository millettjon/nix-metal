(ns jam.metal.partition
  (:require [clojure.pprint :refer [pprint]]
            [clojure.java.io :as io]
            [cheshire.core :as json]
            [jam.path :refer [exists? symlink? readlink]]
            [clojure.string :as str]
            [jam.sh :refer [$ $> die]]))


(defn GiB
  "Converts GiB to bytes."
  [gigi-bytes]
  (* gigi-bytes
     (Math/pow 2 (* 10 3))))

(defn ->GiB
  "Converts bytes to GiB."
  [bytes]
  (/ bytes
     (Math/pow 2 (* 10 3))))
#_ (pprint (->GiB (* 2 1024 1024 1024)))

(defn ->GB
  "Converts bytes to GB."
  [bytes]
  (/ bytes
     (Math/pow 10 (* 3 3))))
#_ (pprint (->GB (* 2 1000 1000 1000)))

;; Define defaults.
(def CONF
  {:disk {:min-size (GiB 10) ; minimum size disk to consider
          :groups   {1 {:raid false}
                     2 {:raid :5}
                     4 {:raid :10}}}})

(defn udev-settle
  "Wait for udev to update /dev symlinks."
  []
  ($> "udevadm" "settle"))

(defn disk-by-id
  [disk]
  (->> "/dev/disk/by-id"
       io/file
       file-seq
       (filter symlink?)
       (some #(when (= disk (-> % readlink .getFileName str))
                #_ (.getName %)
                (.getPath %)))))

(defn kw->option
  "Converts keyword opt into a command line option string."
  [opt]
  (str "--" (name opt)))


;; "blockdevices": [
;;                  {"name":"loop0", "maj:min":"7:0", "rm":false, "size":1212456960, "ro":true, "type":"loop", "mountpoint":"/nix/.ro-store"},
;;                  {"name":"sda", "maj:min":"8:0", "rm":false, "size":1000204886016, "ro":false, "type":"disk", "mountpoint":null},
;;                  {"name":"sdb", "maj:min":"8:16", "rm":true, "size":31142707200, "ro":true, "type":"disk", "mountpoint":null},
;;                  {"name":"nvme0n1", "maj:min":"259:0", "rm":false, "size":128035676160, "ro":false, "type":"disk", "mountpoint":null}
;;                  ]

;; internal scsc hdd 8:0
;; usbstick 8:16
;; nvme 259:0
;; ? why does usb show up a?

;; Getting the model is useful:
;; - ? make sure model matches?
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
                                      [(kw->option (first opt)) (second opt)]
                                      [(kw->option opt)])]
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

(defn slurp-proc
  [path]
  (->> path
       (str "/proc/")
       java.io.FileReader.
       slurp))

(defn meminfo
  "Returns memory info as a map."
  []
  (->> "meminfo"
       slurp-proc
       str/split-lines
       (reduce (fn [m line]
                 (let [[_ k v] (re-matches #"([^\s]+):\s+(\d+).*" line)]
                   (assoc m (keyword k) (Long/parseLong v))))
               {})))

(defn partitions
  "Returns the partitions on disk."
  [disk]
  (let [m (lsblk disk)]
    (->> m
         first
         :children
         (filter #(= "part" (:type %))))))

(defn calculate-swap-MiB
  []
  (let [kiB      (:MemTotal (meminfo))
        MiB      (/ kiB 1024)]
    ;; Use 1/8 of total memory.
    (-> MiB (/ 8) long)))

(defn l
  [& args]
  (println "----" (apply str args) "----"))

(def disk-types
  {:hd "3"
   :ssd "8"
   :nvme "259"})

(defn select-disks
  "Identify disks and select partitioning scheme."
  [conf]
  (let [types (map disk-types (or (:disk-types conf) (keys disk-types)))
        _ (pprint types)
        disks (->> (lsblk [:nodeps ; limit to top level devices
                           [:include (str/join "," types)]])
                   (filter #(>= (-> % :size)
                                (-> CONF :disk :min-size))))]
    (pprint disks)
    (and (empty? disks)
         (die "No suitables disks found."))
    (or (get-in CONF [:disk :groups (count disks)])
        (die "No raid level configured for" (count disks) "disks"))
    (or (apply = (map :maj disks))
        (die "Can't mix diferent type devices."))
    (or (apply = (map :size disks))
        (die "Can't mix diferent size devices."))
    (let [disks (map :name disks)]
      (l "selected disks: " (pr-str disks))
      (assoc conf :disks disks))))

#_ (defn stop-raid
  "Stop all raid devices."
  []
  #_ (-> "mdstat" slurp-proc println)
  ($ "mdadm" "--stop" "--scan"))

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

(defn list-pools
  []
  (let [pools ($> "zpool" "list" "-o" "name" "-H")]
    (when-not (empty? pools)
      (str/split-lines pools))))

(defn destroy-pools
  []
  (doseq [pool (list-pools)]
    ($ "zpool" "destroy" #_ "-f" pool)))

(defn stop-all
  "Stop all block devices."
  ([]
   (l "stopping block-devices")
   (destroy-pools)
   (stop-all (lsblk [:output-all [:include "3,8"]])))
  ([devices]
   ;; Walk device tree. Stop children first then stop self.
   ;; hd(3), sd(8)
   (when-let [{:keys [path type children]} (first devices)]
     ;; process children
     (stop-all children)
     ;; process self
     ;; Check for existence as device may have already been closed.
     (l "stopping " path)
     (when (exists? path)
       (case type
         "crypt" ($ "cryptsetup" "luksClose" path)
         "raid1" (do
                   ;; Erases any signatures (e.g., swap)
                   (wipe-dev path)
                   ($ "mdadm" "--stop" path)
                   (udev-settle))
         "part"  :ok
         "disk"  :ok
         (die "unknown type" type)))
     ;; process siblings
     (stop-all (rest devices)))))

(defn wipe-disks
  [{:keys [disks] :as conf}]
  (stop-all)
  (doseq [disk disks]
    (let [dev (str "/dev/" disk)
          l  (partial l (str disk " - "))]
      (l "wiping paritions")
      ;; Note: It is important to wipe these right away. Otherwise, if
      ;; the kernel finds raid metadata it will and try and use the
      ;; partition before we get a chance to wipe it.
      (doseq [{:keys [name]} (partitions dev)]
        (wipe-dev (str "/dev/" name)))
      (wipe-dev dev)
      ))
  conf)

;; Note: Run "sgdisk -L" to list partition types
(defn new-part
  [disk size type]
  (let [part-num (-> disk partitions count inc)
        part     (str disk "-part" part-num) ; depends on device type ...
        ;; For first partition,
        ;; - leave 16k space at start of disk for partition table
        ;; - align to 1M boundary for performance
        start (if (= 1 part-num) "1M" "0")
        size  (if (string? size)
                size
                (str size "M"))
        n-arg (str "-n" part-num ":" start ":+" size)
        t-arg (str "-t" part-num ":" type)]
    ($ "sgdisk" n-arg t-arg disk)
    ($> "partprobe")
    (udev-settle)
    (wipe-dev part)
    part))

;; Signature for partition handler functions.
;; [conf] -> [partition ...]
(defmulti create-dev
  (fn [{:keys [chain] :as conf}]
    (first chain)))

(defn create-partitions
  "Creates a partion on each disk in disks according to :part/size and
  :part/type in conf. Returns a list of the partitions created."
  [{:keys [disks] :as conf}]
  (doall
   (map (fn [disk]
          (let [id   (disk-by-id disk)
                size (or (:part/size conf)
                         (:size conf))
                type (:part/type conf)]
            (new-part id (str size "M") type)))
        disks)))

(defn run-handler
  [{:keys [disks chain setup-fn map-fn combine-fn] :as conf}]
  (let [current (first chain)
        chain   (rest chain)
        conf    (assoc conf :chain chain)
        ;; Delegate to the next handler if present, otherwise create
        ;; partitions.
        parts   (if (empty? chain)
                  (do (l current " creating partitions")
                      (create-partitions conf))
                  (create-dev (dissoc conf :setup-fn :map-fn :combine-fn)))]
    ;; Process partitions and return possibly new partitions.
    (cond setup-fn   (do
                       (l current " setting up devices")
                       (doseq [part parts]
                         (setup-fn part))
                       parts)
          combine-fn (do
                       (l current " combining devices")
                       [(combine-fn parts)])
          map-fn     (do
                       (l current " creating devices")
                       (doall
                        (map map-fn parts)))
          :else      parts)))

(defmethod create-dev :esp
  [{:keys [disks] :as conf}]
  (let [conf (assoc conf
                    :part/size 512
                    :part/type "EF00" ; EFI system
                    :setup-fn #($ "mkfs.fat" "-F" "32" "-s" "1" "-n" "EFI" %))]
    (run-handler conf)))

(defmethod create-dev :swap
  [{:keys [disks] :as conf}]
  (let [size (calculate-swap-MiB)
        conf (assoc conf
                    :name "swap"
                    :size size
                    :part/size (/ size (count disks))
                    :part/type "8200" ; Linux Swap
                    :setup-fn #($ "mkswap" %))]
    (run-handler conf)))

(defn raid-disable-recovery
  "Disable RAID recovery. We don't want this to slow down machine provisioning
   in the rescue mode. It can run in normal operation after reboot."
  []
  ($ "sysctl" "-w" "dev.raid.speed_limit_max=0"))

;; :part/combine-fn
(defmethod create-dev :md
  [{:keys [disks name size] :as conf}]
  (raid-disable-recovery)
  (let [f (fn [parts]
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
        conf (assoc conf
                    :part/type "FD00" ; Linux RAID
                    :combine-fn f)]
    (run-handler conf)))

(defmethod create-dev :luks
  [{:keys [size disks] :as conf}]
  (let [f    (fn [part]
               ;; /dev/mapper/NAME must be unique
               (let [key-file "/root/luks.key"
                     name     ($> "basename" part)]
              ($ "cryptsetup" "--batch-mode" "luksFormat" part key-file)
              ($ "cryptsetup" "luksOpen" part name "--key-file" key-file)
              (str "/dev/mapper/" name)))
        conf (assoc conf
                    :part/type "8309" ; Linux LUKS
                    :map-fn f)]
    (run-handler conf)))

;; Ref: https://openzfs.github.io/openzfs-docs/Getting%20Started/Ubuntu/Ubuntu%2020.04%20Root%20on%20ZFS.html
(def zfs-bpool-options
  ["-o" "ashift=12"
   "-o" "feature@async_destroy=enabled"
   "-o" "feature@bookmarks=enabled"
   "-o" "feature@embedded_data=enabled"
   "-o" "feature@empty_bpobj=enabled"
   "-o" "feature@enabled_txg=enabled"
   "-o" "feature@extensible_dataset=enabled"
   "-o" "feature@filesystem_limits=enabled"
   "-o" "feature@hole_birth=enabled"
   "-o" "feature@large_blocks=enabled"
   "-o" "feature@lz4_compress=enabled"
   "-o" "feature@spacemap_histogram=enabled"
   "-o" "feature@zpool_checkpoint=enabled"
   "-O" "acltype=posixacl"
   "-O" "canmount=off"
   "-O" "compression=lz4"
   "-O" "devices=off"
   "-O" "normalization=formD"
   "-O" "relatime=on"
   "-O" "xattr=sa"
   ])

(defmethod create-dev :zfs/boot
  [conf]
  (let [f (fn [parts]
            ($ "zpool" "create"
               zfs-bpool-options
               "-O" "mountpoint=/boot"
               "-R" "/mnt"
               "bpool" "mirror"
               parts)
            "bpool")
        conf (assoc conf
                    :part/size 500
                    :part/type "BE00" ; Solaris boot
                    :combine-fn f)]
    (run-handler conf)))


;; Ref: https://openzfs.github.io/openzfs-docs/Getting%20Started/Ubuntu/Ubuntu%2020.04%20Root%20on%20ZFS.html
(def zfs-rpool-options
  ["-o" "ashift=12"
   "-O" "acltype=posixacl"
   "-O" "canmount=off"
   "-O" "compression=lz4"
   "-O" "dnodesize=auto"
   "-O" "normalization=formD"
   "-O" "relatime=on"
   "-O" "xattr=sa"])

(defmethod create-dev :zfs/root
  [conf]
  (let [f (fn [parts]
            ($ "zpool" "create"
               zfs-rpool-options
               "-O" "mountpoint=/"
               "-R" "/mnt"
               "rpool" "mirror"
               parts)
            "rpool")
        conf (assoc conf
                    :part/size 0      ; all available space
                    :part/type "BF00" ; Solaris root
                    :combine-fn f)]
    (run-handler conf)))

#_ (defn create-zfs
  [conf chain]
  (pprint conf)
  )

(defn create-block-devices
  [{:keys [partitions] :as conf}]
  (doall
   (map (fn [chain]
          (println "==================================================")
          (println "PARITIONING " (pr-str chain))
          (println "==================================================")
          (create-dev (assoc conf :chain chain))) partitions)))

(defn -main
  []
  (-> {:disk-types #{:nvme}
       :partitions [
                    [:esp]
                    [:swap :luks :md]
                    [:zfs/boot]
                    [:zfs/root :luks]
                    ]}
      select-disks
      wipe-disks
      create-block-devices
      #_ pprint))

;; put in protection to not raid if only 1 device.
;; error - partprobe is error when messing with the usbstick WTF
;;   - happens after running nvme partitioning
;; add profile for data pool

;; partition mode
;; - equal size devices 1,2,4
;; - unequal size e.g., nvme + hdd
;;   - put everything on fastest drive (what is min size)
;;   - setup other drive for data/backup

;; Delete all partitions.
;; ($ "partx" "--delete" dev)
;;
;; Destroy partition table.
;; ($ "sgdisk" "--zap-all" dev)
;;   note: doesn't work if there are md partitiions, prefer wipefs
;;
;; Tell kernel to re-read partition table(s)
;; ($ "blockdev" "--rereadpt" dev)
;; also, partprobe
;; Various "in use" things can block the kernel from re-reading the new partition table:
;;   See: https://serverfault.com/questions/36038/reread-partition-table-without-rebooting
;;   e.g., cat /proc/mdstat
;;
;; https://www.abstractpath.com/2013/mounting-linux-software-raid-with-a-consistent-device-name/
;; for raid homehost, use <none> when creating
;; and <ignore> in mdadm.conf
