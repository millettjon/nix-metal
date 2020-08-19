(ns jam.box.storage.util
  (:require [clojure.string :as str]
            [cheshire.core :as json]
            [jam.sh :refer [$> kw->opt]]
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
