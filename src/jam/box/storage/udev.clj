(ns jam.box.storage.udev
  (:require [jam.sh :refer [$>]]))

(defn settle
  "Wait for udev to update /dev symlinks."
  []
  ($> "udevadm" "settle"))
