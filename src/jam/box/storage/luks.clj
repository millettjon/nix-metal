(ns jam.box.storage.luks
  (:require [jam.sh :refer [$ $>]]
            [jam.path :refer [exists?]]
            [jam.box.storage.disk :refer [parse-node create-device]]
            [jam.log :refer [info die]]))

(defn luks-create
  [file]
  (when-not (exists? file)
    (info ":luks creating key" file)
    ($ "dd" "bs=512" "count=4" "if=/dev/random" (str "of=" file) "iflag=fullblock"))
  file)

(defmethod create-device :luks
  [node opts]
  (let [[_node-opts child] (parse-node node)
        opts  (assoc opts :type "8309") ; Linux LUKS
        parts (create-device child opts)]
    (doall (map (fn [part]
                  ;; /dev/mapper/NAME must be unique
                  (let [key-file (luks-create "/root/luks.key")
                        name     ($> "basename" part)]
                    ($ "cryptsetup" "--batch-mode" "luksFormat" part key-file)
                    ($ "cryptsetup" "luksOpen" part name "--key-file" key-file)
                    ;; Return the mapped device.
                    (str "/dev/mapper/" name)))
                parts))))
