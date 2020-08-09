(ns jam.metal.vm
  (:require [clojure.java.io :as io]
            [jam.sh :refer [$]]))

(def rescue-iso
  (let [version "6.1.7"
        name    (str "systemrescuecd-amd64-" version ".iso")]
    {:version version
     :name    name
     :url     (str "https://osdn.net/projects/systemrescuecd/storage/releases/" version "/" name)
     :sha256  (str "https://www.system-rescue-cd.org/releases/" version "/" name ".sha256")}))

(defn curl
  [path url]
  ($ "curl" "--output" (str path) "--location" url))

(defn ensure-downloaded
  [{:keys [name url sha256] :as _resource}]
  (let [path (io/file ".cache" name)]
    (when-not (.exists path)
      (curl path url)
      (let [sha256-path (io/file ".cache" (str name ".sha256"))]
        (curl sha256-path sha256)
        ($ "sha256sum" "--check" sha256-path {:directory ".cache"})))))

(defn create-image
  [path {:keys [size] :as _opts}]
  ($ "qemu-img" "create" path size))

(defn create-images
  "Create 2 10G image files."
  []
  #_ (prn "foo" (range 0 2))
  (doseq [i (range 1 3)]
    (let [path (io/file ".cache" (str "image" i ".img"))]
      (when-not (.exists path)
        #_ (println "Creating disk image" (str path))
        (create-image path {:size "10G"})))))

(defn luks-create-key
  "Create luks key file."
  [path]
  (when-not (.exists path)
    (println "Creating luks key.")
    ($ "dd" "bs=512" "count=4" "if=/dev/urandom" (str "of=" path) "iflag=fullblock")))

(defn start
  [iso]
  ($ "qemu-system-x86_64"
     "-nographic"
     ;;  -monitor stdio  # start monitor on stdio
     ;;   -curses
     ;;   -serial pty
     "-enable-kvm"
     "-smp" "2"
     "-m" "1024"
     "-cdrom" iso
     "-hda" ".cache/image1.img"
     "-hdb" ".cache/image2.img"
     ;;   -net none              # make sure EFI shell doesn't attempt netboot
     "-net" "nic"
     "-net" "user,hostfwd=tcp:127.0.0.1:2222-:22"
     ;; -redir tcp:2222::22
     ;; -append "console=ttyS0,115200 text rootpass=123"
     ;; -S     # don't start VM (must press c in monitor)
     "-bios" #_"/usr/share/qemu/OVMF.fd"   ; boot in UEFI mode (requires apt-get install ovmf) (nix-env -iA nixos.OVMF)
     "/nix/store/v6im686ig1p167rmvf8j93ns5mjpgmvz-OVMF-201905-fd/FV/OVMF.fd"))

(defn -main
  []
  (ensure-downloaded rescue-iso)
  (create-images)
  (luks-create-key (io/file ".cache" "luks.key"))
  (start (str ".cache/" (:name rescue-iso))))

;; To connect, not working on nixos
;; ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null root@127.0.0.1 -p2222
