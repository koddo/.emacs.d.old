;;(set-face-attribute 'default nil :family "Monospace" :height 80)
;;(set-face-attribute 'default nil :family "JetBrains Mono" :height 80)
(set-face-attribute 'default nil :family "Monaco" :height 75)


(menu-bar-mode -1)

(setq org-download-screenshot-method "xfce4-screenshooter --region --save %s"   ; "gnome-screenshot -a -f %s" ; for macos: "screencapture -i %s"
      org-download-edit-cmd "open -a Krita %s"   ; TODO: move to preinit
      org-download-backend "wget \"%s\" -O \"%s\"")












