(set-face-attribute 'default nil :family "Monaco" :height 100)

(menu-bar-mode -1)

(setq
 org-download-screenshot-method "screencapture -i %s"
 org-download-edit-cmd "open -a Krita %s"
 org-download-backend "wget \"%s\" -O \"%s\"")
