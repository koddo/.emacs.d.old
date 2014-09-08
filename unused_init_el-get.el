(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)

(setq ym-el-get-packages
      '(   ;;diminish
        autopair
	ledger-mode
        ))

;; TODO: monitor changes on security and upgrade this script if needed
(setq el-get-allow-insecure nil)

(el-get 'sync ym-el-get-packages)
