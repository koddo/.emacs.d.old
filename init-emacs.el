;; (server-start)

;; -------------------------------------------------------------------

(setq user-full-name "Alex Scherbanov")
(setq user-mail-address "alex@egotv.ru")

;; -------------------------------------------------------------------

(setq debug-on-error t)
(tool-bar-mode -1)   ; menu-bar-mode moved to preinit
(setq inhibit-startup-message t)
(setq initial-scratch-message ";; scratch buffer")
(setq frame-title-format "emacs")
(setq debug-on-error t)
(setq visible-bell t)
(setq calendar-week-start-day 1)     ;; week starts from Monday

(line-number-mode t)
(column-number-mode t)

;; -------------------------------------------------------------------

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)   ;; chmod u+x current file when saving if not set


(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

;; -------------------------------------------------------------------
