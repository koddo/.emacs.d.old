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

;; for warning-suppress-types later in init
(require 'warnings)


;; -------------------------------------------------------------------


(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)   ;; chmod u+x current file when saving if not set

(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)


;; -------------------------------------------------------------------

(require 'epa-file)   ; gnupg, gpg
(setq epa-file-inhibit-auto-save t)   ; it's on by default, but to be sure


;; -------------------------------------------------------------------


(add-hook 'dired-mode-hook 'auto-revert-mode)   ; watch filesystem for changes
(use-package dired+)
(add-hook 'dired-load-hook '(lambda () (require 'dired-x)))
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode)))
(defvar ym-dired-omit-files-system-specific "")  ; see preinit_win.el -- to hide RECYCLER folder
(setq dired-omit-files (concat "^\\.[^.].*$\\|^\\.$\\|^#.*$" ym-dired-omit-files-system-specific))        ;; omit ".files" and ".", but show ".."
;; (setq dired-omit-extensions '())
;; (defun ym-add-to-list-dired-omit-extensions (extensions-list)
;;   (mapc (lambda (ext) (add-to-list 'dired-omit-extensions ext))
;;         extensions-list))
;; (require 'dired-details)
;; (dired-details-install)
;; (setq dired-details-hidden-string "")
;; (setq dired-details-hide-link-targets nil)
;; (require 'ls-lisp) ;; ignore case when listing directory
;; (setq ls-lisp-ignore-case t)
;; (setq ls-lisp-use-insert-directory-program nil)
;; (setq ls-lisp-use-string-collate nil)
;; (setq delete-by-moving-to-trash t)
;; (set-face-attribute 'diredp-symlink nil :foreground "Blue")   ; trash-directory is set in preinit.this_machine.el

(require 'wdired)
(setq wdired-confirm-overwrite t)


;; -------------------------------------------------------------------


(recentf-mode 1)
(setq recentf-max-saved-items 200)


;; -------------------------------------------------------------------
