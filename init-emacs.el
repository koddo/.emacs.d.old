;; (server-start)

;; -------------------------------------------------------------------

(setq user-full-name "Alexander Scherbanov")
(setq user-mail-address "alexander.scherbanov@gmail.com")

;; -------------------------------------------------------------------

(setq debug-on-error t)
(tool-bar-mode -1)   ; menu-bar-mode moved to preinit
(setq inhibit-startup-message t)
(setq initial-scratch-message
      ";; scratch buffer\n\n"
      )
(setq frame-title-format "emacs")
(setq debug-on-error t)
(setq visible-bell t)
(setq calendar-week-start-day 1)     ;; week starts from Monday
(setq truncate-partial-width-windows nil)

(line-number-mode t)
(column-number-mode t)

(setq scroll-error-top-bottom t)  ;; point moves to the beginning or the end of the buffer (depending on scrolling direction) when no more scrolling possible
(setq next-line-add-newlines t)
;; maybe someday try to fix hl-line-mode at the end of buffer: https://emacs.stackexchange.com/questions/24311/eval-buffer-works-init-does-not-hl-line-fix

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

(setq delete-by-moving-to-trash t)

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
;; (set-face-attribute 'diredp-symlink nil :foreground "Blue")   ; trash-directory is set in preinit.this_machine.el

(require 'wdired)
(setq wdired-confirm-overwrite t)


;; -------------------------------------------------------------------


(recentf-mode 1)
(setq recentf-max-saved-items 200)


;; -------------------------------------------------------------------
;; auto save buffers

(defun ym/save-buffer-silently ()
  (interactive)
  (cl-flet ((message (format &rest args)
		     nil))
    (save-buffer)))

(defun ym/save-some-buffers-silently ()
  (interactive)
  (cl-flet ((message (format &rest args)
		     nil))
    (save-some-buffers t)))

(add-hook 'focus-out-hook 'ym/save-some-buffers-silently)   ; https://emacs.stackexchange.com/questions/265/how-to-auto-save-buffers-when-emacs-loses-focus

(defadvice switch-to-buffer (before ym/save-buffer-silently activate)
  (when buffer-file-name (ym/save-buffer-silently)))
(defadvice other-window (before other-window-now activate)
  (when buffer-file-name (ym/save-buffer-silently)))
(defadvice windmove-up (before other-window-now activate)
  (when buffer-file-name (ym/save-buffer-silently)))
(defadvice windmove-down (before other-window-now activate)
  (when buffer-file-name (ym/save-buffer-silently)))
(defadvice windmove-left (before other-window-now activate)
  (when buffer-file-name (ym/save-buffer-silently)))
(defadvice windmove-right (before other-window-now activate)
  (when buffer-file-name (ym/save-buffer-silently)))

;; -------------------------------------------------------------------

;; isearch extension that shows number of matches and current match index
(use-package anzu
  :config
  (setq anzu-search-threshold 1000)
  (global-anzu-mode +1)
  )

;; -------------------------------------------------------------------

;; https://stackoverflow.com/questions/2081577/setting-emacs-to-split-buffers-side-by-side
(setq split-height-threshold 80)
(setq split-width-threshold 160)

;; -------------------------------------------------------------------

;; https://www.emacswiki.org/emacs/CleanBufferList
(require 'midnight)

;; -------------------------------------------------------------------

;; (global-auto-revert-mode)
;; (revert-without-query '())
;; (use-package revbufs
;;   )

;; magit-ionotify-mode ?

;; (auto-revert-verbose t)
;; maybe also see this: https://www.gnu.org/software/emacs/manual/html_node/emacs/Interlocking.html
;; (defun ask-user-about-supersession-threat (fn)
;;   "blatantly ignore files that changed on disk"
;;   )
;; (defun ask-user-about-lock (file opponent)
;;   "always grab lock"
;;   t
;;   )

;; -------------------------------------------------------------------
