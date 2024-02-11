;; -*- lexical-binding: t; -*-

(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)

;; -------------------------------------------------------------------

(server-start)

;; -------------------------------------------------------------------

(set-frame-name "emacs")
;; frame-title-format

;; (make-frame '((name . "main")))

;; -------------------------------------------------------------------

(use-package diminish)

;; to rename minor modes see https://github.com/myrjola/diminish.el
;; to diminish a major mode, (setq mode-name "whatever") in the mode hook
;; e.g., (add-hook 'lisp-mode-hook (lambda () (setq mode-name "λ")))

(setq eldoc-echo-area-use-multiline-p nil)
(diminish 'eldoc-mode)

;; maybe try https://github.com/radian-software/blackout instead

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
;; maybe someday try to fix hl-line-mode at the end of buffer: https://emacs.stackexchange.com/questions/24311/eval-buffer-works-init-does-not-hl-line-fix

;; for warning-suppress-types later in init
(require 'warnings)

(setq use-dialog-box nil)

;; -------------------------------------------------------------------

;; (add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(set-scroll-bar-mode 'right)
(setq scroll-preserve-screen-position t)

(setq auto-window-vscroll nil)   ; ?



(setq mouse-wheel-scroll-amount '(1))




;; also https://www.emacswiki.org/emacs/SmoothScrolling


;; -------------------------------------------------------------------

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(require 'whitespace)
(setq whitespace-style
      ;; '(tabs tab-mark space-mark)
      '(face tabs spaces trailing lines space-before-tab newline indentation empty space-after-tab space-mark tab-mark newline-mark missing-newline-at-eof)
      )
;; toggle with M-x global-whitespace-mode	

;; -------------------------------------------------------------------

(require 'epa-file)   ; gnupg, gpg
(setq epa-file-inhibit-auto-save t)   ; it's on by default, but to be sure


;; -------------------------------------------------------------------

(setq delete-by-moving-to-trash t)

;; -------------------------------------------------------------------

(use-package rx)

;; -------------------------------------------------------------------

(use-package dired+
  :config
  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(dired-omit-extensions '() nil nil "Customized by me")
   '(dired-omit-files (rx (or
                           (seq bol "." eol) 
                           (seq bol "." (not (any "."))) 
                           ))
                      nil nil "Customized by me"))
  (add-hook 'dired-mode-hook 'dired-omit-mode)
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)

  (add-hook 'dired-mode-hook 'auto-revert-mode)   ; watch filesystem for changes

  (defun ym/dired-toggle-hide-and-omit ()
  (interactive)
  (if dired-omit-mode
      (progn (dired-omit-mode -1)
             (dired-hide-details-mode -1))
    (dired-omit-mode 1)
    (dired-hide-details-mode 1)))
  (define-key dired-mode-map "(" 'ym/dired-toggle-hide-and-omit)

  ;; (require 'ls-lisp) ;; ignore case when listing directory
  ;; (setq ls-lisp-ignore-case t)
  ;; (setq ls-lisp-use-insert-directory-program nil)
  ;; (setq ls-lisp-use-string-collate nil)

 )

(require 'wdired)
(setq wdired-confirm-overwrite t)

(require   'uniquify)  ; buffer names are uniquified with parts of directory name, for ex.: name|folder
(setq       uniquify-buffer-name-style 'reverse)

;; -------------------------------------------------------------------


(recentf-mode 1)
(setq recentf-max-saved-items 200)

(save-place-mode 1)

(setq history-length 25)
(savehist-mode 1)

;; -------------------------------------------------------------------
;; auto-save, backup, and lockfiles

;; disable the old and built-in auto-save-mode that creates a lot of junk #files#
(setq auto-save-default nil)
(setq auto-save-list-file-name nil)
(setq auto-save-list-file-prefix nil)
(setq auto-save-no-message t)   ; not sure if it has effect in other modes, but leaving it

(use-package super-save
  :diminish super-save-mode
  :config

  (setq auto-save-visited-interval 5)
  (auto-save-visited-mode 1)   ; it's a built-in mode, I found the super-save-auto-save-when-idle less usable, because it polluted messages
  ;; or, for fine-grained control: https://github.com/ChillarAnand/real-auto-save
  ;; it allows the following: (add-hook 'org-mode-hook 'real-auto-save-mode)
    
  (setq super-save-remote-files nil)
  (setq super-save-exclude '(".gpg"))
  ;; (setq super-save-idle-duration 5)    ; this is default, but it's not used anyway, see auto-save-visited-mode

  (setq save-silently t)
  (super-save-mode +1)
)

(setq make-backup-files nil)

;; editing files by different instances almost never happens in my case
(setq create-lockfiles nil)

;; -------------------------------------------------------------------

;; isearch extension that shows number of matches and current match index
(use-package anzu
  :diminish anzu-mode
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

(global-auto-revert-mode 1)
(setq auto-revert-interval 1)
(setq auto-revert-use-notify t)   ; t is the default
;; (setq auto-revert-remote-files t)   ; nil is the default -- this is for tramp, remote files


;; According to EmacsWiki(https://www.emacswiki.org/emacs/AutoRevertMode), binding revert-buffer-function to a function discarding changes and then reverting might work.

;; You should try:

;; (setq revert-buffer-function 'inform-revert-modified-file)

;; If you use use-package, you can use something like that:

;; (use-package files
;;   :ensure nil
;;   :custom (revert-buffer-function 'inform-revert-modified-file))



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

(setq-default truncate-lines t)
(global-visual-line-mode -1)
;; in case if you're confused about the word-wrap variable, here's a clarification: Instead of setting this variable directly, most users should use Visual Line mode.

;; toggle visual-line-mode per buffer via hydra
;; some modes turn it on in hooks, like org-mode, because it's mostly text, not code


;; -------------------------------------------------------------------

(setq      comint-buffer-maximum-size 5000) ; truncate the shell buffer
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer) ; to a certain maximum number of lines

;; -------------------------------------------------------------------

(put 'downcase-region 'disabled nil)
(put   'upcase-region 'disabled nil)

;; -------------------------------------------------------------------

(setq require-final-newline t)   ; t means when saving; other options are 'visit, 'visit-save, 'ask, nil
(setq next-line-add-newlines t)       ; TODO: make it add a new line so there's always a newline after cursor

;; -------------------------------------------------------------------

;; tramp

;; TODO: let it reuse ssh connections
;; https://stackoverflow.com/questions/56105716/magit-over-tramp-re-use-ssh-connection
;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Frequently-Asked-Questions.html#Frequently-Asked-Questions
;; TRAMP does not use default ssh ControlPath

;; -------------------------------------------------------------------

(setq ediff-split-window-function 'split-window-horizontally)


;; -------------------------------------------------------------------





;; -------------------------------------------------------------------


;; -------------------------------------------------------------------

