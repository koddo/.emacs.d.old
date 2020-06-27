;;; -*- lexical-binding: t; -*-

;; (use-package maxframe
;;   :init
;;   ;; (add-hook 'window-setup-hook 'maximize-frame t)
;;   )


;; -------------------------------------------------------------------


(use-package diminish)
;; to rename minor modes see https://github.com/myrjola/diminish.el
;; to diminish a major mode, (setq mode-name "whatever") in the mode hook
;; e.g., (add-hook 'lisp-mode-hook (lambda () (setq mode-name "λ")))


;; -------------------------------------------------------------------


;; functions of interest: undo-tree-visualize
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode)
  )


;; -------------------------------------------------------------------


(use-package emojify
  :config
   ;; (remove-hook 'emojify-inhibit-functions #'emojify-in-org-tags-p)
  )


;; -------------------------------------------------------------------


;; https://github.com/emacsorphanage/popwin
(use-package popwin
  :config
  (popwin-mode 1))


;; -------------------------------------------------------------------


(use-package ido
  :config
  (setq
   ido-virtual-buffers t   ; keep a list of closed buffers
   ido-enable-flex-matching t
   ido-use-faces nil   ; turned off for flx
   ido-default-buffer-method 'selected-window    ; do not switch frames if a buffer is opened -- http://ergoemacs.org/misc/emacs_ido_switch_window.html
   ido-auto-merge-work-directories-length -1     ; disable search for a file in other recent used directories -- https://stackoverflow.com/questions/17986194/emacs-disable-automatic-file-search-in-ido-mode
   )
  (ido-mode 1)
  (ido-everywhere 1)
  )
(use-package flx-ido
  :config
  (flx-ido-mode 1)
  ;; (setq flx-ido-threshold 10000)
  )
(use-package ido-grid-mode
  :config
  ;; (setq ido-grid-mode-start-collapsed t)
  )
;; C-h f, while Amx is active, runs describe-function on the currently selected command.
;; M-. jumps to the definition of the selected command.
;; C-h w shows the key bindings for the selected command. (Via where-is.)
;; there is also amx-major-mode-commands
(use-package amx   ; smex successor, for M-x
  :config
  (setq
   amx-ignored-command-matchers nil
   ido-cr+-max-items 50000   ; default is 30000
   amx-show-key-bindings nil
   ;; and set amx-save-file to do-not-litter
   )
  (amx-mode 1)
  (add-to-list 'warning-suppress-types '(amx))
  )
(use-package ido-completing-read+   ; enhanced ido-everywhere
  :config
  (ido-ubiquitous-mode 1)
  )
(use-package ido-yes-or-no
  :config
  (ido-yes-or-no-mode)
  )
;; (add-to-list 'ido-ignore-buffers "\\` ")
(add-to-list 'ido-ignore-buffers "\\*Ido Completions\\*")
(add-to-list 'ido-ignore-buffers "\\*Ibuffer\\*")
(add-to-list 'ido-ignore-buffers "\\*Ibuffer\\*")
(add-to-list 'ido-ignore-buffers "\\*Messages\\*")
(add-to-list 'ido-ignore-buffers "\\*Completions\\*")
(add-to-list 'ido-ignore-buffers "\\*Scratch\\*")
(add-to-list 'ido-ignore-buffers "\\*Help\\*")
(setq ido-use-filename-at-point 'guess)   ;; nil, guess and t for literal filename
(setq ido-use-url-at-point t)
(setq ido-file-extensions-order '(".org" ".md"))
;; (setq ido-enter-matching-directory nil)
;; (setq ido-show-dot-for-dired t)
;; (setq ido-enable-tramp-completion nil)
;; (setq ido-max-prospects 7)

(use-package ido-sort-mtime
  :config
  (setq
   ido-sort-mtime-limit 2000
   ;; ido-sort-mtime-tramp-files-at-end nil
   )
  )

;; -------------------------------------------------------------------


;; (use-package ag)
;; (use-package projectile-ripgrep)
(use-package deadgrep)
;; (use-package emacs-wgrep)   ;; for editing grep buffer; deadgrep support -- https://github.com/mhayashi1120/Emacs-wgrep/pull/58

(use-package projectile
  :config
  (projectile-mode +1)
  (setq frame-title-format     ;; taken from https://emacs.stackexchange.com/questions/35432/how-to-set-projectile-project-name-as-frame-title
        '(""
          "%b"
          (:eval
           (let ((project-name (projectile-project-name)))
             (unless (string= "-" project-name)
               (format " in [%s]" project-name))))))
  )


;; -------------------------------------------------------------------


(use-package treemacs)
(use-package treemacs-projectile
  :after treemacs projectile)
(use-package treemacs-magit
  :after treemacs magit)
(use-package lsp-treemacs)

;; -------------------------------------------------------------------



;; TODO: https://alhassy.github.io/org-special-block-extras/



;; -------------------------------------------------------------------



;; https://github.com/emacsorphanage/helm-ag









