;;; -*- lexical-binding: t; -*-

(use-package maxframe
  :init
  ;; (add-hook 'window-setup-hook 'maximize-frame t)
  )



(use-package diminish)
;; to rename minor modes see https://github.com/myrjola/diminish.el
;; to diminish a major mode, (setq mode-name "whatever") in the mode hook
;; e.g., (add-hook 'lisp-mode-hook (lambda () (setq mode-name "λ")))



;; functions of interest: undo-tree-visualize
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode)
  )



(use-package emojify
  :config
   ;; (remove-hook 'emojify-inhibit-functions #'emojify-in-org-tags-p)
  )




