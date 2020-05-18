;;; -*- lexical-binding: t; -*-

(use-package diminish)

;; functions of interest: undo-tree-visualize
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode)
  )

;; (use-package emojify
;;   :config
;;    ;; (remove-hook 'emojify-inhibit-functions #'emojify-in-org-tags-p)
;;   )


