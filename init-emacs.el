(transient-mark-mode 1)   ; No region when it is not highlighted
(delete-selection-mode 1)
(setq shift-select-mode 1)   ; explicitly stating the default

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq text-scale-mode-step 1.1)
