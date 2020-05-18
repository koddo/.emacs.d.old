;; (server-start)


(transient-mark-mode 1)      ; no region when it is not highlighted
(delete-selection-mode 1)    ; typed text replaces the selection if the selection is active
(setq shift-select-mode 1)   ; shifted motion keys activate the mark momentarily
;; to copy/cut a region and leave it highlighted we use a wrapper to set deactivate-mark to nil, see keybindings


(add-hook 'before-save-hook 'delete-trailing-whitespace)


(setq text-scale-mode-step 1.1)
