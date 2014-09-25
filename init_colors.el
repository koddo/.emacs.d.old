
;; -------------------------------------------------------------------
(global-hl-line-mode 1)
(setq ym-hl-line-color-normal-mode-color "LightSteelBlue1")
(set-face-background 'hl-line ym-hl-line-color-normal-mode-color)
;; -------------------------------------------------------------------
(set-face-background 'show-paren-match-face "#fcc")
(set-face-foreground 'show-paren-match-face "#f00")
(set-face-attribute 'show-paren-match-face nil :weight 'extra-bold)
;; -------------------------------------------------------------------
(set-face-background 'mode-line "gray60")
(setq ym-mode-line-directory-font '(:foreground "grey30"))
(setq ym-mode-line-file-name-font '(:weight bold))
;; -------------------------------------------------------------------
(defface ym-org-agenda-deadline-face                '((t :foreground "DarkMagenta")) "")
(defface ym-org-agenda-dim-done-face                '((t :foreground "grey80" :weight bold)) "")
(defface ym-org-agenda-dim-undone-face              '((t :foreground "grey93" :weight normal)) "")
(defface ym-org-agenda-current-time-string-face     '((t :background "grey90")) "")
(defface ym-org-agenda-tag-birthday-face            '((t :foreground "red")) "")
(defface ym-org-agenda-tag-sport-face               '((t :foreground "orange")) "")
(defface ym-org-clock-summary-total-time-line-highlight '((t :foreground "red")) "")
(setq ym-org-agenda-tags-faces '((":birthday:" .  '(:foreground "red" :underline nil))
                                 (":sport:" . '(:foreground "orange" :underline nil))
                                 (":hide:" . '(:foreground "grey" :underline nil))
                                 (":redtag:" . '(:foreground "red" :underline nil))
                                 (":health:" . '(:foreground "orange" :underline nil))))
(setq org-agenda-deadline-faces '((1.0 . org-warning)
                                  (0.0 . org-scheduled)))
                                  ;; (0.0 . ym-org-agenda-deadline-face)))
(custom-set-faces
 '(org-agenda-clocking ((t (:underline "Red"))) t)   ; clocked in task was highlighted with face inherited from secondary-selection-face with yellow background
 '(org-scheduled-previously ((t (:foreground "DarkGreen"))) t)   ; was Firebrick
 '(org-mode-line-clock ((t (:weight bold))) t)
 '(org-mode-line-clock-overrun ((t (:weight bold :background "yellow"))) t))
(setq org-todo-keyword-faces '(("WAITING" . "DarkOrange")
                               ("REGULARLY" . "ForestGreen")   ; was LimeGreen
                               ("TODO" . "ForestGreen")
                               ("NEXT" . "ForestGreen")))
(set-face-foreground 'org-special-keyword "gray70")   ; for :LOGBOOK: drawer
(setq ym-org-agenda-dim-top-tag-regexp ":top:")
(defface ym-org-agenda-dim-top-tag-face '((t :foreground "grey80")) "")
(custom-set-faces
 '(org-habit-clear-face                 ((t (:background "grey95"))) t)
 '(org-habit-clear-future-face          ((t (:background "grey95"))) t)
 '(org-habit-ready-face                 ((t (:background "LightSteelBlue2"))) t)
 '(org-habit-ready-future-face          ((t (:background "LightSteelBlue2"))) t)
 '(org-habit-alert-face                 ((t (:background "LightSteelBlue2"))) t)
 '(org-habit-alert-future-face          ((t (:background "LightSteelBlue2"))) t)
 '(org-habit-overdue-face               ((t (:background "grey95"))) t)
 '(org-habit-overdue-future-face        ((t (:background "grey95"))) t))
;; -------------------------------------------------------------------
(add-hook 'org-agenda-finalize-hook (lambda ()
                                      (beginning-of-buffer)
                                      (while (search-forward-regexp ym-org-todo-keywords-working-regexp nil t)
                                        (overlay-put
                                         (make-overlay
                                          (- (line-beginning-position) 1)
                                          (line-end-position))
                                         'invisible t))
                                      (beginning-of-buffer)
                                      (flet ((line-matches-p (regex)
                                                             (let ((case-fold-search nil))
                                                               (string-match-p regex
                                                                               (buffer-substring-no-properties
                                                                                (line-beginning-position)
                                                                                (line-end-position)))))
                                             (color-tag (cons-cell)
                                                        (let ((tag    (car cons-cell))
                                                              (color  (cdr cons-cell)))
                                                          (while (search-forward-regexp tag nil t)
                                                          (when (not (line-matches-p ym-org-todo-state-string-in-log))
                                                            (ym-add-overlay color
                                                                            (- (point) (length tag))
                                                                            (point))))
                                                        (beginning-of-buffer))))
                                        (mapcar 'color-tag ym-org-agenda-tags-faces))
                                      (beginning-of-buffer)
                                      (while (search-forward-regexp ym-org-todo-keywords-done-regexp nil t)
                                        (ym-add-overlay-to-line 'ym-org-agenda-dim-done-face))
                                      (beginning-of-buffer)
                                      (while (search-forward-regexp ym-org-agenda-dim-top-tag-regexp nil t)
                                        (ym-add-overlay-to-line 'ym-org-agenda-dim-top-tag-face))
                                      (beginning-of-buffer)
                                      (while (search-forward-regexp ym-org-todo-keywords-undone-regexp nil t)
                                        (ym-add-overlay-to-line 'ym-org-agenda-dim-undone-face))
                                      (beginning-of-buffer)
                                      (progn (search-forward org-agenda-current-time-string nil t)
                                             (ym-add-overlay-to-line 'ym-org-agenda-current-time-string-face))
                                      (when (numberp org-agenda-current-span)
                                        (beginning-of-buffer)
                                        (forward-line)
                                        (while (search-forward-regexp "^[^ ]" nil t)
                                          (beginning-of-line)
                                          (insert "================================================================================\n")
                                          (forward-line))
                                        (beginning-of-buffer)
                                        (while (search-forward ":hide:" nil t)
                                          (ym-org-agenda-hide-line-temporarily t)))
                                      (beginning-of-buffer)
                                      (while (search-forward-regexp "^=+$" nil t)   ; add a newline before ==================
                                        (beginning-of-line) ; (forward-line -1)
                                        (insert "\n")
                                        (forward-line 2)
                                        )
                                      (remove-text-properties   ; stop the mouse cursor from highlighting lines in the agenda
                                       (point-min) (point-max) '(mouse-face t))
                                      ))     ;; (setq org-agenda-finalize-hook nil)
(defun ym-add-overlay-to-line (face)
  (overlay-put
   (make-overlay
    (line-beginning-position)
    (line-end-position))
   'face face))
(defun ym-add-overlay (face startpoint endpoint)
  (overlay-put
   (make-overlay
    startpoint
    endpoint)
   'face face
   ))
;; for debugging overlays
(defun ym-list-overlays-at (&optional pos)
  "Describe overlays at POS or point."
  (interactive)
  (setq pos (or pos (point)))
  (let ((overlays (overlays-at pos))
        (obuf (current-buffer))
        (buf (get-buffer-create "*Overlays*"))
        (props '(priority window category face mouse-face display
                          help-echo modification-hooks insert-in-front-hooks
                          insert-behind-hooks invisible intangible
                          isearch-open-invisible isearch-open-invisible-temporary
                          before-string after-string evaporate local-map keymap
                          field))
        start end text)
    (if (not overlays)
        (message "None.")
      (set-buffer buf)
      (erase-buffer)
      (dolist (o overlays)
        (setq start (overlay-start o)
              end (overlay-end o)
              text (with-current-buffer obuf
                     (buffer-substring start end)))
        (when (> (- end start) 13)
          (setq text (concat (substring text 1 10) "...")))
        (insert (format "From %d to %d: \"%s\":\n" start end text))
        (dolist (p props)
          (when (overlay-get o p)
            (insert (format " %15S: %S\n" p (overlay-get o p))))))
      (pop-to-buffer buf))))
(defun ym-list-text-properties-at-point (&optional pos)
  (interactive)
  (setq pos (or pos (point)))
  (let ((properties (text-properties-at pos)))
    (message "%S" properties)
    )
  )

;; -------------------------------------------------------------------
;; (defface ym-diredp-org-extension '((t :foreground "red")) "My face for org-mode files in dired+.")
;; (setq ym-diredp-org-extension 'ym-diredp-org-extension)
;; (add-to-list 'diredp-font-lock-keywords-1
;;              (list "^  \\(.*\\(\\.org[*]?\\)\\)$" 2 'ym-diredp-org-extension t))   ;; (pop diredp-font-lock-keywords-1)
;; -------------------------------------------------------------------




 





