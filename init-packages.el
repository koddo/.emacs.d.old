;;; -*- lexical-binding: t; -*-




;; -------------------------------------------------------------------


;; -------------------------------------------------------------------


;; -------------------------------------------------------------------







;; (use-package perspective)
;; (use-package persp-mode)
;; (use-package persp-projectile)

;; (ym-define-key (kbd "s-p") 'projectile-command-map)


;; -------------------------------------------------------------------


;; (use-package treemacs
;;   :config
;;   (setq treemacs-no-png-images t)
;;   )
;; (use-package treemacs-projectile
;;   :after treemacs projectile)
;; (use-package treemacs-magit
;;   :after treemacs magit)

;; (use-package all-the-icons
;;   :if (display-graphic-p))
;; (use-package treemacs-all-the-icons
;;   :after treemacs
;;   )
;; (treemacs-load-theme "all-the-icons")
;; ;; (use-package lsp-treemacs)

;; (use-package doom-modeline
;;   :init (doom-modeline-mode 1)
;;   :config
;;   (setq mode-line-percent-position nil
;;         doom-modeline-workspace-name nil
;;         )
;;   )

;; (use-package doom-themes
;;   :config
;;   ;; (load-theme 'doom-one-light t)
;;   (load-theme 'doom-tomorrow-day t)

;;   )

;; (custom-set-faces
;;  `(fringe ((t (:background "grey90")))))



;; -------------------------------------------------------------------


;; -------------------------------------------------------------------

;; (use-package company
;;   :config
;;   (add-to-list
;;    'company-backends 'company-yasnippet)
;;   (defun ym-adfadf () (interactive) (company-abort) (company-begin-backend 'company-yasnippet))
;;   (setq company-minimum-prefix-length 3)

;;   ;; FIXME: (low) company-yasnippet doesn't work for me, figure out why
;;   )

;; (use-package company-quickhelp
;;   :config
;;   (company-quickhelp-mode))


;; -------------------------------------------------------------------


;; -------------------------------------------------------------------



;; -------------------------------------------------------------------


;; -------------------------------------------------------------------

;; python

;; traad, rope for refactoring
;; jedi

;; LSP?

;; polymode for markdown and python is somewhat similar to org-mode+babel for everything else
(use-package polymode)
(use-package poly-markdown)

;; display plots and images in ipython
;; https://github.com/astoff/comint-mime



;; https://www.masteringemacs.org/article/polymode-multiple-major-modes-how-to-use-sql-python-in-one-buffer
;; to configure it for evaluating python, try this: https://emacs.stackexchange.com/questions/74478/evaluate-single-python-code-blocks-in-a-quarto-file-like-in-r-studio-or-jupyter

;; difference between the built-in python mode and the python-mode package: https://www.reddit.com/r/emacs/comments/sshhdi/difference_between_inbuild_python_and_pythonmode/
;; tldr: the built-in python-mode is fine
;; (use-package python-mode)

(use-package pyvenv
  ;; :config
  ;; (pyvenv-mode 1)       ; shows current venv in modeline

  ;; try pyvenv-tracking-mode
  ;; https://blog.allardhendriksen.nl/posts/tracking-project-virtual-environments-with-pyvenv-tracking-mode/
  ;; https://stackoverflow.com/questions/37472595/how-to-activate-the-anancondas-env-python-in-emacs/37489343#37489343
  ;; https://github.com/jorgenschaefer/pyvenv/issues/6
  )


;; System Crafters -- Python Development Configuration
;; https://www.youtube.com/watch?v=jPXIP46BnNA
;; https://systemcrafters.net/emacs-ide/python-development-config/

;; Python Executable Tracker
;; https://github.com/wyuenho/emacs-pet




;; -------------------------------------------------------------------



;; -------------------------------------------------------------------


;; -------------------------------------------------------------------




;;;; links to revisions
;;;; from https://www.reddit.com/r/emacs/comments/asbjai/comment/egv5ymf/
;;;; or [[elisp:(magit-status "/my/repo")][magit status for my repository]]
;; (use-package orgit
;;   ;; Automatically copy orgit link to last commit after commit
;;   :hook (git-commit-post-finish . orgit-store-after-commit)
;;   :config
;;   (defun orgit-store-after-commit ()
;;     "Store orgit-link for latest commit after commit message editor is finished."
;;     (let* ((repo (abbreviate-file-name default-directory))
;;            (rev (magit-git-string "rev-parse" "HEAD"))
;;            (link (format "orgit-rev:%s::%s" repo rev))
;;            (summary (substring-no-properties (magit-format-rev-summary rev)))
;;            (desc (format "%s (%s)" summary repo)))
;;       (push (list link desc) org-stored-links))))





;; -------------------------------------------------------------------



;; -------------------------------------------------------------------



;; -------------------------------------------------------------------




;; -------------------------------------------------------------------



;; -------------------------------------------------------------------




;; -------------------------------------------------------------------


;; -------------------------------------------------------------------






;; -------------------------------------------------------------------

;; clone in order to prevent reusing buffers

;; write the same for occur using occur-rename-buffer
;; for rg clone?


(defvar bubbles-empty-buffer-fn
  (lambda (i) (get-buffer-create "*scratch*"))
  "When there are more windows than buffers, fill them in with the results of this function. It must accept an index arg. Added just in case someone wants to implement something fancy, like adding dired buffers to the end.")

(defun bubbles (&rest numbers)
  (let* ((bufs (mapcar
                (lambda (w) (window-buffer w))
                (window-list)))
         (i 0)
         (main-area-width (if (< bubbles/main-area-colsize (/ (frame-width) (length numbers)))
                              (/ (frame-width) (length numbers))
                            bubbles/main-area-colsize
                              ))
         )
    (cl-flet ((switch-to-next-buffer ()    ; We could avoid switching to the same buffer at the very first entry in the list, but this would hurt simplicity of the body.
                (if bufs
                    (progn
                      (let ((next-buf (car bufs)))
                        (unless (eq next-buf (current-buffer))      ; Handling a special case here, when there are multiple windows for the same buffer, with different positions. Otherwise, point may jump.
                         (switch-to-buffer next-buf)))
                      (pop bufs))
                  (progn
                    (switch-to-buffer (funcall bubbles-empty-buffer-fn i))
                    (cl-incf i)))))
      (delete-other-windows)
      (dotimes (idx (length numbers))    ; instead of dolist, because we want to handle edge cases
        (unless (= idx (- (length numbers) 1))
         (if (= idx 0)
            (split-window-right main-area-width)
          (split-window-right
           (round (/ (window-total-width)
                     (- (length numbers) idx)))
           )))
        (switch-to-next-buffer)
        (dotimes (idx-v (- (nth idx numbers) 1))
          (split-window-below
           (round (/ (window-total-height)
                     (- (nth idx numbers) idx-v)))
           )
          (other-window 1)
          (switch-to-next-buffer))
        (other-window 1))
      (while (ignore-error user-error (windmove-left))
        (comment do nothing))
      (while (ignore-error user-error (windmove-up))
        (comment do nothing))
      )))

(defun bubbles-window-configuration-is-ok-p ()
  (cl-flet ((next-and-below-are-same-p (_)    ; I'm not entirely sure this catches all possible problems, but it's good enough.
              (if (window-in-direction 'below)
                  (eq (window-in-direction 'below)
                      (next-window))
                (let ((cur-wnd (selected-window))
                      (top-wnd-next-column (progn (ignore-error user-error (windmove-right))
                                                  (while (ignore-error user-error (windmove-up)))
                                                  (selected-window))))
                  (select-window cur-wnd)
                  (eq (next-window)
                      top-wnd-next-column)))))
    (let ((list-of-broken-assumptions
           (seq-filter #'not
                       (mapcar
                        #'next-and-below-are-same-p          ; the crux of it
                        (window-list)))))
     (if (seq-empty-p list-of-broken-assumptions)
        t
       nil))))


(defun my-wnd-info (w)
  (with-selected-window w
      (with-current-buffer (window-buffer w)
        (append
         (list
          :buffer-filename-or-name (if (buffer-file-name)
                                       (file-relative-name buffer-file-name (projectile-project-root))
                                     (window-buffer w))
          :projectile-project-root (projectile-project-root)
          :is-file (when (buffer-file-name) t)
          :line-number (save-restriction (widen) (line-number-at-pos))
          :window-width  (window-width)
          :window-height (window-height))
         (when (eq w (selected-window))
             (list :is-selected-window t))
         ))))

;; (defun bubbles-detect-window-configuration-str-2 (get-relevant-window-data-fn)
;;   (interactive)
;;   (if (not (bubbles-window-configuration-is-ok-p))
;;       (error "Bubbles detected a broken window configuration.")
;;     (let ((list-of-wnd-data (mapcar
;;                              get-relevant-window-data-fn
;;                              (window-list))))
;;       (while (ignore-error user-error (windmove-left)))
;;       (while (ignore-error user-error (windmove-up)))
;;       (let ((wnd-rows-count-list '()))
;;         (cl-flet ((count-rows-in-a-column () (let ((r 1))
;;                              (while (ignore-error user-error (windmove-up)))
;;                              (while (ignore-error user-error (windmove-down))
;;                                (cl-incf r)
;;                                )
;;                              ;; r
;;                              (push r wnd-rows-count-list)
;;                              )))
;;           (count-rows-in-a-column)
;;           (while (ignore-error user-error (windmove-right)
;;                                (count-rows-in-a-column))))
;;         (push (nreverse wnd-rows-count-list) list-of-wnd-data)
;;         )
;;       )
;;     )
;;   )


;;;; an example usage of do-while construct in elisp using the widely disliked loop macro, for reference
;; (let ((x 10))
;;   (loop
;;    do (progn
;;         (decf x)
;;         (print x))
;;    while (plusp x)))

(defun bubbles-detect-window-configuration-str-2 (get-relevant-window-data-fn)
  (interactive)
  (cl-flet ((windmove-left-or-nil ()   (ignore-error user-error (windmove-left)))   ; otherwise it throws an error when it's the leftmost window
            (windmove-right-or-nil ()  (ignore-error user-error (windmove-right)))
            (windmove-up-or-nil ()     (ignore-error user-error (windmove-up)))
            (windmove-down-or-nil ()   (ignore-error user-error (windmove-down))))
    (if (not (bubbles-window-configuration-is-ok-p))
        (error "Bubbles detected a broken window configuration.")
      (let ((the-window-before-we-started-walking (selected-window)))
       (while (windmove-left-or-nil))
       (while (windmove-up-or-nil)q)
       (let ((cols '()))     ; our configuration is a list of columns, which are lists of rows, which are descriptions of windows
         (cl-loop
          do (progn   ; wrapped the body of do-while, just for readability
               (while (windmove-up-or-nil))   ; it's a different while, not related to the loop macro
               (let ((rows '()))
                 (cl-loop
                  do (push (funcall get-relevant-window-data-fn (selected-window)) rows)
                  while (windmove-down-or-nil))
                 (push (nreverse rows) cols))
               )
          while (windmove-right-or-nil))
         (select-window the-window-before-we-started-walking)
         (nreverse cols)
         )))))

(defun rebuild-bubbles (conf)
  (interactive)
  (let ((columns conf))   ; just renaming it
    (delete-other-windows)
    (cl-loop for idx from 0
             for col in columns do
             (unless (= idx
                        (- (length columns) 1))   ; the last one
               (split-window-right))
             (cl-loop for idx-v from 0
                      for row in col
                      do
                      (switch-to-buffer (plist-get row :buffer-filename-or-name))
                      (unless (= idx-v
                                 (- (length col) 1))   ; the last one
                        (split-window-below)
                        (other-window 1)
                        ))
             (other-window 1))
    ))

;; (bubbles-detect-window-configuration-str #'my-wnd-info)

;; (setq my-test-wm-conf (bubbles-detect-window-configuration-str-2 #'my-wnd-info))
;; (pp my-test-wm-conf)
;; (rebuild-bubbles my-test-wm-conf)

;; (rebuild-bubbles my-test-wm-conf)

      ;; (other-window 1)  ; After all splitting we're left in the last window, this transfers us to the first one.
      ;; (while (ignore-error user-error (windmove-left)))
      ;; (while (ignore-error user-error (windmove-up)))



(defvar bubbles/main-area-colsize 130)

(defun bubbles/enlarge-main-area (ncols &rest args)
  (interactive)
  (let* ((save-to-preferences (plist-get args :save-to-preferences))
         (cur-wnd (selected-window))
         (n-divisions 1))
    (while (ignore-error user-error (windmove-right))
      (comment do nothing while we move to the right edge of the frame))
    (while (ignore-error user-error (windmove-left))
      (cl-incf n-divisions))
    (while (ignore-error user-error
             (windmove-right))
      (shrink-window-horizontally ncols))
    (select-window cur-wnd)
    (when save-to-preferences
      (let* ((cur-main-area-size (or (gethash n-divisions bubbles/main-area-enlarments-at-different-divisions)
                                     0))
             (increased-size (+ cur-main-area-size ncols)))
        (puthash n-divisions
                 increased-size
                 bubbles/main-area-enlarments-at-different-divisions)))))

(defun bubbles-from-str (s)
  (interactive "s")
  (let ((l (mapcar #'string-to-number (mapcar #'char-to-string s))))
    (apply #'bubbles l)))

(defun bubbles-balance-windows ()
  (interactive)
  
  )


(defun my-enlarge-right (delta)
  (interactive "p")
  (if (window-in-direction 'right)
      (with-selected-window (window-in-direction 'right)
        (shrink-window-horizontally delta))))


(comment
 (bubbles 1 2)
 (bubbles 2 3)
 (bubbles 3 3)
 (bubbles 1 2 3)
 (bubbles 1 2 4)
 (bubbles 1 2 4)
 (bubbles 1 2 5)
 (bubbles 1 3 4)
 (bubbles 2 3 4)
 (bubbles 2 3 4 5)

 (xah-print-hash bubbles/main-area-enlarments-at-different-divisions)

 (defun xah-print-hash (hashtable)
   "Prints the hashtable, each line is key, val"
   (maphash
    (lambda (k v)
      (princ (format "%s , %s" k v))
      (princ "\n"))
    hashtable
    ))
 
 (enlarge-window-horizontally 3)
 (enlarge-window (round (* (window-width) .5)) t)

 
 ;; (defvar bubbles-main-area-increase-factor 0.4)       ; I don't bother to calculate the right percentage, this factor is not seen by the user, so it can be 
 (shrink-window (round (* (window-width) bubbles-main-area-percentage)) t)


 ;; (defvar bubbles-main-area-enlarge-by-number-of-columns 30)

 

 
 (progn
   (delete-other-windows)
   (split-window-horizontally)
   (split-window-vertically)
   (other-window 1)
   (split-window-vertically)
   (other-window 1)
   ;; (split-window-horizontally)
   (other-window 1)
   (split-window-vertically)
   (other-window 1)
   (balance-windows)
   )

 
 ;; (window-parent (selected-window))
 (split-window (window-parent (selected-window)) nil 'right)   ; instead of (split-window-horizontally)
 (split-window (frame-root-window) nil 'right)   ; instead of (split-window-horizontally)

 
 

 (window-in-direction 'down)   ; https://www.gnu.org/software/emacs/manual/html_node/elisp/Windows-and-Frames.html
 (window-full-height-p)
 

 (mapcar
  (lambda (w)
    (with-current-buffer (window-buffer w)
      (format "[[%s::%d]]"
              (if (buffer-file-name)
                  (file-relative-name buffer-file-name (projectile-project-root))
                (window-buffer w))
              (save-restriction (widen) (line-number-at-pos)))))
  (window-list))

 (car (seq-filter
       (lambda (w)
         (and (window-at-side-p w 'bottom)
              (window-at-side-p w 'right)))
       (window-list)))


 (split-window-vertically (floor (* 0.68 (window-height))))

 
 (setq display-buffer-alist '((popper-display-control-p          ; the original value
                               (popper-select-popup-at-bottom))))

 (setq display-buffer-alist '((popper-display-control-p
                               (display-buffer-no-window))
                              (".*"
                               (display-buffer-same-window))))
 
 (setq display-buffer-alist '(("\\*Help\\*"
                               (display-buffer-in-side-window)
                               (side . right)
                               (slot . 99999999)
                               )))

 (defun my-switch-to-buffer-list (buffer alist)
   ;; (split-window-below)
   (select-window
    ;; (display-buffer-below-selected buffer alist)
    (display-buffer-use-some-window buffer alist)
    ))
 (setq display-buffer-alist '((".*" (my-switch-to-buffer-list))))
 (setq display-buffer-alist '((popper-display-control-p          ; the original value
                               (my-switch-to-buffer-list))))

 ;; see https://www.masteringemacs.org/article/demystifying-emacs-window-manager
 ;; see https://e17i.github.io/articles-emacs-display-1/
 )



;; (advice-add 'split-window-right :after #'balance-windows)

;; -------------------------------------------------------------------

(use-package org-transclusion)

(defun org-dblock-write:transclusion (params)
  (progn
    (with-temp-buffer
      
      (insert-file-contents (plist-get params :filename))
      (let ((range-start (or (plist-get params :min) (line-number-at-pos (point-min))))
            (range-end (or (plist-get params :max) (line-number-at-pos (point-max)))))
        (copy-region-as-kill (line-beginning-position range-start)
                             (line-end-position range-end)))
      )
    (insert "\n#+begin_src elisp\n")
    (yank)
    (insert "\n#+end_src\n")
    ))

;; -------------------------------------------------------------------










