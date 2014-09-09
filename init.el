(defun ym-add-everything-to-load-path-in (base)
    (add-to-list 'load-path base)
    (dolist (f (directory-files base))
      (let ((name (concat base "/" f)))
        (when (and (file-directory-p name) 
                   (not (equal f ".."))
                   (not (equal f ".")))
          (add-to-list 'load-path name)))))
(ym-add-everything-to-load-path-in "~/.emacs.d/ym-packages")
(add-to-list 'load-path "~/.emacs.d/ym-packages/org-mode/contrib/lisp")
(add-to-list 'load-path "~/.emacs.d/ym-packages/org-mode/lisp")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(load-file "~/.emacs.d/init__preinit_this_machine.el")
(setq custom-file "~/.emacs.d/init_custom.el")
(load custom-file 'noerror)


(load-file "~/.emacs.d/init_emacs.el")
(load-file "~/.emacs.d/init_org.el")
(load-file "~/.emacs.d/init_development.el")
(load-file "~/.emacs.d/init_keybindings.el")
(load-file "~/.emacs.d/init_functions.el")
(load-file "~/.emacs.d/init_colors.el")



(dired "~/")
(dired "~/Downloads")
(dired "~/workspace")
(find-file ym-org-agenda-file)

(defun ym-clone-subtree-of-agenda-file-to-buffer (subtree-regexp buffer-name)
  (when (not (buffer-live-p (get-buffer buffer-name)))
    (switch-to-buffer ".org.agenda.org.gpg")
    (clone-indirect-buffer buffer-name t t)
    (beginning-of-buffer)
    (search-forward-regexp subtree-regexp)
    (beginning-of-line)
    (org-narrow-to-subtree)
    (org-content 1000)))
(ym-clone-subtree-of-agenda-file-to-buffer "^* Contacts"            ym-org-contacts-view-buffer-name)
(ym-clone-subtree-of-agenda-file-to-buffer "^* Problems count"      ym-org-problems-count-view-buffer-name)
  
(ym-org-day-view)


























