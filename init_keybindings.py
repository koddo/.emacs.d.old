from more_itertools import powerset 

def asdf(l1, s2):
    for k in l1:
        for subset in powerset(list(s2)):
            backslash = '\\'
            ss = ('C',) + subset
            frm = '(kbd "' + '' + '-'.join(ss) + '-' + k.replace(backslash, "") + '")'
            # frm = '[\\' + '-\\'.join(ss) + '-' + k.replace(backslash, "") + ']'
            to = '-'.join(ss) + '-' + k
            l = f'(define-key input-decode-map {frm} [{to}])'
            print(l)
        print()
        

asdf(['i', 'm'], 'SMsH')
asdf(['\[', '{'], 'sH')



# ;; C-i, C-m, C-[ with all combinations of Shift, Meta, Super, Hyper

# (define-key input-decode-map (kbd "C-i") [C-i])
#                  (define-key input-decode-map (kbd "C-S-i") [C-S-i])
#                  (define-key input-decode-map (kbd "C-M-i") [C-M-i])
#                  (define-key input-decode-map (kbd "C-s-i") [C-s-i])
#                  (define-key input-decode-map (kbd "C-H-i") [C-H-i])
#                  (define-key input-decode-map (kbd "C-S-M-i") [C-S-M-i])
#                  (define-key input-decode-map (kbd "C-S-s-i") [C-S-s-i])
#                  (define-key input-decode-map (kbd "C-S-H-i") [C-S-H-i])
#                  (define-key input-decode-map (kbd "C-M-s-i") [C-M-s-i])
#                  (define-key input-decode-map (kbd "C-M-H-i") [C-M-H-i])
#                  (define-key input-decode-map (kbd "C-s-H-i") [C-s-H-i])
#                  (define-key input-decode-map (kbd "C-S-M-s-i") [C-S-M-s-i])
#                  (define-key input-decode-map (kbd "C-S-M-H-i") [C-S-M-H-i])
#                  (define-key input-decode-map (kbd "C-S-s-H-i") [C-S-s-H-i])
#                  (define-key input-decode-map (kbd "C-M-s-H-i") [C-M-s-H-i])
#                  (define-key input-decode-map (kbd "C-S-M-s-H-i") [C-S-M-s-H-i])

# (define-key input-decode-map (kbd "C-m") [C-m])
#                  (define-key input-decode-map (kbd "C-S-m") [C-S-m])
#                  (define-key input-decode-map (kbd "C-M-m") [C-M-m])
#                  (define-key input-decode-map (kbd "C-s-m") [C-s-m])
#                  (define-key input-decode-map (kbd "C-H-m") [C-H-m])
#                  (define-key input-decode-map (kbd "C-S-M-m") [C-S-M-m])
#                  (define-key input-decode-map (kbd "C-S-s-m") [C-S-s-m])
#                  (define-key input-decode-map (kbd "C-S-H-m") [C-S-H-m])
#                  (define-key input-decode-map (kbd "C-M-s-m") [C-M-s-m])
#                  (define-key input-decode-map (kbd "C-M-H-m") [C-M-H-m])
#                  (define-key input-decode-map (kbd "C-s-H-m") [C-s-H-m])
#                  (define-key input-decode-map (kbd "C-S-M-s-m") [C-S-M-s-m])
#                  (define-key input-decode-map (kbd "C-S-M-H-m") [C-S-M-H-m])
#                  (define-key input-decode-map (kbd "C-S-s-H-m") [C-S-s-H-m])
#                  (define-key input-decode-map (kbd "C-M-s-H-m") [C-M-s-H-m])
#                  (define-key input-decode-map (kbd "C-S-M-s-H-m") [C-S-M-s-H-m])

# (define-key input-decode-map (kbd "C-[") [C-\[])
#                  (define-key input-decode-map (kbd "C-s-[") [C-s-\[])
#                  (define-key input-decode-map (kbd "C-H-[") [C-H-\[])
#                  (define-key input-decode-map (kbd "C-s-H-[") [C-s-H-\[])

# (define-key input-decode-map (kbd "C-{") [C-{])
#  (define-key input-decode-map (kbd "C-s-{") [C-s-{])
#   (define-key input-decode-map (kbd "C-H-{") [C-H-{])
#    (define-key input-decode-map (kbd "C-s-H-{") [C-s-H-{])

# ;; I couldn't redefine all those bindings with meta and C-[, they all translate to M-ESC with other modifiers
# ;; but that's not a problem, we can make bindings with them directly
#     ;; we do this later in the ym-keys-minor-mode


# (global-set-key (kbd "C-c <C-i>") (lambda () (interactive)(message "TODO: make this copy and move up here")))
#     (global-set-key (kbd "C-x <C-i>") (lambda () (interactive)(message "TODO: make this copy and move up here")))

# ;; -------------------------------------------------------------------

# (defun ym-undefined-key () (interactive) (message "undefined keybinding yet, see init.el"))
