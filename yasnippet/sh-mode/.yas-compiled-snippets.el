;;; Compiled snippets and support files for `sh-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'sh-mode
                     '(("ymyasnippetautoinsertmodetemplate" "#!/usr/bin/env bash\n\n$0\n\n" "ymyasnippetautoinsertmodetemplate" nil nil nil "/Users/alex/.emacs.d/yasnippet/sh-mode/ymyasnippetautoinsertmodetemplate" nil nil)
                       ("if" "if [[ $1 ]]; then\n    $0\nfi\n" "if" nil nil nil "/Users/alex/.emacs.d/yasnippet/sh-mode/if" nil nil)
                       ("f" "${1:name} () {\n    ${2:echo \"return string --- var=\\$($1)\"\n    return exit_code}$0\n}" "function" nil nil nil "/Users/alex/.emacs.d/yasnippet/sh-mode/function" nil nil)
                       ("for" "for ${1:var} in ${2:stuff} ; do\n    $0\ndone" "for" nil nil nil "/Users/alex/.emacs.d/yasnippet/sh-mode/for" nil nil)))


;;; Do not edit! File generated at Sun May 14 16:52:11 2017
