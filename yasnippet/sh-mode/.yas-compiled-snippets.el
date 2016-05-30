;;; Compiled snippets and support files for `sh-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'sh-mode
                     '(("for" "for ${1:var} in ${2:stuff} ; do\n    $0\ndone" "for" nil nil nil nil nil nil)
                       ("f" "${1:name} () {\n    ${2:echo \"return string --- var=\\$($1)\"\n    return exit_code}$0\n}" "function" nil nil nil nil nil nil)
                       ("if" "if [[ $1 ]]; then\n    $0\nfi\n" "if" nil nil nil nil nil nil)
                       ("ymyasnippetautoinsertmodetemplate" "#!/usr/bin/env bash\n\n$0\n\n" "ymyasnippetautoinsertmodetemplate" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Tue May 24 19:03:29 2016
