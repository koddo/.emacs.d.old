
;; -------------------------------------------------------------------
(ym-add-to-list-dired-omit-extensions '(".o" ".a"))
;; -------------------------------------------------------------------
;; automatically indent pasted text if in programming-modes
(defvar yank-indent-modes '(emacs-lisp-mode
                            c-mode c++-mode
                            tcl-mode
                            sql-mode
                            perl-mode cperl-mode
                            java-mode jde-mode
                            lisp-interaction-mode
                            LaTeX-mode TeX-mode
                            erlang-mode
                            python-mode
                            ruby-mode
                            haskell-mode
                            )
 "Modes in which to indent regions that are yanked (or yank-popped)")
(defvar yank-advised-indent-threshold 1000000
  "Threshold (# chars) over which indentation does not automatically occur.")
(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) yank-advised-indent-threshold)
      (indent-region beg end nil)))
(defadvice yank (after yank-indent activate)
  "If current mode is one of 'yank-indent-modes, indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (member major-mode yank-indent-modes))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))
(defadvice yank-pop (after yank-pop-indent activate)
  "If current mode is one of 'yank-indent-modes, indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (member major-mode yank-indent-modes))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))
;; -------------------------------------------------------------------
;; for writing elisp macros
(require 'macrostep)
(defun m/elisp-macrostep-expand ()
  (interactive)
  (backward-sexp)
  (macrostep-expand))
;; -------------------------------------------------------------------
;; show matching paren
(require 'paren)
(setq show-paren-delay 0)
(setq show-paren-style 'mixed)   ; highlight parens if both visible, highlight the whole expression when one paren is invisible
;; (modify-syntax-entry ?\{ "(}")
;; (modify-syntax-entry ?\} "){")
(show-paren-mode t)
(progn (require 'autopair)   ; insert paired parenthesis
       (autopair-global-mode)
       (setq autopair-blink nil)
       (setq autopair-skip-whitespace 'chomp))   ; ) ) => )) when closing
;; -------------------------------------------------------------------
(require 'yasnippet)
(setq yas/snippet-dirs '("~/.emacs.d/yasnippet"))   ; only use my snippets, skip default snippets dir
(setq yas-new-snippet-default "# -*- mode: snippet -*-\n# name: $1\n# key: ${2:${1:$(yas--key-from-desc yas-text)}}\n# --\n$0")
(yas/global-mode 1)
(add-hook 'snippet-mode-hook
          (lambda ()
            (setq require-final-newline nil)
            (add-hook 'after-save-hook 'yas-recompile-all 'append 'make-it-local)
            (add-hook 'after-save-hook 'yas-reload-all 'append 'make-it-local)))
(auto-insert-mode 1)
(setq auto-insert-query nil)
(defun ym-auto-insert-action () (yas-expand-snippet (yas--template-content (yas--get-template-by-uuid major-mode "ymyasnippetautoinsertmodetemplate")) (point-min) (point-max)))
(define-auto-insert 'sh-mode 'ym-auto-insert-action)   ; when adding new modes, put file 'ymyasnippetautoinsertmodetemplate' to yasnippet/mode/ dir
;; (setq yas/indent-line 'fixed)
;; (setq yas/wrap-around-region nil)
;; clojure (ns ...) auto fill: http://inclojurewetrust.blogspot.ru/2011/04/fed-up-of-typing-ns-declarations.html
;; https://github.com/swannodette/clojure-snippets
;;; ??? hippie-expand
;; -------------------------------------------------------------------
;; -------------------------------------------------------------------
;; (setq save-abbrevs nil)   ; stop asking whether to save newly added abbrev when quitting emacs
;; (setq-default abbrev-mode t)   ; turn on abbrev mode globally
;; (define-abbrev-table 'sql-mode-abbrev-table
;;   (mapcar #'(lambda (v) (list v (upcase v) nil 1))
;;           '("absolute" "action" "add" "after" "all" "allocate" "alter" "and" "any" "are" "array" "as" "asc" "asensitive" "assertion" "asymmetric" "at" "atomic" "authorization" "avg" "before" "begin" "between" "bigint" "binary" "bit" "bitlength" "blob" "boolean" "both" "breadth" "by" "call" "called" "cascade" "cascaded" "case" "cast" "catalog" "char" "char_length" "character" "character_length" "check" "clob" "close" "coalesce" "collate" "collation" "column" "commit" "condition" "connect" "connection" "constraint" "constraints" "constructor" "contains" "continue" "convert" "corresponding" "count" "create" "cross" "cube" "current" "current_date" "current_default_transform_group" "current_path" "current_role" "current_time" "current_timestamp" "current_transform_group_for_type" "current_user" "cursor" "cycle" "data" "date" "day" "deallocate" "dec" "decimal" "declare" "default" "deferrable" "deferred" "delete" "depth" "deref" "desc" "describe" "descriptor" "deterministic" "diagnostics" "disconnect" "distinct" "do" "domain" "double" "drop" "dynamic" "each" "element" "else" "elseif" "end" "equals" "escape" "except" "exception" "exec" "execute" "exists" "exit" "external" "extract" "false" "fetch" "filter" "first" "float" "for" "foreign" "found" "free" "from" "full" "function" "general" "get" "global" "go" "goto" "grant" "group" "grouping" "handler" "having" "hold" "hour" "identity" "if" "immediate" "in" "indicator" "initially" "inner" "inout" "input" "insensitive" "insert" "int" "integer" "intersect" "interval" "into" "is" "isolation" "iterate" "join" "key" "language" "large" "last" "lateral" "leading" "leave" "left" "level" "like" "local" "localtime" "localtimestamp" "locator" "loop" "lower" "map" "match" "map" "member" "merge" "method" "min" "minute" "modifies" "module" "month" "multiset" "names" "national" "natural" "nchar" "nclob" "new" "next" "no" "none" "not" "null" "nullif" "numeric" "object" "octet_length" "of" "old" "on" "only" "open" "option" "or" "order" "ordinality" "out" "outer" "output" "over" "overlaps" "pad" "parameter" "partial" "partition" "path" "position" "precision" "prepare" "preserve" "primary" "prior" "privileges" "procedure" "public" "range" "read" "reads" "real" "recursive" "ref" "references" "referencing" "relative" "release" "repeat" "resignal" "restrict" "result" "return" "returns" "revoke" "right" "role" "rollback" "rollup" "routine" "row" "rows" "savepoint" "schema" "scope" "scroll" "search" "second" "section" "select" "sensitive" "session" "session_user" "set" "sets" "signal" "similar" "size" "smallint" "some" "space" "specific" "specifictype" "sql" "sqlcode" "sqlerror" "sqlexception" "sqlstate" "sqlwarning" "start" "state" "static" "submultiset" "substring" "sum" "symmetric" "system" "system_user" "table" "tablesample" "temporary" "then" "time" "timestamp" "timezone_hour" "timezone_minute" "to" "trailing" "transaction" "translate" "translation" "treat" "trigger" "trim" "true" "under" "undo" "union" "unique" "unknown" "unnest" "until" "update" "upper" "usage" "user" "using" "value" "values" "varchar" "varying" "view" "when" "whenever" "where" "while" "window" "with" "within" "without" "work" "write" "year" "zone")
;;           ))
(load-library "sql-indent")
(setq sql-indent-first-column-regexp    ; added "alter"
      (concat "\\(^\\s-*" (regexp-opt '(
                                        "select" "update" "insert" "delete"
                                        "union" "intersect"
                                        "from" "where" "into" "group" "having" "order"
                                        "set"
                                        "create" "drop" "truncate"
                                        ;; <------------------------ my
                                        "alter"
                                        "reset"
                                        "begin" "end" "commit" "return" "declare"
                                        "if"
                                        "$$"
                                        "grant"
                                        "--") t) "\\(\\b\\|\\s-\\)\\)\\|\\(^```$\\)"))
(defun m/align-sql (start end)
 (interactive "*r")
 (align-regexp start end "\\s-*[^[:space:]]*\\(\\s-*\\)" 1 3 nil)
 )
(add-to-list 'auto-mode-alist '("\\.pgtap$" . sql-mode))
;; -------------------------------------------------------------------
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jinja$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.dtl$" . web-mode))
(setq web-mode-markup-indent-offset 4)
(setq web-mode-code-indent-offset 4)
;; -------------------------------------------------------------------
(require 'flycheck)
;; -------------------------------------------------------------------
(require 'expand-region)
;; (global-set-key (kbd "C-=") 'er/expand-region)     ; TODO: keybinding
;; -------------------------------------------------------------------
(require 'quickrun)
;; (quickrun-add-command "shellscript"
;;                       '((:command . (lambda () sh-shell))
;;                         (:description . "Run Shellscript file")))
;; -------------------------------------------------------------------
(require 'clojure-mode)
(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs\\'" . clojurescript-mode))
(add-to-list 'auto-mode-alist '("\\.cljx\\'" . clojure-mode))
(require 'queue)
(require 'cider)
;; (setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")
;; -------------------------------------------------------------------
(require 'yaml-mode)
;; -------------------------------------------------------------------
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(autoload 'gfm-mode "markdown-mode" "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown$" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . gfm-mode))
;; -------------------------------------------------------------------
(defun ym-jekyll-post-template ()
  (interactive)
  (yas-expand-snippet (yas--template-content (yas--get-template-by-uuid major-mode "ymjekyllposttemplate"))))
(defun ym-rename-current-buffer-file (&optional suggestion)   ; http://emacs.stackexchange.com/questions/2849/save-current-file-with-a-slightly-different-name/2850#2850
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " nil nil 'confirm
                                      (if suggestion
                                          suggestion
                                        filename)   ; a bug here -- when using ido-everywhere and falling back to a regular read-file-name with C-f, it inserts a name twice, but this doesn't bother me, because I never use this fallback
                                      )))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'."
                   name (file-name-nondirectory new-name)))))))
(defun ym-jekyll-regenerate-date-and-filename ()
  (interactive)
  (beginning-of-buffer)
  (search-forward-regexp "^--- *$" nil t 2)
  (let ((end-of-front-matter (point)))
    (beginning-of-buffer)
    (search-forward-regexp "^date:.*$" end-of-front-matter t)
    (delete-region (line-beginning-position)
                   (line-end-position))
    (insert "date:   " (format-time-string "%Y-%m-%d %H:%M:%S %z"))
    (beginning-of-buffer)
    (search-forward-regexp "^title:.*$" end-of-front-matter t)
    (let ((title-line (buffer-substring (line-beginning-position)
                                  (line-end-position))))
      (save-match-data
        (string-match "title:  \"\\(.*\\)\"" title-line)
        (let ((title
               (s-replace-all '((" " . "-") ("'" . "") ("\"" . "") ("!" . "") ("?" . "") ("." . "") ("," . ""))
                              (downcase (match-string 1 title-line)))))
          (ym-rename-current-buffer-file (concat (format-time-string "%Y-%m-%d") "-" title ".markdown"))
          )
        )
      )
    )
  )


;; -------------------------------------------------------------------
;; $ pip install rope jedi flake8 importmagic autopep8 yapf
(require 'pyvenv)
(pyvenv-workon "default352")
(require 'elpy)
(setq elpy-rpc-backend "jedi")
(require 'company-quickhelp)
(company-quickhelp-mode 1)
(require 'company)
(require 'highlight-indentation)
;; (autoload 'find-file-in-project "find-file-in-project" nil t)
;; (autoload 'find-file-in-project-by-selected "find-file-in-project" nil t)
;; (autoload 'find-directory-in-project-by-selected "find-file-in-project" nil t)
;; (autoload 'ffip-show-diff "find-file-in-project" nil t)
;; (autoload 'ffip-save-ivy-last "find-file-in-project" nil t)
;; (autoload 'ffip-ivy-resume "find-file-in-project" nil t)



;; helm
;; https://github.com/syohex/emacs-helm-ag
;; https://github.com/bbatsov/helm-projectile







;; https://demonastery.org/2013/04/emacs-narrow-to-region-indirect/
;; https://emacs.stackexchange.com/questions/12180/why-use-indirect-buffers/12185#12185
(defun narrow-to-region-indirect (start end)
  "Restrict editing in this buffer to the current region, indirectly."
  (interactive "r")
  (deactivate-mark)
  (let ((buf (clone-indirect-buffer nil nil)))
    (with-current-buffer buf
      (narrow-to-region start end)
      (python-mode))
    (switch-to-buffer buf)
    (font-lock-fontify-buffer)   ; without this the colors get lost
    ))


;; -------------------------------------------------------------------
;; c++
(setq-default c-basic-offset 4)
;; -------------------------------------------------------------------

