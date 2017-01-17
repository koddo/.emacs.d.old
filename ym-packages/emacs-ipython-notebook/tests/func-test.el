(eval-when-compile (require 'cl))
(require 'ert)

(require 'ein-loaddefs)
(require 'ein-notebooklist)
(require 'wid-edit)
(require 'ein-testing)
(require 'ein-testing-cell)

(let ((backend (getenv "EL_REQUEST_BACKEND")))
  (when (and backend (not (equal backend "")))
    (setq request-backend (intern backend))
    (message "Using request-backend = %S" request-backend)))

(ein:setq-if-not ein:testing-dump-file-log "func-test-batch-log.log")
(ein:setq-if-not ein:testing-dump-file-messages "func-test-batch-messages.log")
(setq message-log-max t)


(defvar ein:testing-port 8889)

(defun ein:testing-wait-until (message predicate &optional predargs max-count)
  "Wait until PREDICATE function returns non-`nil'.
PREDARGS is argument list for the PREDICATE function.
Make MAX-COUNT larger \(default 50) to wait longer before timeout."
  (ein:log 'debug "TESTING-WAIT-UNTIL start")
  (ein:log 'debug "TESTING-WAIT-UNTIL waiting on: %s" message)
  (unless (setq max-count 50))
  (unless (loop repeat max-count
                when (apply predicate predargs)
                return t
                ;; borrowed from `deferred:sync!':
                do (sit-for 0.05)
                do (sleep-for 0.05))
    (error "Timeout"))
  (ein:log 'debug "TESTING-WAIT-UNTIL end"))

(defun ein:testing-get-notebook-by-name (url-or-port notebook-name &optional path)
  (ein:log 'debug "TESTING-GET-NOTEBOOK-BY-NAME start")
  ;; Kill notebook list buffer here to make sure next
  ;; `ein:testing-wait-until' works properly.
  (kill-buffer (ein:notebooklist-get-buffer url-or-port))
  (when path
    (setq notebook-name (format "%s/%s" path notebook-name)))
  (with-current-buffer (ein:notebooklist-open url-or-port path)
    (sleep-for 1.0) ;; Because some computers are too fast???
    (ein:testing-wait-until "ein:notebooklist-open" (lambda () ein:%notebooklist%))
    (prog1
        (ignore-errors
          (ein:notebooklist-open-notebook-by-name notebook-name url-or-port))
      (ein:log 'debug "TESTING-GET-NOTEBOOK-BY-NAME end"))))

(defun ein:testing-get-untitled0-or-create (url-or-port &optional path)
  (unless path (setq path ""))
  (ein:log 'debug "TESTING-GET-UNTITLED0-OR-CREATE start")
  (let ((notebook (ein:testing-get-notebook-by-name url-or-port "Untitled.ipynb")))
    (if notebook
        (progn (ein:log 'debug
                 "TESTING-GET-UNTITLED0-OR-CREATE notebook already exists")
               notebook)
      (ein:log 'debug
        "TESTING-GET-UNTITLED0-OR-CREATE creating notebook")
      (let ((created nil)
            (kernelspec (first (ein:list-available-kernels url-or-port))))
        (ein:notebooklist-new-notebook url-or-port kernelspec path
                                       (lambda (&rest -ignore-)
                                         (setq created t)))
        (ein:testing-wait-until "ein:notebooklist-new-notebook"
                                (lambda () created)))
      (prog1
          (ein:testing-get-notebook-by-name url-or-port "Untitled.ipynb" path)
        (ein:log 'debug "TESTING-GET-UNTITLED0-OR-CREATE end")))))

(defvar ein:notebooklist-after-open-hook nil)

(defadvice ein:notebooklist-url-retrieve-callback
  (after ein:testing-notebooklist-url-retrieve-callback activate)
  "Advice to add `ein:notebooklist-after-open-hook'."
  (run-hooks 'ein:notebooklist-after-open-hook))

(defun ein:testing-delete-notebook (url-or-port notebook &optional path)
  (ein:log 'debug "TESTING-DELETE-NOTEBOOK start")
  (ein:notebook-close notebook)
  (with-current-buffer (ein:notebooklist-open url-or-port path)
    (ein:testing-wait-until "ein:notebooklist-open"
                            (lambda () ein:%notebooklist%))
    (ein:notebooklist-delete-notebook (ein:$notebook-notebook-path notebook)))
  (ein:log 'debug "TESTING-DELETE-NOTEBOOK end"))

(ert-deftest ein:testing-get-untitled0-or-create ()
  (ein:log 'verbose "ERT TESTING-GET-UNTITLED0-OR-CREATE start")
  (let ((notebook (ein:testing-get-untitled0-or-create ein:testing-port)))
    (ein:testing-wait-until
     "ein:testing-get-untitled0-or-create"
     (lambda () (ein:aand (ein:$notebook-kernel notebook)
                          (ein:kernel-live-p it))))
    (with-current-buffer (ein:notebook-buffer notebook)
      (should (equal (ein:$notebook-notebook-name ein:%notebook%)
                     "Untitled.ipynb"))))
  (ein:log 'verbose "ERT TESTING-GET-UNTITLED0-OR-CREATE end"))

(ert-deftest ein:testing-delete-untitled0 ()
  (ein:log 'verbose "----------------------------------")
  (ein:log 'verbose "ERT TESTING-DELETE-UNTITLED0 start")
  (ein:log 'debug "ERT TESTING-DELETE-UNTITLED0 creating notebook")
  (let ((notebook (ein:testing-get-untitled0-or-create ein:testing-port)))
    (ein:testing-wait-until
     "ein:test-get-untitled0-or-create"
     (lambda () (ein:aand (ein:$notebook-kernel notebook)
                          (ein:kernel-live-p it))))
    (ein:log 'debug "ERT TESTING-DELETE-UNTITLED0 delete notebook")
    (ein:testing-delete-notebook ein:testing-port notebook))
  (ein:log 'debug
    "ERT TESTING-DELETE-UNTITLED0 check that the notebook is deleted")
  (let ((num-notebook
         (length (ein:testing-get-notebook-by-name ein:testing-port
                                                   "Untitled.ipynb"
                                                   ""))))
    (should (= num-notebook 0)))
  (ein:log 'debug "ERT TESTING-DELETE-UNTITLED0 end"))

(ert-deftest ein:notebook-execute-current-cell-simple ()
  (let ((notebook (ein:testing-get-untitled0-or-create ein:testing-port)))
    (ein:testing-wait-until
     "ein:testing-get-untitled0-or-create"
     (lambda () (ein:aand (ein:$notebook-kernel notebook)
                          (ein:kernel-live-p it))))
    (with-current-buffer (ein:notebook-buffer notebook)
      (call-interactively #'ein:worksheet-insert-cell-below)
      (insert "a = 100\na")
      (let ((cell (call-interactively #'ein:worksheet-execute-cell)))
        (ein:testing-wait-until "ein:worksheet-execute-cell"
                                (lambda () (not (oref cell :running)))))
      ;; (message "%s" (buffer-string))
      (save-excursion
        (should (search-forward-regexp "Out \\[[0-9]+\\]" nil t))
        (should (search-forward "100" nil t))))))

(defun ein:testing-image-type (image)
  "Return the type of IMAGE.
See the definition of `create-image' for how it works."
  (assert (and (listp image) (eq (car image) 'image)) nil
          "%S is not an image." image)
  (plist-get (cdr image) :type))

(ert-deftest ein:notebook-execute-current-cell-pyout-image ()
  (let ((notebook (ein:testing-get-untitled0-or-create ein:testing-port)))
    (ein:testing-wait-until
     "ein:testing-get-untitled0-or-create"
     (lambda () (ein:aand (ein:$notebook-kernel notebook)
                          (ein:kernel-live-p it))))
    (with-current-buffer (ein:notebook-buffer notebook)
      (call-interactively #'ein:worksheet-insert-cell-below)
      ;; Use IPython.core.display rather than IPython.display to
      ;; test it with older (< 0.13) IPython.
      (insert (concat "from IPython.core.display import SVG\n"
                      (format "SVG(data=\"\"\"%s\"\"\")"
                              ein:testing-example-svg)))
      (let ((cell (call-interactively #'ein:worksheet-execute-cell)))
        ;; It seems in this case, watching `:running' does not work
        ;; well sometimes.  Probably "output reply" (iopub) comes
        ;; before "execute reply" in this case.
        (ein:testing-wait-until "ein:worksheet-execute-cell"
                                (lambda () (oref cell :outputs)))
        ;; This cell has only one input
        (should (= (length (oref cell :outputs)) 1))
        ;; This output is a SVG image
        (let ((out (nth 0 (oref cell :outputs))))
          (should (equal (plist-get out :output_type) "execute_result"))
          (should (plist-get out :svg))))
      ;; Check the actual output in the buffer:
      (save-excursion
        (should (search-forward-regexp "Out \\[[0-9]+\\]" nil t))
        (should (= (forward-line) 0))
        (if (image-type-available-p 'svg)
            (let ((image (get-text-property (point) 'display)))
              (should (eq (ein:testing-image-type image) 'svg)))
          (ein:log 'info
            "Skipping image check as SVG image type is not available."))))))

(ert-deftest ein:notebook-execute-current-cell-stream ()
  (let ((notebook (ein:testing-get-untitled0-or-create ein:testing-port)))
    (ein:testing-wait-until
     "ein:testing-get-untitled0-or-create"
     (lambda () (ein:aand (ein:$notebook-kernel notebook)
                          (ein:kernel-live-p it))))
    (with-current-buffer (ein:notebook-buffer notebook)
      (call-interactively #'ein:worksheet-insert-cell-below)
      (insert "print('Hello')")
      (let ((cell (call-interactively #'ein:worksheet-execute-cell)))
        (ein:testing-wait-until "ein:worksheet-execute-cell"
                                (lambda () (not (oref cell :running)))))
      (save-excursion
        (should-not (search-forward-regexp "Out \\[[0-9]+\\]" nil t))
        (should (search-forward-regexp "^Hello$" nil t))))))

(ert-deftest ein:notebook-execute-current-cell-question ()
  (let ((notebook (ein:testing-get-untitled0-or-create ein:testing-port)))
    (ein:testing-wait-until
     "ein:testing-get-untitled0-or-create"
     (lambda () (ein:aand (ein:$notebook-kernel notebook)
                          (ein:kernel-live-p it))))
    (with-current-buffer (ein:notebook-buffer notebook)
      (call-interactively #'ein:worksheet-insert-cell-below)
      (insert "range?")
      (let ((cell (call-interactively #'ein:worksheet-execute-cell)))
        (ein:testing-wait-until
         "ein:worksheet-execute-cell"
         (lambda () (not (oref cell :running)))))
      (with-current-buffer (get-buffer (ein:$notebook-pager notebook))
        (should (search-forward "Docstring:"))))))

(ert-deftest ein:notebook-request-help ()
  (let ((notebook (ein:testing-get-untitled0-or-create ein:testing-port)))
    (ein:testing-wait-until
     "ein:testing-get-untitled0-or-create"
     (lambda () (ein:aand (ein:$notebook-kernel notebook)
                          (ein:kernel-live-p it))))
    (with-current-buffer (ein:notebook-buffer notebook)
      (call-interactively #'ein:worksheet-insert-cell-below)
      (let ((pager-name (ein:$notebook-pager ein:%notebook%)))
        (ein:aif (get-buffer pager-name)
            (kill-buffer it))
        (insert "file")
        (call-interactively #'ein:pytools-request-help)
        ;; Pager buffer will be created when got the response
        (ein:testing-wait-until
         "ein:pythools-request-help"
         (lambda () (get-buffer pager-name)))
        (with-current-buffer (get-buffer pager-name)
          (should (search-forward "Docstring:")))))))
