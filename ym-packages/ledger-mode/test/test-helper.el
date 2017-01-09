;;; test-helper.el --- ERT for ledger-mode

;;; Commentary:
;;  ERT test helpers for ledger-mode

;;; Code:
(require 'ledger-mode)
(require 'ert)
(require 'cl-macs) ; Common Lisp Macros

(defvar demo-ledger)

;; FIXME instead of setting the string, load the content from file
;; ../test/input/demo.ledger
(setq demo-ledger "2010/12/01 * Checking balance
  Assets:Checking                   $1,000.00
  Equity:Opening Balances

2010/12/20 * Organic Co-op
  Expenses:Food:Groceries             $ 37.50  ; [=2011/01/01]
  Expenses:Food:Groceries             $ 37.50  ; [=2011/02/01]
  Expenses:Food:Groceries             $ 37.50  ; [=2011/03/01]
  Expenses:Food:Groceries             $ 37.50  ; [=2011/04/01]
  Expenses:Food:Groceries             $ 37.50  ; [=2011/05/01]
  Expenses:Food:Groceries             $ 37.50  ; [=2011/06/01]
  Assets:Checking                   $ -225.00

2010/12/28 Acme Mortgage
  Liabilities:Mortgage:Principal    $  200.00
  Expenses:Interest:Mortgage        $  500.00
  Expenses:Escrow                   $  300.00
  * Assets:Checking                $ -1000.00

2011/01/02 Grocery Store
  Expenses:Food:Groceries             $ 65.00
  * Assets:Checking

2011/01/05 Employer
  * Assets:Checking                 $ 2000.00
  Income:Salary

2011/01/14 Bank
  ; Regular monthly savings transfer
  Assets:Savings                     $ 300.00
  Assets:Checking

2011/01/19 Grocery Store
  Expenses:Food:Groceries             $ 44.00 ; hastag: not block
  Assets:Checking

2011/01/25 Bank
  ; Transfer to cover car purchase
  Assets:Checking                  $ 5,500.00
  Assets:Savings
  ; :nobudget:

2011/01/25 Tom's Used Cars
  Expenses:Auto                    $ 5,500.00
  ; :nobudget:
  Assets:Checking

2011/01/27 Book Store
  Expenses:Books                       $20.00
  Liabilities:MasterCard

2011/04/25 Tom's Used Cars
  Expenses:Auto                    $ 5,500.00
  ; :nobudget:
  Assets:Checking

2011/04/27 Bookstore
  Expenses:Books                       $20.00
  Assets:Checking

2011/12/01 Sale
  Assets:Checking                     $ 30.00
  Income:Sales
")


(defun ledger-tests-reset-custom-values (group)
  "Reset custom variables from GROUP to standard value."
  (let ((members (custom-group-members group nil)))
    (dolist (member members)
      (cond ((eq (cadr member) 'custom-group)
             (ledger-tests-reset-custom-values (car member)))
            ((eq (cadr member) 'custom-variable)
             (custom-reevaluate-setting (car member)))))))


(defmacro ledger-tests-with-temp-file (contents &rest body)
  ;; from python-tests-with-temp-file
  "Create a `ledger-mode' enabled file with CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
always located at the beginning of buffer."
  (declare (indent 1) (debug t))
  `(let* ((temp-file (make-temp-file "ledger-tests-"))
          (ledger-buffer (find-file-noselect temp-file)))
     (unwind-protect
         (with-current-buffer ledger-buffer
           (switch-to-buffer ledger-buffer)    ; this selects window
           (ledger-mode)
           (insert ,contents)
           (goto-char (point-min))
           ,@body)
       (and ledger-buffer (kill-buffer ledger-buffer))
       (ledger-tests-reset-custom-values 'ledger))))


(defun ledger-test-visible-buffer-string ()
  "Same as `buffer-string', but excludes invisible text."
  (ledger-test-visible-buffer-substring (point-min) (point-max)))


(defun ledger-test-visible-buffer-substring (start end)
  "Same as `buffer-substring', but excludes invisible text.
The two arguments START and END are character positions."
  (let (str)
    (while (< start end)
      (let ((next-pos (next-char-property-change start end)))
        (when (not (invisible-p start))
          (setq str (concat str (buffer-substring start next-pos))))
        (setq start next-pos)))
    str))


(provide 'test-helper)

;;; test-helper.el ends here
