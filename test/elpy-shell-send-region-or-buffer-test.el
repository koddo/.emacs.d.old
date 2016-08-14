(ert-deftest elpy-shell-send-region-or-buffer-should-send-buffer-without-region ()
  (elpy-testcase ()
    (python-mode)
    (elpy-mode)
    (insert "print('Whole buffer sent')\n")
    (elpy-shell-send-region-or-buffer)
    (should (string-match "Whole buffer sent"
                          (with-current-buffer "*Python*"
                            (elpy/wait-for-output "Whole")
                            (buffer-string))))))

(ert-deftest elpy-shell-send-region-or-buffer-should-send-region-if-active ()
  (elpy-testcase ()
    (python-mode)
    (elpy-mode)
    (insert "print('Whole buffer sent')\n"
            "print('Only region sent')\n")
    (goto-char (point-min))
    (forward-line 1)
    (elpy/mark-region (line-beginning-position)
                      (point-max))
    (elpy-shell-send-region-or-buffer)
    (let ((output (with-current-buffer "*Python*"
                    (elpy/wait-for-output "Only")
                    (buffer-string))))
      (should (string-match "Only region sent" output))
      (should (not (string-match "Whole buffer sent" output))))))

(ert-deftest elpy-shell-send-region-or-buffer-should-display-but-not-select-buffer ()
  (elpy-testcase ()
    (python-mode)
    (elpy-mode)
    (insert "print('Yay')\n")
    (elpy-shell-send-region-or-buffer)
    (should (get-buffer-window "*Python*"))
    (should (not (equal (current-buffer)
                        (get-buffer "*Python*"))))))

(ert-deftest elpy-shell-send-region-or-buffer-should-notify-of-removing-main ()
  (elpy-testcase ()
    (python-mode)
    (elpy-mode)
    (insert "def foo():\n"
            "  pass\n"
            "\n"
            "if __name__ == '__main__':\n"
            "  print('Argh')\n")
    (elpy-shell-send-region-or-buffer)
    (string-match "Removed if __main__"
                  (if (fboundp 'shut-up-current-output)
                      (shut-up-current-output)
                    (with-current-buffer ert-runner-output-buffer
                      (buffer-string))))))
