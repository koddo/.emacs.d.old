;;; test-quickrun.el ---

;; Copyright (C) 2012 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL:
;; Version: 0.01

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'ert)
(require 'quickrun)

(ert-deftest quickrun:exec-quickrun ()
  "Exec `quickrun'"
  (let ((buf (find-file-noselect "sample/sample.py")))
   (with-current-buffer buf
     (quickrun))
   ;; quickrun is async function
   (sleep-for 1)
   (with-current-buffer "*quickrun*"
     (let ((str (buffer-substring-no-properties (point-min) (point-max))))
       (should (string= "Hello Python quickrun.el\n" str))))))

(ert-deftest quickrun:add-command ()
  "Add new command"
  (quickrun-add-command "-test"
                        '((:command . "test foo")
                          (:description . "test description")))
  (let ((params (assoc-default "-test" quickrun/language-alist)))
    (should params)
    (let ((command (assoc-default :command params))
          (desc (assoc-default :description params)))
      (should (string= command "test foo"))
      (should (string= desc "test description")))))

(ert-deftest quickrun:override-configuration ()
  "Override registerd command"
  (quickrun-add-command "c/gcc"
                        '((:command . "clang")
                          (:description . "Compile clang"))
                        :override t)
  (let* ((params (assoc-default "c/gcc" quickrun/language-alist))
         (command (assoc-default :command params)))
    (should (string= command "clang"))))

;;; test-quickrun.el ends here
