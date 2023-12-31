;;; aa-milky.el --- cadged bits from milkypostman et al  -*- lexical-binding:t -*-

;; Copyright (C) 2011-2021  The Authors

;; Author: The Authors
;; Based on code written by Cercle du Lait <dcurtis@milkbox.net> and friends
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Of particular value is melpa's sense for what file patterns belong in a package.

;;; Code:

(require 'cl-lib)

(defconst aa-milky-default-files-spec
  '("*.el" "lisp/*.el"
    "dir" "*.info" "*.texi" "*.texinfo"
    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
    "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
    (:exclude
     ".dir-locals.el" "lisp/.dir-locals.el"
     "test.el" "tests.el" "*-test.el" "*-tests.el"
     "lisp/test.el" "lisp/tests.el" "lisp/*-test.el" "lisp/*-tests.el"))
  "Default value for :files attribute in specs.")

(defun aa-milky-expand-file-specs (dir specs &optional subdir allow-empty)
  "In DIR, expand SPECS, optionally under SUBDIR.
The result is a list of (SOURCE . DEST), where SOURCE is a source
file path and DEST is the relative path to which it should be copied.

If the resulting list is empty, an error will be reported.  Pass t
for ALLOW-EMPTY to prevent this error."
  (let ((default-directory dir)
        (prefix (if subdir (format "%s/" subdir) ""))
        (lst))
    (dolist (entry specs)
      (setq lst
            (if (consp entry)
                (if (eq :exclude (car entry))
                    (cl-nset-difference lst
                                        (aa-milky-expand-file-specs
                                         dir (cdr entry) nil t)
                                        :key 'car
                                        :test 'equal)
                  (nconc lst
                         (aa-milky-expand-file-specs
                          dir
                          (cdr entry)
                          (concat prefix (car entry))
                          t)))
              (nconc
               lst (mapcar (lambda (f)
                             (cons f
                                   (concat prefix
                                           (replace-regexp-in-string
                                            "\\.el\\.in\\'"
                                            ".el"
                                            (file-name-nondirectory f)))))
                           (file-expand-wildcards entry))))))
    (when (and (null lst) (not allow-empty))
      (error "No matching file(s) found in %s: %s" dir specs))
    lst))

(defun aa-milky-config-file-list (file-list)
  (cond
   ((null file-list)
    aa-milky-default-files-spec)
   ((or (eq :defaults (car file-list)) (equal "defaults" (car file-list)))
    (append aa-milky-default-files-spec (cdr file-list)))
   (t
    file-list)))

(defun aa-milky-locate (dir el files)
  (let* ((specs (aa-milky-config-file-list files))
         (unsorted (mapcar #'car (aa-milky-expand-file-specs dir specs nil t)))
         (sorted (cl-sort unsorted (lambda (x y) (< (length x) (length y))))))
    (cl-find-if (lambda (f) (and (equal (file-name-nondirectory f) el)
                                 (file-exists-p (expand-file-name f dir))))
                sorted)))

(provide 'aa-milky)
;;; aa-milky.el ends here
