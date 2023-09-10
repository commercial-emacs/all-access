;;; aa.el --- Emacs lisp package archive self officiator  -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2021 The Authors of aa.el

;; Authors: Commercial Emacs <github id: commercial-emacs>
;; Version: 0.1.0
;; Keywords: maint tools
;; URL: https://commandlinesystems.com
;; Package-Requires: ((emacs "26.1") (ghub) (web-server "0.1.3pre"))

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with aa.el.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Install any package. Official source including commit history at `gitawonk.com`_.
;;
;; ``M-x aa``
;;   Specifying owner/package, e.g., ``magnars/dash.el`` yields the desired github
;;   repo.  More conveniently, enter free-form keywords to conduct a github search.
;;
;; ``M-x aa-query``
;;   Manage your subscription.
;;
;; ``M-x aa-install``
;;   Enter the package name to install or reinstall.
;;
;; ``M-x aa-delete``
;;   Enter the package name to delete.
;;
;; .. _gitawonk.com: https://gitawonk.com/dickmao/all-access

;;; Code:

(require 'package)
(require 'aa-admin)

(defvar ert--running-tests)

;;;###autoload
(defun aa-delete (package &optional force nosave)
  "Merely calls `package-delete' but spares guff about dependencies."
  (interactive
   (let* ((package-table
           (mapcar
            (lambda (p) (cons (package-desc-full-name p) p))
            (delq nil
                  (mapcar (lambda (p) (unless (package-built-in-p p) p))
                          (apply #'append (mapcar #'cdr package-alist))))))
          (package-name (completing-read "Delete package: "
                                         (mapcar #'car package-table)
                                         nil t)))
     (list (assoc-default package-name package-table)
           current-prefix-arg nil)))
  (let ((cease-and-desist
	 (lambda (args)
	   (setf (nthcdr 1 args) (cons t (nthcdr 2 args)))
	   args)))
    (unwind-protect
	(progn
	  (add-function :filter-args (symbol-function 'package-delete) cease-and-desist)
	  (package-delete package force nosave))
      (remove-function (symbol-function 'package-delete) cease-and-desist))))

;;;###autoload
(defun aa-install (package)
  "Fetch and install PACKAGE directly from git forges."
  (interactive (list (completing-read
                      "Package: "
                      (aa-query-get-package-names)
                      nil t nil)))
  (when (symbolp package) (setq package (symbol-name package)))
  (aa-admin-for-pkg package (aa-admin-batch-install))
  (unless (bound-and-true-p ert--running-tests)
    (package-menu--post-refresh)))

;;;###autoload
(defun aa-purge ()
  "Purge residual git worktrees and references, and still-born packages.
Will not delete the backups subdirectory."
  (interactive)
  (aa-admin-purge))

(declare-function use-package-as-symbol "use-package-core")

;;;###autoload
(defalias 'aa-use-package-ensure-function
  (lambda (name args _state)
    "Hook into the use-package ensure subsystem."
    (dolist (ensure args)
      (when-let ((package
		  (or (and (eq ensure t) (use-package-as-symbol name))
		      ensure)))
	(when (consp package)
          (setq package (car package)))
        (unless (package-installed-p package)
          (aa-install package))))))

(when (equal aa-defs-toplevel-dir aa-defs-install-dir)
  (let ((default-directory aa-defs-toplevel-dir))
    (if (not (executable-find "git"))
        (display-warning 'aa "git program not found" :error)
      (unless (zerop (aa-admin--call nil "git" "rev-parse" "--show-toplevel"))
        (with-temp-buffer
          (unless (zerop (aa-admin--call t "git" "init" "--bare"))
            (error "aa abort: %s" (buffer-string))))))))

(provide 'aa)
;;; aa.el ends here
