;;; aa-defs.el --- aa basic definitions  -*- lexical-binding:t -*-

;; Copyright (C) 2011-2021  The Authors

;; Author: The Authors

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

;;; Code:

(defgroup aa nil "elisp package archive self officiator" :group 'applications)

(defconst aa-defs-call-process-buffer-name "*aa-call-process*")
(defsubst aa-defs-call-process (command &optional destination)
  (cl-destructuring-bind (program &rest args)
      (split-string command)
    (apply #'call-process
  	   program
  	   nil
           (or destination
  	       (list (get-buffer-create aa-defs-call-process-buffer-name) t))
  	   nil
  	   args)))

(defcustom aa-defs-debug nil
  "Governs some print statements in the admin code."
  :group 'aa
  :type 'boolean)

(defcustom aa-defs-install-dir (locate-user-emacs-file "aa")
  "Base of aa operations."
  :type 'directory
  :set (lambda (symbol value)
         (set-default symbol value)
         (make-directory value t))
  :group 'aa)

(defconst aa-defs-toplevel-dir aa-defs-install-dir
  "TODO, get rid of this.")

(defmacro aa-defs-sling (&rest args)
  (declare (indent defun))
  `(directory-file-name
    (mapconcat #'file-name-as-directory
	       (list ,@args)
	       "")))

(defun aa-defs-message (&rest args)
  (when aa-defs-debug (apply #'message args)))

(defmacro aa-defs-let-token-dir (token-dir host &rest body)
  (declare (indent defun))
  `(let* ((data-home (or (getenv "XDG_DATA_HOME") (expand-file-name "~/.local/share")))
          (,token-dir (aa-defs-sling data-home "aa" (symbol-name ,host))))
     (unless (file-directory-p ,token-dir)
       (make-directory ,token-dir t))
     (unless (equal "700" (format "%o" (file-modes ,token-dir)))
       (set-file-modes ,token-dir #o700))
     ,@body))

(defun aa-defs-form-from-file-contents (filename)
  (ignore-errors
    (with-temp-buffer
      (save-excursion (insert-file-contents filename))
      (read (current-buffer)))))

(provide 'aa-defs)
;;; aa-defs.el ends here
