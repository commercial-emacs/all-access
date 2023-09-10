;;; test-aa.el --- Tests for aa -*- lexical-binding: t; -*-

;; Copyright (C) 2021 The Authors of aa.el

;; Authors: dickmao <github id: dickmao>
;; URL: https://github.com/dickmao/aa

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

;; Test stuff.

;;; Code:

(require 'aa-disc)
(require 'ert)
(require 'tar-mode)
(require 'use-package)

(defmacro test-aa-for-mock (testdir &rest body)
  (declare (indent defun))
  `(dolist (mock '("" "mockhub.com/package.git" "mockhub.com/package-dot.git" "mockhub.com/package-dot-dopple.git"))
     (let ((default-directory (aa-defs-sling ,testdir mock)))
       ,@body)))

(defconst test-aa-toplevel-dir
  (with-temp-buffer
    (if (zerop (call-process "git" nil t nil "rev-parse" "--show-toplevel"))
        (string-trim (buffer-string))
      (error "test-aa: not in a git directory"))))

(cl-defmacro test-aa--doit (&rest body &key specs &allow-other-keys)
  (declare (indent defun))
  `(unwind-protect
       (let* ((specs (or ,specs
			 `(("utest" :url ,(aa-defs-sling "mockhub.com/package.git") :files ("lisp/*.el" (:exclude "lisp/ptest.el")))
			   ("ptest" :url ,(aa-defs-sling "mockhub.com/package.git") :files ("lisp/*.el" (:exclude "lisp/utest.el"))))))
	      (aa-defs-toplevel-dir
	       (expand-file-name "test" test-aa-toplevel-dir))
	      (default-directory aa-defs-toplevel-dir)
	      (user-emacs-directory default-directory)
	      (package-user-dir (locate-user-emacs-file "elpa"))
	      use-package-ensure-function
	      aa-defs-install-dir
	      package-alist
	      package-activated-list
	      package-archives
	      package-archive-contents
	      (package-directory-list
	       (eval (car (get 'package-directory-list 'standard-value))))
	      (package-load-list
	       (eval (car (get 'package-load-list 'standard-value))))
	      (package-gnupghome-dir (expand-file-name "gnupg" package-user-dir)))
	 (test-aa-for-mock default-directory
	   (delete-directory ".git" t)
	   (with-temp-buffer
	     (unless (zerop (aa-admin--call t "git" "init"))
	       (error "%s (init): %s" default-directory (buffer-string))))
	   (with-temp-buffer
	     (unless (zerop (aa-admin--call t "git" "config" "user.name" "kilroy"))
	       (error "%s (name): %s" default-directory (buffer-string))))
	   (with-temp-buffer
	     (unless (zerop (aa-admin--call t "git" "config" "user.email" "kilroy@wuz.here"))
	       (error "%s (email): %s" default-directory (buffer-string))))
	   (with-temp-buffer
	     (unless (zerop (aa-admin--call t "git" "add" "."))
	       (error "%s (add): %s" default-directory (buffer-string))))
	   (with-temp-buffer
	     (unless (zerop (aa-admin--call t "git" "commit" "-am" "initial commit"))
	       (error "%s (commit): %s" default-directory (buffer-string)))))
	 (customize-set-variable 'aa-defs-install-dir (locate-user-emacs-file "aa"))
	 (customize-set-variable 'use-package-ensure-function
				 'aa-use-package-ensure-function)
	 (delete-directory package-user-dir t)
	 (make-directory package-user-dir t)
	 (delete-directory aa-admin--build-dir t)
	 (cl-letf (((symbol-function 'aa-query-get-spec)
		    (lambda (name)
		      (assoc (if (symbolp name)
				 (symbol-name name)
			       name)
			     specs))))
	   ,@body))
     (test-aa-for-mock (expand-file-name "test" test-aa-toplevel-dir)
       (delete-directory ".git" t))))

(ert-deftest test-aa-basic ()
  (test-aa--doit t))

(ert-deftest test-aa-build ()
  (test-aa--doit
    (aa-admin-for-pkg 'utest
      (aa-admin-batch-fetch)
      (aa-admin-batch-build)
      (should (file-directory-p (aa-defs-sling aa-admin--build-dir "utest/lisp")))
      (with-temp-buffer
	(insert-file-contents-literally
	 (aa-defs-sling aa-admin--archive-dir "utest-0.5.0.tar"))
	(tar-mode)
	(should (cl-some (lambda (descriptor)
			   (string= "utest-pkg.el"
				    (file-name-nondirectory (tar-header-name descriptor))))
			 tar-parse-info))
	(should-not (cl-some (lambda (descriptor)
			       (string= "ptest.el"
					(file-name-nondirectory (tar-header-name descriptor))))
			     tar-parse-info))))
    (aa-admin-for-pkg 'ptest
      (aa-admin-batch-fetch)
      (aa-admin-batch-build)
      (with-temp-buffer
	(insert-file-contents-literally
	 (aa-defs-sling aa-admin--archive-dir "ptest-0.5.0.tar"))
	(tar-mode)
	(should (cl-some (lambda (descriptor)
			   (string= "ptest-pkg.el"
				    (file-name-nondirectory (tar-header-name descriptor))))
			 tar-parse-info))
	(should-not (cl-some (lambda (descriptor)
			       (string= "utest.el"
					(file-name-nondirectory (tar-header-name descriptor))))
			     tar-parse-info))))))

(ert-deftest test-aa-fetch ()
  (test-aa--doit
    (should-error (aa-admin-for-pkg 'test (aa-admin-batch-fetch)))
    (aa-admin-for-pkg 'ptest (aa-admin-batch-fetch))))

(ert-deftest test-aa-install ()
  (test-aa--doit
    (aa-install "utest")))

(ert-deftest test-aa-find-library-name ()
  "Test milky-locate for lossy recipes."
  (let* ((local-dir (make-temp-file "aa" t))
         (local-specs `(("utest" :url ,local-dir :files (:defaults "lisp/*.el"))))
         (local-file (aa-defs-sling "lisp" "foo.el")))
    (unwind-protect
        (test-aa--doit
          :specs local-specs
          (make-directory (aa-defs-sling local-dir "lisp"))
          (with-temp-file (expand-file-name local-file local-dir) (insert "foo"))
          (should (equal local-file
                         (aa-admin--find-file (car local-specs)
                                                  (plist-get (cdr (car local-specs)) :url)
                                                  "foo.el"))))
      (delete-directory local-dir t))))

;; Says ert-deftest:
;; Macros in BODY are expanded when the test is defined, not when it
;; is run.  If a macro (possibly with side effects) is to be tested,
;; it has to be wrapped in `(eval (quote ...))'.
;; This is what Patrice O'Neal would call "tricky sh_t"
(ert-deftest test-aa-use-package-ensure ()
  (test-aa--doit
    (should-not (package-installed-p 'utest))
    (eval (quote (use-package utest :ensure t)))
    (should (package-installed-p 'utest))))

(ert-deftest test-aa-purge ()
  (test-aa--doit
   (aa-purge)))

(ert-deftest test-aa-ghub-unchanged ()
  (cl-flet ((ws (s) (replace-regexp-in-string "\\s-" "" s)))
    (cl-letf (((symbol-function 'ghub--retrieve)
	       (lambda (_arg req)
		 (ghub--graphql-req-query-str req)))
	      (aa-disc-hosts '(github gitlab)))
      (test-aa--doit
	(let ((query (aa-disc--query-project 'github "foo/bar" #'ignore nil)))
	  (should (equal (ws query)
			 (ws "query {
  repository (
    name: \"bar\",
    owner: \"foo\") {
    id
    nameWithOwner
    url
    pushedAt
    description
    stargazers {
      totalCount
    }
    defaultBranchRef {
      name
    }
  }
}"))))

	(let ((query (aa-disc--query-project 'gitlab "foo/bar" #'ignore nil))
	      (readmes (mapconcat
			#'cl-prin1-to-string
			(cl-mapcan
			 (lambda (u)
			   (list
			    u
			    (concat (capitalize (file-name-sans-extension u))
				    (file-name-extension u t))
			    (concat (upcase (file-name-sans-extension u))
				    (file-name-extension u t))))
			 aa-disc--readme-filenames)
			" ")))
	  (should (equal (ws query)
			 (ws (format "query {
  project (
    fullPath: \"foo/bar\") {
    id
    nameWithOwner: fullPath
    url: httpUrlToRepo
    pushedAt: lastActivityAt
    description
    stargazers: starCount
    defaultBranchRef:
    repository {
      rootRef
    }
    readme:
    repository {
      blobs (
        paths: [%s]) {
        nodes {
          rawTextBlob
        }
      }
    }
  }
}" readmes)))))))))

(provide 'test-aa)

;;; test-aa.el ends here
