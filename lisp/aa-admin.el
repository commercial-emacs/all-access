;;; aa-admin.el --- aa guts based on elpa-admin.el  -*- lexical-binding:t -*-

;; Copyright (C) 2011-2021 The Authors

;; Author: The Authors
;; Based on code written by Stefan Monnier <monnier@iro.umontreal.ca>

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

;; In the interest of code coverage, I gutted most of smonnier's elpa-admin.el.
;; Ideally, we converge to something that can be used for self-officiating
;; and remote-officiating package archives (gallery: that's not going to happen).

;;; Code:

(require 'cl-lib)
(require 'lisp-mnt)
(require 'package)
(require 'message)
(require 'aa-defs)
(require 'aa-milky)
(require 'aa-query)
(require 'subr-x)

(defvar aa-admin-too-big-to-fail nil
  "Work around melpa guy's delusion about github tags.")

(defsubst aa-admin--normalize-url (url)
  (url-recreate-url (url-generic-parse-url
                     (replace-regexp-in-string "\\.git$" "" url))))

(defconst aa-admin--ref-master-dir "refs/remotes/master")

(defvar aa-admin--sandbox nil
  "If non-nil, run some of the less trusted commands in a sandbox.
This is recommended when building packages from untrusted sources,
but this requires Bubblewrap to be installed and has only been tested
on some Debian systems.")

(defconst aa-admin--build-dir "packages")
(defconst aa-admin--archive-dir "archive")

(defsubst aa-admin--spec-get (pkg-spec prop &optional default)
  (or (plist-get (cdr pkg-spec) prop) default))

(defmacro aa-admin-for-pkg (name &rest body)
  (declare (indent defun))
  `(let ((name* (if (symbolp ,name) (symbol-name ,name) ,name)))
     ,@(mapcar (lambda (expr)
                 `(let ((command-line-args-left (list name*)))
                    ,expr))
               body)))

(defconst aa-admin--re-no-dot "\\`\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*"
  "Regular expression matching all files except \".\" and \"..\".")

(defun aa-admin--convert-require (elt)
  (let ((vers (version-to-list (car (cdr elt)))))
    (if vers
        (list (car elt) vers)
      (list (car elt)))))

(defun aa-admin--dirname (dir &optional base)
  (file-name-as-directory (expand-file-name dir base)))

(defun aa-admin--main-file (pkg-spec dir)
  (cl-flet ((get (what) (aa-admin--spec-get pkg-spec what)))
    (let* ((name (car pkg-spec))
           (el (if (equal "el" (file-name-extension name))
                   name
                 (format "%s.el" name)))
           (main-file (get :main-file))
           (lisp-dir (get :lisp-dir))
           (files (get :files)))
      (cond (main-file main-file)
            (lisp-dir (concat (file-name-as-directory lisp-dir) el))
            (t (aa-milky-locate dir el files))))))

(defun aa-admin--pkg-file (pkg-spec dir)
  (let* ((name (car pkg-spec))
         (el (format "%s-pkg.el" name))
         (files (aa-admin--spec-get pkg-spec :files)))
    (aa-milky-locate dir el files)))

(defun aa-admin--find-file (pkg-spec dir file)
  (cl-flet ((get (what) (aa-admin--spec-get pkg-spec what)))
    (let ((lisp-dir (get :lisp-dir))
          (files (get :files)))
      (cond (lisp-dir (expand-file-name
                       (concat (file-name-as-directory lisp-dir) file)
                       dir))
            (t (aa-milky-locate dir file files))))))

(defun aa-admin--refspec (pkg-spec)
  (let ((ref-type (if (aa-admin--spec-get pkg-spec :tag) "tags" "heads")))
    (cl-destructuring-bind (ref-dir . ref-name)
        (aa-admin--ref-master-components pkg-spec)
      (if (string= "HEAD" ref-name)
          (format "+HEAD:%s/%s" ref-dir ref-name)
        (format "+refs/%s/%s:%s/%s" ref-type ref-name ref-dir ref-name)))))

(defun aa-admin--ref-master-components (pkg-spec)
  "Return cons of (REF-DIR . REF-NAME).
The path REF-DIR/REF-NAME describes the full master ref.  We
return both parts separately since REF-NAME could contain
slashes (e.g., a branch with slash characters), and would thus be
impossible to recover from a single path."
  (let ((name (or (aa-admin--spec-get pkg-spec :release-branch)
                  (aa-admin--spec-get pkg-spec :branch)
                  (aa-admin--spec-get pkg-spec :tag)
                  "HEAD"))
        (what (car pkg-spec)))
    (cons (aa-defs-sling aa-admin--ref-master-dir
            (if (symbolp what) (symbol-name what) what))
          name)))

(defun aa-admin--ref-master (pkg-spec)
  (cl-destructuring-bind (ref-dir . ref-name)
      (aa-admin--ref-master-components pkg-spec)
    (format "%s/%s" ref-dir ref-name)))

(defun aa-admin--build-tar-transform (name r)
  (let ((from (nth 0 r)) (to (nth 1 r)))
    (cl-assert (not (string-match "[][*\\|?]" from)))
    (cl-assert (not (string-match "[][*\\|?]" to)))
    (format "--transform=s|^packages/%s/%s|packages/%s/%s|"
            name
            (if (string-match "/\\'" from)
                (concat (substring from 0 -1) "\\($\\|/\\)")
              (concat from "$"))
            name to)))

(defun aa-admin--temp-file (f)
  (when (boundp 'aa-admin--temp-files)
    (push (if (stringp f) (expand-file-name f) f) aa-admin--temp-files)))

(defmacro aa-admin--check-apply (f &rest body)
  (declare (indent defun))
  `(with-temp-buffer
     (let ((ret (apply (function ,f) `(,(current-buffer) t) ,@body)))
       (unless (zerop ret)
	 (error "aa-admin--check-apply (%s): %s\n%s"
		ret
		(let ((raw (list ,@body)))
		  (mapconcat #'identity
			     (mapcar #'cl-prin1-to-string
				     (cl-subseq raw 0 (min 5 (length raw))))
			     " "))
		(buffer-string))))))

(defsubst aa-admin--sed-hack (s)
  "Sed obeys something called BRE (basic regex).  Plus sign isn't special."
  (replace-regexp-in-string (regexp-quote (regexp-quote "+")) "+" (regexp-quote s)))

(defun aa-admin--build-one-tarball (tarball dir pkg-spec metadata)
  "Create file TARBALL for NAME if not done yet.
Return non-nil if a new tarball was created."
  (aa-defs-message "Building tarball %s..." tarball)
  (let* ((destdir (let ((result (file-name-directory tarball)))
                    (prog1 result
                      (make-directory result t))))
         (build-dir aa-admin--build-dir)
         (name (car pkg-spec))
         (vers (nth 0 metadata))
         (elpaignore (expand-file-name ".elpaignore" dir))
         (files (aa-admin--spec-get pkg-spec :files))
         ;; three cases:
         ;; 1. registered with an elpa (:prospective absent)
         ;; 2. dot spec (:prospective got usurped)
         ;; 3. neither of the above (:prospective present)
         ;; In first two cases, do as melpa does.  In last case, do as elpa does.
         (prospective-p (and (aa-admin--spec-get pkg-spec :prospective)
                             (not (cl-assert (not files)))))
         (ignores (aa-admin--spec-get pkg-spec :ignored-files))
         (renames (aa-admin--spec-get pkg-spec :renames))
         (ldir (aa-admin--spec-get pkg-spec :lisp-dir))
         (tardir (concat (file-name-as-directory build-dir) name)))
    (when ldir
      (cl-pushnew (list (file-name-as-directory ldir) "") renames
                  :test #'equal))
    ;; Run `make' before building the Info file, so that the `make' rule
    ;; can be used to build the Info/Texinfo file.
    (aa-admin--make pkg-spec dir)
    (aa-admin--build-Info pkg-spec dir)
    (let ((pkg-file (expand-file-name (concat name "-pkg.el") dir)))
      (if (file-exists-p pkg-file)
          (aa-defs-message "Don't overwrite existing %s..." pkg-file)
        (aa-admin--write-pkg-file pkg-file name metadata)))
    (when files
      (unless (aa-admin--pkg-file pkg-spec dir)
        (push (concat name "-pkg.el") files)))
    (cl-assert (not (string-match "[][*\\|?]" name)))
    (cl-assert (not (string-match "[][*\\|?]" vers)))
    (if (or ignores renames prospective-p)
	(aa-admin--check-apply
	  aa-admin--call
	  (if (executable-find "gtar") "gtar" "tar")
	  `("--exclude-vcs"
            ,@(mapcar (lambda (i) (format "--exclude=%s/%s/%s" build-dir name i))
		      '(".git" ".dir-locals.el" ".mailmap"
                        ".github" ".travis.yml"
                        "test" "tests"))
	    ,@(cond
	       (ignores
		(mapcar (lambda (i) (format "--exclude=%s/%s/%s" build-dir name i))
			ignores))
	       ((file-readable-p elpaignore) `("-X" ,elpaignore)))
	    ,@(mapcar (lambda (r) (aa-admin--build-tar-transform name r))
		      renames)
	    "--transform"
	    ,(format "s|^%s/%s|%s-%s|" build-dir name name vers)
	    "-cf" ,tarball
	    ,tardir))
      (let* ((mapping (aa-milky-expand-file-specs
                       (expand-file-name tardir default-directory)
                       (aa-milky-config-file-list files)))
             (seds** (mapcar
                      (lambda (x)
                        (cl-destructuring-bind (s . d) x
                          (format "s|^%s/%s|%s-%s/%s|"
                                  (aa-admin--sed-hack tardir)
                                  (aa-admin--sed-hack s)
                                  name vers d)))
                      mapping))
	     (seds* (cl-sort (delete-dups seds**) (lambda (x y) (> (length x) (length y)))))
             (seds (cl-mapcan (lambda (x) (list "--transform" x)) (delete-dups seds*))))
        (aa-admin--check-apply
	  aa-admin--call-region
	  (if (executable-find "gtar") "gtar" "tar")
          (mapcar (lambda (pair) (aa-defs-sling tardir (car pair)))
                  mapping)
          `(,@seds
            "-cf" ,tarball
            "--files-from" "-"))))
    (let ((pkgdesc
           ;; FIXME: `aa-admin--write-pkg-file' wrote the metadata to
           ;; <pkg>-pkg.el and then `aa-admin--process-multi-file-package'
           ;; reads it back.  We could/should skip the middle man.
           (aa-admin--process-multi-file-package
            dir name 'dont-rename)))
      (aa-defs-message "%s: %s" name pkgdesc)
      (let ((link (expand-file-name (format "%s.tar" name) destdir)))
        (when (file-symlink-p link) (delete-file link))
        (make-symbolic-link (file-name-nondirectory tarball) link))
      (message "Built %s" tarball)
      'new)))

(defmacro aa--spin-args (action &rest in-case-error)
  (declare (indent defun))
  (setq in-case-error
	(or in-case-error `((error "%s: %s" ,(symbol-name action)
				   (error-message-string err)))))
  `(while command-line-args-left
     (let* ((pkg-name (pop command-line-args-left))
            (pkg-spec (aa-query-get-spec pkg-name)))
       (if pkg-spec
	   (condition-case-unless-debug err
               (funcall (function ,action) pkg-spec)
	     (error ,@in-case-error))
         (display-warning 'aa (format "%s: %s not found"
					  ',action (or pkg-name "")))))))

(defun aa-admin-batch-build (&rest _)
  (aa--spin-args aa-admin--build-one-package
    (display-warning
     'aa
     (format "aa-admin--build-one-package: %s: %s"
	     pkg-name (error-message-string err)) :error)))

(defun aa-admin--tidy-one-package (pkg-spec)
  (let* ((default-directory aa-defs-toplevel-dir)
	 (name (car pkg-spec))
	 (ref-master (aa-admin--ref-master pkg-spec))
         (packages-dir aa-admin--build-dir)
         (pkg-dir (expand-file-name name packages-dir))
         (metadata (aa-admin--metadata pkg-dir pkg-spec))
         (vers (nth 0 metadata))
         (tarball (format "%s-%s.tar" name vers)))
    (with-temp-buffer
      (unless (cl-every
	       #'zerop
	       (list (aa-admin--call t "git" "update-ref" "-d" ref-master)
		     (aa-admin--call t "git" "worktree" "remove" "-f" pkg-dir)))
	(aa-defs-message "aa-admin--tidy-one-package: %s" (buffer-string))))
    (delete-directory pkg-dir t)
    (let ((link (expand-file-name tarball aa-admin--archive-dir)))
      (when (or (file-symlink-p link) (file-exists-p link))
        (delete-file link)))))

(defun aa-admin--dired-size (dir)
  "https://emacswiki.org/emacs/DiredGetFileSize"
  (cl-flet* ((file-size
              (filename)
              (float (file-attribute-size (file-attributes filename))))
             (file-size-total
              (filename-list)
              (truncate (apply #'+ (mapcar #'file-size filename-list)))))
    (file-size-total (directory-files dir t aa-admin--re-no-dot t))))

(defun aa-admin-purge ()
  (interactive)
  (let* ((default-directory aa-defs-toplevel-dir)
	 (previous-size (aa-admin--dired-size default-directory)))
    (with-temp-buffer
      (save-excursion (aa-admin--call t "git" "worktree" "list" "--porcelain"))
      (cl-loop with worktrees
	       with cand
	       until (eobp)
	       for line = (split-string (string-trim (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
	       do (cond ((and (string= "worktree" (car line)) (= 2 (length line)))
			 (setq cand (cadr line)))
			((and (string= "detached" (car line)) (= 1 (length line)))
			 (push cand worktrees)))
	       do (forward-line)
	       finally do (mapc
			   (lambda (x)
			     (aa-admin--call nil "git" "worktree" "remove" "-f" x)
                             (delete-directory x t))
			   worktrees)))
    (with-temp-buffer
      (save-excursion (aa-admin--call t "git" "for-each-ref" "--format=%(refname)"
					  aa-admin--ref-master-dir))
      (cl-loop until (eobp)
	       for line = (string-trim (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
	       do (aa-admin--call nil "git" "update-ref" "-d" line)
	       do (forward-line)))
    (when (file-directory-p aa-admin--archive-dir)
      (dolist (link (directory-files aa-admin--archive-dir t ".*\\.tar\\'" t))
	(when (or (file-symlink-p link) (file-exists-p link))
          (delete-file link))))
    (apply #'aa-admin--call nil (split-string "git gc --prune=all"))
    (message "aa-admin-purge: %s bytes -> %s bytes"
	     previous-size (aa-admin--dired-size default-directory))))

(defun aa-admin-batch-tidy (&rest _)
  (aa--spin-args aa-admin--tidy-one-package))

(defun aa-admin-batch-install (&rest _)
  (aa--spin-args aa-admin--install-one-package))

(cl-defun aa-admin--install-file (name
                                      file
                                      &aux
                                      (name (if (stringp name) (intern name) name)))
  (mapc (lambda (odesc)
	  (when-let ((odir (directory-file-name (package-desc-dir odesc)))
	             (leaf (file-name-nondirectory odir))
                     (backup-dir (expand-file-name "backups" aa-defs-install-dir))
                     (backup-name (let ((backup-directory-alist `(("." . ,backup-dir))))
                                    (car (find-backup-file-name leaf)))))
            (ignore-errors (copy-directory odir backup-name t t)
                           (package-delete odesc t))))
	(cdr (assq name package-alist)))
  (let ((workaround
         (lambda (orig-args)
           "Shunt melpa's ginormous versions to best available."
           ;; NB this unnecessarily builds the best-avail dependency
           ;; because aa-admin--build-one-package couldn't have known the
           ;; author's version header without first fetching it.
           (cl-destructuring-bind (packages
                                   &optional requirements
                                   &rest rest-args
                                   &aux requirements*)
               orig-args
             (dolist (requirement requirements)
               (cl-destructuring-bind (name req-version &rest details)
                   requirement
                 (let* ((pkg-desc (car (cdr (assq name package-archive-contents))))
                        (best-avail (when pkg-desc
                                      (package-desc-version pkg-desc)))
                        (problem-p (when (and best-avail req-version)
                                     (version-list-< best-avail req-version)))
                        (melpa-p (when req-version
                                   (version-list-<= '(19001201 1) req-version)))
                        (prompt "`%s` required v%s exceeds available v%s. Proceed? ")
                        (spoof (if aa-admin-too-big-to-fail
                                   (version-to-list (number-to-string 0))
                                 (and problem-p
                                      (or melpa-p
                                          (y-or-n-p
                                           (format prompt name
                                                   (package-version-join req-version)
                                                   (package-version-join best-avail)))
                                          (top-level))
                                      best-avail))))
                   (when spoof
                     (setf (nth 1 requirement) spoof)
                     (message "aa-admin--install-file: %s min required %s -> %s"
                              name
                              (package-version-join req-version)
                              (package-version-join spoof))))
                 (push requirement requirements*)))
             (cons packages (cons (nreverse requirements*) rest-args))))))
    (unwind-protect
        (progn
          (add-function :filter-args
                        (symbol-function 'package-compute-transaction)
                        workaround)
          (package-install-file file))
      (remove-function (symbol-function 'package-compute-transaction) workaround))))

(defun aa-admin--install-one-package (pkg-spec)
  (unless package--initialized
    (package-initialize 'no-activate))
  (cl-loop
   with target = (intern (car pkg-spec))
   ;; that package-alist elements have :archive set to nil is reassuring in light
   ;; of our scoping the archive-related special variables.
   with package-archive-contents = (copy-tree package-archive-contents)
   with package-archives = (cl-remove-if (lambda (pair) (string= (car pair) "aa"))
                                         package-archives)
   with default-directory = aa-defs-toplevel-dir
   with seen = (list (list target))
   with queue = (list (list target))
   while queue
   do (cl-destructuring-bind (name
			      &optional version
			      &aux
			      (version (or version (version-to-list "0pre")))
			      (target-p (eq name target)))
	  (pop queue)
	(when (or target-p
                  (not (package-installed-p name version)))
          (unless target-p
            (aa-defs-message
             "aa-admin--install-one-package: fetching %S %S"
             name version))
          (when (eq name 'emacs)
            (error "aa-admin--install-one-package: %s requires emacs version %s"
                   (symbol-name target) (package-version-join version)))
          (aa-admin-for-pkg name
            (aa-admin-batch-fetch)
            (aa-admin-batch-build))
          ;; mutation occurs in batch-build
          (when (and target-p (aa-query-get-spec name))
            (setq pkg-spec (aa-query-get-spec name)))
          (when-let ((name-spec (if target-p
			            pkg-spec
			          (aa-query-get-spec name))))
            (let* ((name-dir (expand-file-name
			      (format "%s/%s" aa-admin--build-dir name)
			      aa-defs-toplevel-dir))
                   (main-desc
                    (when-let ((main-file (aa-admin--main-file name-spec name-dir)))
                      (with-temp-buffer
                        (insert-file-contents (expand-file-name main-file name-dir))
		        (ignore-errors (package-buffer-info)))))
	           (pkg-desc
                    (when-let ((pkg-file (aa-admin--pkg-file name-spec name-dir)))
	              (with-temp-buffer
	                (insert-file-contents (expand-file-name pkg-file name-dir))
                        (package--read-pkg-desc 'tar))))
                   (guess-desc (if (and main-desc pkg-desc)
                                   (let ((main-reqs (package-desc-reqs main-desc))
                                         (pkg-reqs (package-desc-reqs pkg-desc)))
                                     (if (> (length main-reqs) (length pkg-reqs))
                                         main-desc
                                       pkg-desc))
                                 (or main-desc pkg-desc))))
              (if guess-desc
                  (progn
	            (setf (package-desc-kind guess-desc) 'tar)
	            (setf (package-desc-archive guess-desc) "aa")
                    (setf (alist-get name package-archive-contents) (list guess-desc))
                    (mapc
	             (lambda (req)
	               (unless (memq (car req) (mapcar #'car seen))
	                 (setq queue (append queue (list req)))
	                 (push req seen)))
	             (package-desc-reqs guess-desc)))
                (message "aa-admin--install-one-package: parsing problem %s" name))))))
   finally do
   (let* ((dir (expand-file-name (symbol-name target) aa-admin--build-dir))
          (metadata (aa-admin--metadata dir pkg-spec))
          (vers (nth 0 metadata))
          (tarball (expand-file-name
                    (format "dist/%s-%s.tar" target vers)
                    dir)))
     (add-to-list 'package-archives `("aa" . ,(file-name-as-directory (expand-file-name aa-admin--archive-dir aa-defs-toplevel-dir))))
     (if (file-readable-p tarball)
         (progn
           (aa-admin--install-file target tarball)
	   (unless aa-defs-debug
	     (dolist (dep (mapcar #'car seen))
	       (if (eq dep target)
	           (ignore-errors (aa-admin--tidy-one-package pkg-spec))
		 (when-let ((to-tidy (aa-query-get-spec dep)))
	           (ignore-errors (aa-admin--tidy-one-package to-tidy)))))))
       (error "aa-admin--install-one-package: %s not found" tarball)))))

(defun aa-admin--build-one-package (pkg-spec)
  (if (eq (nth 1 pkg-spec) :core)
      (error "aa-admin--build-one-package: core unhandled")
    (let* ((name (car pkg-spec))
           (packages-dir aa-admin--build-dir)
           (pkg-dir (expand-file-name name packages-dir)))
      (make-directory packages-dir t)
      (aa-admin--worktree-sync pkg-spec pkg-dir)))
  (let* ((default-directory aa-defs-toplevel-dir)
         (name (car pkg-spec))
         (dir (expand-file-name name aa-admin--build-dir))
         (metadata (aa-admin--metadata dir pkg-spec))
         (vers (nth 0 metadata)))
    ;; First, try and build the devel tarball
    ;; Do it before building the release tarball, because building
    ;; the release tarball may revert to some older commit.
    (let* ((tarball (format "%s-%s.tar" name vers))
           (tarpath (expand-file-name tarball (aa-defs-sling dir "dist")))
           (archive-path (expand-file-name tarball aa-admin--archive-dir)))
      (aa-admin--build-one-tarball tarpath dir pkg-spec metadata)
      (make-directory (file-name-directory archive-path) t)
      (when (or (file-symlink-p archive-path) (file-exists-p archive-path))
        (delete-file archive-path))
      (make-symbolic-link (aa-defs-sling "../packages" name "dist" tarball)
                          archive-path))))

(defun aa-admin--call (destination program &rest args)
  "Like ‘call-process’ for PROGRAM, DESTINATION, ARGS.
The INFILE and DISPLAY arguments are fixed as nil."
  (aa-defs-message "call-process %s %s" program args)
  (apply #'call-process program nil destination nil args))

(defun aa-admin--call-region (destination program files-from &rest args)
  "Like ‘call-process’ for PROGRAM, DESTINATION, ARGS.
The INFILE and DISPLAY arguments are fixed as nil."
  (let ((manifest (mapconcat #'identity files-from "\n")))
    (aa-defs-message "call-process-region %s %s %s" program manifest args)
    (with-temp-buffer
      (save-excursion (insert manifest))
      (apply #'call-process-region (point-min) (point-max) program nil destination nil args))))

(defconst aa-admin--bwrap-args
  '("--unshare-all"
    "--dev" "/dev"
    "--proc" "/proc"
    "--tmpfs" "/tmp"))

(defvar aa-admin--sandboxed-ro-binds
  '("/lib" "/lib64" "/bin" "/usr" "/etc/alternatives" "/etc/emacs"))

(defun aa-admin--call-sandboxed (destination &rest args)
  "Like ‘aa-admin--call’ but sandboxed.
More specifically, uses Bubblewrap such that the command is
confined to only have write access to the `default-directory'.
Signal an error if the command did not finish with exit code 0."
  (if (not aa-admin--sandbox)
      (apply #'aa-admin--call destination args)
    (aa-defs-message "call-sandboxed %s" args)
    (let ((dd (expand-file-name default-directory))) ;No `~' allowed!
      (setq args (nconc `("--bind" ,dd ,dd) args)))
    ;; Add read-only dirs in reverse order.
    (dolist (b aa-admin--sandboxed-ro-binds)
      (when (file-exists-p b)         ;`brwap' burps on binds that don't exist!
        (setq b (expand-file-name b))
        (setq args (nconc `("--ro-bind" ,b ,b) args))))
    (let ((exitcode
           (apply #'aa-admin--call destination "bwrap"
                  (append aa-admin--bwrap-args args))))
      (unless (eq exitcode 0)
        (if (eq destination t)
            (error "Error-indicating exit code in aa-admin--call-sandboxed:\n%s"
                   (buffer-string))
          (error "Error-indicating exit code in aa-admin--call-sandboxed"))))))

(defun aa-admin--override-version (pkg-spec orig-fun header)
  (let ((version-map (plist-get (cdr pkg-spec) :version-map))
	(dont-release (plist-get (cdr pkg-spec) :dont-release))
	(str (funcall orig-fun header)))
    (or (when (or (equal header "version")
		  (and str (equal header "package-version")))
	  (or (cadr (assoc str version-map))
              (and str dont-release
		   (string-match dont-release str)
		   (replace-match "snapshot" t t str))
	      str
	      "0pre"))
	str)))

;; Some packages use version numbers which `version-to-list' doesn't
;; recognize out of the box.  So here we help.

(add-to-list 'version-regexp-alist '("^[-.+ ]*beta-?$" . -2)) ;"1.0.0-beta-3"
(add-to-list 'version-regexp-alist '("^[-.+ ]*dev$" . -4))    ;2.5-dev

(defun aa-admin--metadata (dir pkg-spec)
  "Return a list (VERSION DESCRIPTION REQ EXTRAS),
VERSION is the version string of the simple package;
DESCRIPTION is the brief description of the package;
REQ is a list of requirements;
EXTRAS is an alist with additional metadata.

PKG is the name of the package and DIR is the directory where it is."
  (let* ((main-file* (aa-admin--main-file pkg-spec dir))
	 (main-file (when main-file* (expand-file-name main-file* dir))))
    (unless (and main-file (file-exists-p main-file))
      (error "Can't find main file %s" main-file))
    (let (pkg-version)
      (when-let ((pkg-file* (aa-admin--pkg-file pkg-spec dir))
                 (pkg-file (expand-file-name pkg-file* dir))
                 (exp (aa-defs-form-from-file-contents pkg-file))
                 (def-p (eq (car-safe exp) 'define-package))
                 (pkg-desc (apply #'package-desc-from-define (cdr exp)))
                 (version (package-desc-version pkg-desc)))
        (setq pkg-version (package-version-join version)))
      (with-temp-buffer
	(insert-file-contents main-file)
	(goto-char (point-min))
	(let* ((advice (apply-partially
			#'aa-admin--override-version
			pkg-spec))
	       (pkg-desc
		(unwind-protect
                    (progn
		      (advice-add #'lm-header :around advice)
                      (package-buffer-info))
                  (advice-remove #'lm-header advice)))
               (extras (package-desc-extras pkg-desc))
               (version (if (and pkg-version
				 (version-list-< (package-desc-version pkg-desc)
						 (version-to-list pkg-version)))
                            (version-to-list pkg-version)
                          (package-desc-version pkg-desc)))
               (keywords (lm-keywords-list))
               (found-keywords (alist-get :keywords extras)))
          (when (and keywords (not found-keywords))
            ;; Using an old package-buffer-info which doesn't include
            ;; keywords.  Fix it by hand.
            (push (cons :keywords keywords) extras))
          (list (package-version-join version)
		(package-desc-summary pkg-desc)
		(package-desc-reqs pkg-desc)
		extras))))))

(defun aa-admin--alist-to-plist-args (alist)
  (mapcar (lambda (x)
            (if (and (not (consp x))
                     (or (keywordp x)
                         (not (symbolp x))
                         (memq x '(nil t))))
                x `',x))
          (apply #'nconc
                 (mapcar (lambda (pair) (list (car pair) (cdr pair))) alist))))

(defun aa-admin--plist-args-to-alist (plist)
  (let (alist)
    (while plist
      (let ((value (cadr plist)))
        (when value
          (cl-assert (keywordp (car plist)))
          (push (cons (car plist)
                      (if (eq 'quote (car-safe value)) (cadr value) value))
                alist)))
      (setq plist (cddr plist)))
    alist))

(defun aa-admin--process-multi-file-package (dir pkg &optional dont-rename)
  "Deploy the contents of DIR into the archive as a multi-file package.
Rename DIR/ to PKG-VERS/, and return the descriptor."
  (let* ((exp (aa-admin--multi-file-package-def dir pkg))
	 (vers (nth 2 exp))
         (req-exp (nth 4 exp))
	 (req (mapcar #'aa-admin--convert-require
                      (if (eq 'quote (car-safe req-exp)) (nth 1 req-exp)
                        (when req-exp
                          (error "REQ should be a quoted constant: %s"
                                 req-exp)))))
         (extras (aa-admin--plist-args-to-alist (nthcdr 5 exp))))
    (unless (equal (nth 1 exp) pkg)
      (error "Package name %s doesn't match file name %s"
	     (nth 1 exp) pkg))
    (unless dont-rename (rename-file dir (concat pkg "-" vers)))
    (cons (intern pkg) (vector (version-to-list vers)
                               req (nth 3 exp) 'tar extras))))

(defun aa-admin--multi-file-package-def (dir pkg)
  "Return the `define-package' form in the file DIR/PKG-pkg.el."
  (let ((pkg-file (expand-file-name (concat pkg "-pkg.el") dir)))
    (unless (file-exists-p pkg-file)
      (error "File not found: %s" pkg-file))
    (aa-defs-form-from-file-contents pkg-file)))

(defun aa-admin--write-pkg-file (pkg-file name metadata)
  ;; FIXME: Use package-generate-description-file!
  (let ((print-level nil)
        (print-quoted t)
	(print-length nil))
    (aa-admin--temp-file pkg-file)
    (write-region
     (concat (format ";; Generated package description from %s.el  -*- no-byte-compile: t -*-\n"
		     name)
	     (prin1-to-string
              (cl-destructuring-bind (version desc requires extras)
                  metadata
                (nconc
                 (list 'define-package
                       name
                       version
                       desc
                       (list 'quote
                             ;; Turn version lists into string form.
                             (mapcar
                              (lambda (elt)
                                (list (car elt)
                                      (package-version-join (cadr elt))))
                              requires)))
                 (aa-admin--alist-to-plist-args extras))))
	     "\n")
     nil
     pkg-file)))

(defun aa-admin--pull (dirname)
  (when-let ((default-directory (aa-admin--dirname dirname))
	     (pkg (file-name-nondirectory dirname))
             (pkg-spec (aa-query-get-spec pkg)))
    ;; Undo any local changes to `<pkg>-pkg.el', in case it's under
    ;; version control.
    (aa-admin--call nil "git" "checkout" "--" (concat pkg "-pkg.el"))
    (with-temp-buffer
      (cond
       ((file-directory-p ".git")
        (aa-admin--call t "git" "pull"))
       ((file-exists-p ".git") ;; A worktree, presumably.
        (let ((remote-ref (aa-admin--ref-master pkg-spec)))
	  (if (aa-admin--ref-p remote-ref)
              (unless (with-temp-buffer
                        (aa-admin--call t "git" "status" "--branch" "--porcelain=2")
                        (string-match "\n# branch.upstream" (buffer-string)))
                (unless (zerop (aa-admin--call nil "git" "branch"
                                                   "--set-upstream-to"
					           remote-ref))
                  (error "aa-admin--pull: %s" (buffer-string))))
	    (error "No remote ref %s" remote-ref)))
	(aa-admin--call t "git" "merge"))
       (t (error "No .git in %s" default-directory))))))

(defun aa-admin--worktree-sync (pkg-spec pkg-dir)
  "Sync worktree of PKG-SPEC in PKG-DIR."
  (aa-admin--call nil "git" "worktree" "remove" "-f" pkg-dir)
  (delete-directory pkg-dir t)
  (with-temp-buffer
    (unless (zerop (aa-admin--call t "git" "worktree" "add" "--detach"
				       pkg-dir (aa-admin--ref-master pkg-spec)))
      (error "aa-admin--worktree-sync: %s" (buffer-string)))))

(defun aa-admin--build-Info (pkg-spec dir)
  (let ((docfile (aa-admin--spec-get pkg-spec :doc)))
    (dolist (f (if (listp docfile) docfile (list docfile)))
      (aa-admin--build-Info-1 f dir))))

(defun aa-admin--build-Info-1 (docfile dir)
  (let* ((aa-admin--sandboxed-ro-binds
          (cons default-directory aa-admin--sandboxed-ro-binds))
         (default-directory (aa-admin--dirname dir))
         (tmpfiles '()))
    (when (and docfile (file-readable-p docfile)
               (string-match "\\.org\\'" docfile))
      (with-temp-buffer
        (aa-admin--call-sandboxed
         t "emacs" "--batch" "-l" "ox-texinfo"
         ;; When building :core packages, don't follow the symlink,
         ;; otherwise Org will want to export into the Emacs tree!
         "--eval" "(setq vc-follow-symlinks nil)"
         docfile
         "--eval" "(message \"ELPATEXI=%s\" (org-texinfo-export-to-texinfo))")
        (message "%s" (buffer-string))
        (goto-char (point-max))
        (when (re-search-backward "ELPATEXI=\\(.*\\)\n?" nil t)
          (setq docfile (concat (file-name-directory docfile)
                                (match-string 1)))
          (push docfile tmpfiles)
          (aa-admin--temp-file docfile))))

    (when (and docfile (file-readable-p docfile)
               (string-match "\\.texi\\(nfo\\)?\\'" docfile))
      (let ((info-file (concat
                        (file-name-sans-extension
                         (file-name-nondirectory docfile))
                        ".info")))
        (aa-admin--temp-file info-file)
        (with-temp-buffer
          (aa-admin--call-sandboxed
           t "makeinfo" "--no-split" docfile "-o" info-file)
          (message "%s" (buffer-string)))
        (setq docfile info-file)))

    (when (and docfile (not (string-match "\\.info\\'" docfile)))
      (error "Not a supported doc format: %s" docfile))

    (when (and docfile (file-readable-p docfile)
               (file-name-directory docfile))
      ;; The built-in support for Info files in package.el only
      ;; works for Info file that are in the top-level directory.
      ;; FIXME: We could just not use it, but then we'd need to do
      ;; something like add a dummy .el file at toplevel with
      ;; an ;;;###autoload cookie which adds the right directory to
      ;; Info-directory-list.  This would have the advantage that
      ;;   emacs -l .../<pkg>-autoloads.el
      ;; would properly setup the Info reader, tho!
      (let ((info-file (file-name-nondirectory docfile)))
        (aa-admin--temp-file info-file)
        (copy-file docfile info-file)
        (setq docfile info-file)))

    (mapc #'delete-file tmpfiles)     ;Avoid intermediate files in the tarball.

    (when (and docfile (file-readable-p docfile))
      (let ((dir-file (expand-file-name "dir")))
        (aa-admin--temp-file dir-file)
        (with-temp-buffer
          (aa-admin--call-sandboxed
           t "install-info" (concat "--dir=" dir-file) docfile)
          (message "%s" (buffer-string)))))))

(defun aa-admin--make (pkg-spec dir)
  (let ((target (aa-admin--spec-get pkg-spec :make))
        (cmd (aa-admin--spec-get pkg-spec :shell-command)))
    (when (or cmd target)
      (with-temp-buffer
        (let ((aa-admin--sandboxed-ro-binds
               (cons default-directory aa-admin--sandboxed-ro-binds))
              (default-directory (aa-admin--dirname dir)))
          (when cmd
            (aa-admin--call-sandboxed t shell-file-name
                                   shell-command-switch
                                   cmd))
          (when target
            (apply #'aa-admin--call-sandboxed t "make"
                   (if (consp target) target (list target))))
          (aa-defs-message "%s" (buffer-string)))))))

(defun aa-admin--branch-p (possible)
  (zerop (aa-admin--call nil "git" "rev-parse" "--verify" possible)))

(defun aa-admin--ref-p (possible)
  (zerop (aa-admin--call nil "git" "show-ref" "--verify" possible)))

(defun aa-admin-cobble-url (pkg-spec)
  (cl-flet ((get (what) (aa-admin--spec-get pkg-spec what)))
    (let ((external (get :external))
          (url (get :url))
          (fetcher (get :fetcher))
          (repo (get :repo)))
      (cond (external external)
            (url url)
            ((and fetcher repo)
	     (format "https://%s.com/%s.git" fetcher repo))
            (t nil)))))

(defun aa-admin--fetch-one-package (pkg-spec)
  (when-let ((url (aa-admin-cobble-url pkg-spec))
             (refspec (aa-admin--refspec pkg-spec)))
    (unwind-protect
        (with-temp-buffer
          (if (zerop (apply #'aa-admin--call t
			    (split-string (format "git fetch --no-tags --depth 1 %s %s"
						  url refspec))))
              (cl-destructuring-bind (from to)
                  (split-string refspec ":")
                (message "%s[%s] -> %s" url from to))
            (error "aa-admin--fetch-one-package: %s" (buffer-string))))
      (apply #'aa-admin--call nil (split-string "git gc --prune=all")))))

(defun aa-admin-batch-fetch ()
  (let ((pkgs command-line-args-left))
    (setq command-line-args-left nil)
    (dolist (pkg pkgs)
      (if-let ((pkg-spec (aa-query-get-spec pkg)))
          (aa-admin--fetch-one-package pkg-spec)
	(unless (locate-library pkg) ; i.e., unless built-in
          (error "aa-admin-batch-fetch: no spec for %s" pkg))))))

(provide 'aa-admin)
;;; aa-admin.el ends here
