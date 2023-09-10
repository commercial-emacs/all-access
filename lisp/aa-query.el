;;; aa-query.el --- Emacs All Access  -*- lexical-binding:t -*-

;; Copyright (C) 2011-2023  The Authors

;; Author: The Authors
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

;;; Code:

(require 'aa-defs)
(require 'url-auth)
(require 'web-server)

(eval-when-compile
  (when (< emacs-major-version 29)
    (defun seq-keep (function sequence)
      "Apply FUNCTION to SEQUENCE and return all non-nil results."
      (delq nil (seq-map function sequence)))))

(defvar url-http-end-of-headers)

(defvar aa-query-prospective-spec nil
  "Global variable relies on one-at-a-time installs.")

(defconst aa-query-server "shmelpa.commandlinesystems.com")
(defconst aa-query-domain "allaccess.auth.commandlinesystems.com")
(defconst aa-query-client "7li6vomknkgvomeqhj9674nlcb")
(defconst aa-query-ws-port 17973)

(defconst aa-query-redirect-url
  (url-recreate-url
   (url-parse-make-urlobj "http" nil nil "127.0.0.1"
			  aa-query-ws-port "/" nil nil t)))

(defconst aa-query-spec-url
  (url-recreate-url
   (url-parse-make-urlobj "https" nil nil aa-query-server nil
			  "/spec" nil nil t)))

(defconst aa-query-subscription-url
  (url-recreate-url
   (url-parse-make-urlobj "https" nil nil aa-query-server nil
			  "/subscription" nil nil t)))

(defconst aa-query-success-url
  (url-recreate-url
   (url-parse-make-urlobj "https" nil nil aa-query-server nil
			  "/success.html" nil nil t)))

(defconst aa-query-cancel-url
  (url-recreate-url
   (url-parse-make-urlobj "https" nil nil aa-query-server nil
			  "/cancel.html" nil nil t)))

(defconst aa-query-checkout-url
  (url-recreate-url
   (url-parse-make-urlobj "https" nil nil aa-query-server nil
			  "/checkout" nil nil t)))

(defconst aa-query-hosted-ui
  (let ((redirect (url-recreate-url
		   (url-parse-make-urlobj "https" nil nil aa-query-server
					  nil "/code/" nil nil t))))
    (url-recreate-url
     (let ((query-string (url-build-query-string
			  `(("response_type" "code")
			    ("client_id" ,aa-query-client)
			    ("redirect_uri" ,redirect)))))
       (url-parse-make-urlobj "https" nil nil aa-query-domain nil
			      (concat "/login?" query-string) nil nil t)))))

(defsubst aa-query-id-token-alist ()
  (when-let ((token (aa-query-token "id_token")))
    (json-parse-string
     (base64-decode-string
      (cl-second (split-string (symbol-name token) "\\."))
      'base64url)
     :array-type 'list
     :object-type 'alist)))

(defun aa-query-token (which)
  (aa-defs-let-token-dir token-dir 'cognito
    (let ((path (expand-file-name which token-dir)))
      (aa-defs-form-from-file-contents path))))

(defmacro aa-query-squirrel (token)
  "Persist value of symbol .TOKEN to a file named TOKEN."
  `(aa-defs-let-token-dir token-dir 'cognito
     (with-temp-file (concat (file-name-as-directory token-dir) (symbol-name ',token))
       (insert ,(intern (concat "." (symbol-name token))) "\n"))))

(defmacro aa-squirrel-tokens (alist)
  "Return list of tokens changed.
Side effect squirrel changed tokens to disk."
  `(let (result)
     (let-alist ,alist
       (when .access_token
	 (aa-query-squirrel access_token)
	 (push 'access_token result))
       (when .refresh_token
	 (aa-query-squirrel refresh_token)
	 (push 'refresh_token result))
       (when .id_token
	 (aa-query-squirrel id_token)
	 (push 'id_token result))
       (nreverse result))))

(defun aa-query-hosted-ui-flow ()
  "Return t if hosted-ui => token.php => ws."
  (unwind-protect
      (catch 'success
	(ws-start
	 '(((:POST . ".*") .
	    (lambda (request)
	      (with-slots (process headers) request
		(aa-squirrel-tokens (mapcar (lambda (pair)
						  (cons (if (stringp (car pair))
							    (intern (car pair))
							  (car pair))
							(cdr pair)))
						headers))
		(ws-response-header process 200 '("Content-type" . "text/plain"))
		(process-send-string process "Thanks. Identity verified.\n")
		(process-send-eof process)
		(throw 'success t)))))
	 aa-query-ws-port)
	(browse-url aa-query-hosted-ui)
	(message "Blocking on browser login... C-g to abort")
	(cl-loop repeat 210 ; 3.5 minutes to complete login
		 do (accept-process-output nil 1)
		 finally return nil))
    (ws-stop-all)))

(defun aa-query--session ()
  "Return alist of checkout session from shmelpa."
  (let* (url-registered-auth-schemes
	 (id-token (aa-query-token "id_token"))
	 (url-request-method "POST")
	 (url-request-extra-headers `(("Content-Type" .
				       "application/x-www-form-urlencoded")
				      ;; Dummy authorization so that
				      ;; `url-http-handle-authentication'
				      ;; doesn't keep trying.
				      ("Authorization" .
				       "Basic")))
	 (url-request-data (url-build-query-string
			    `(("id_token" ,id-token)))))
    (let ((buffer (url-retrieve-synchronously aa-query-checkout-url)))
      (unwind-protect
	  (with-current-buffer buffer
	    (goto-char url-http-end-of-headers)
	    (let ((status (buffer-local-value 'url-http-response-status buffer))
		  (decoded (condition-case nil
			       (json-parse-buffer :false-object nil
						  :null-object nil
						  :array-type 'list
						  :object-type 'alist)
			     (json-end-of-file (error "aa-query--session: server error")))))
	      (prog1 decoded
		(unless (eq 200 status)
		  (error "aa-query--session: http status %s" status)))))
	(when (buffer-live-p buffer)
	  (kill-buffer buffer))))))

(defun aa-query-checkout-flow (url)
  "Return t if checkout.stripe.com => {success,cancel}.html => ws."
  (unwind-protect
      (catch 'success
	(ws-start
	 '(((:POST . ".*") .
	    (lambda (request)
	      (with-slots (process headers) request
		(ws-response-header
		 process 303
		 `("Location" . ,(if (assoc-default "success" headers)
				     aa-query-success-url
				   aa-query-cancel-url)))
		(process-send-eof process)
		(throw 'success (assoc-default "success" headers))))))
	 aa-query-ws-port)
	(browse-url url)
	(message "Blocking on checkout... C-g to abort")
	(cl-loop repeat 210 ; 3.5 minutes to complete checkout
		 do (accept-process-output nil 1)
		 finally return nil))
    (ws-stop-all)))

(defun aa-query-update-payment-flow (url)
  "Return t if billing.stripe.com => shmelpa/flow_return => ws."
  (unwind-protect
      (catch 'success
	(ws-start
	 '(((:POST . ".*") .
	    (lambda (request)
	      (with-slots (process headers) request
		(ws-response-header process 200 '("Content-type" . "text/plain"))
		(process-send-string process "Thanks. Returned to emacs.\n")
		(process-send-eof process)
		(throw 'success (assoc-default "payment_updated" headers))))))
	 aa-query-ws-port)
	(browse-url url)
	(message "Blocking on payment method update...C-g to abort")
	(cl-loop repeat 210 ; 3.5 minutes to complete checkout
		 do (accept-process-output nil 1)
		 finally return nil))
    (ws-stop-all)))

(defmacro aa-query--align (&rest args)
  (declare (indent defun))
  (let* ((cars (let ((idx 0))
		 (seq-keep (lambda (x) (prog1 (when (zerop (% idx 2)) x)
					 (cl-incf idx)))
			   args)))
	 (cdrs (let ((idx 0))
		 (seq-keep (lambda (x) (prog1 (unless (zerop (% idx 2)) x)
					 (cl-incf idx)))
			   args)))
	 (widest (cl-reduce #'max (mapcar #'length cars))))
    `(mapc
      (lambda (pair)
	(cl-flet ((insert-lhs ()
		    (insert (make-string (1+ (- ,widest (string-width (car pair)))) ?\s)
			    (propertize (concat (car pair) ": ")
					'font-lock-face 'font-lock-function-name-face)))
		  (decorate-rhs (rhs)
		    (if (equal "Notification" (car pair))
			(insert (propertize rhs 'font-lock-face 'font-lock-string-face))
		      (insert rhs))))
	  (if (and (stringp (cdr pair)) (not (zerop (length (cdr pair)))))
	      (progn (insert-lhs)
		     (decorate-rhs (cdr pair))
		     (insert "\n"))
	    (when (functionp (cdr pair))
	      (insert-lhs)
	      (let ((pt (point)))
		(funcall (cdr pair))
		(if (equal pt (point))
		    (progn
		      (beginning-of-line)
		      (kill-line))
		  (insert "\n")))))))
      (cl-map 'list #'cons (quote ,cars) (backquote ,cdrs)))))

(defun aa-query-checkout ()
  (let-alist (aa-query--session)
    (if .error
	(user-error (format "aa-query-checkout: %s" .error))
      (aa-query-checkout-flow .url))))

(defun aa-query-update-payment ()
  (let-alist (aa-query--subscription :action "update")
    (aa-query-update-payment-flow .url)))

(defun aa-query-cancel-subscription ()
  (let-alist (aa-query--subscription :action "cancel")
    (equal .status "canceled")))

;;;###autoload
(defalias 'aa-query #'aa-query-show-subscription)

;;;###autoload
(defun aa-query-show-subscription (&optional notification)
  (interactive (list nil))
  (if-let ((format-time (lambda (secs)
			  (format-time-string "%Y-%m-%d %a %H:%M:%S %Z"
					      (seconds-to-time secs))))
	   (id-token-alist (aa-query-id-token-alist))
	   (subscription-alist (aa-query--subscription)))
    (with-help-window "*All Access*"
      (let-alist subscription-alist
	(aa-query--align
	  "Notification" ,notification
	  "User" ,(alist-get 'email id-token-alist)
	  "Status" ,(capitalize .status)
	  "Canceled At" ,(when .canceled_at
			   (funcall format-time .canceled_at))
	  "Period Begin" ,(when (and (or (not .trial_end)
					 (< .trial_end .current_period_end))
				     (not (member .status '("canceled" "paused"))))
			    (funcall format-time .current_period_start))
	  "Period End" ,(when (and (or (not .trial_end)
					 (< .trial_end .current_period_end))
				     (not (member .status '("canceled" "paused"))))
			  (funcall format-time .current_period_end))
	  "Trial Begin" ,(when (and .trial_end
				    (>= .trial_end .current_period_end))
			   (funcall format-time .trial_start))
	  "Trial End" ,(when (and .trial_end
				  (>= .trial_end .current_period_end))
			 (funcall format-time .trial_end))
	  "Billing Cycle Begin" ,(unless (member .status '("canceled" "paused"))
				   (funcall format-time .billing_cycle_anchor))
	  "Billing Details" ,(unless (member .status '("canceled" "paused"))
			       (concat (upcase (alist-get 'currency .plan)) " "
				       (number-to-string (* 0.01 (alist-get 'amount .plan)))
				       " per "
				       (alist-get 'interval .plan)))
	  "Payment Method" ,(unless (member .status '("canceled" "paused"))
			      (or (when-let ((card (alist-get 'card .default_payment_method)))
				    (concat (capitalize (alist-get 'brand card)) " ending "
					    (alist-get 'last4 card)))
				  (when-let ((link (alist-get 'link .default_payment_method)))
				    (concat "Link(TM) to "(alist-get 'email link)))
				  .default_source
				  (if (or (equal .status "paused")
					  (equal .status "canceled"))
				      "Not Added"
				    "Not Yet Added")))
	  "Actions" ,(lambda ()
		       (when (equal .status "canceled")
			 (package-make-button
			  "Uncancel"
			  'action (lambda (_button)
				    (aa-query-show-subscription
				     (format "Uncancel %s"
					     (if (aa-query-checkout)
						 "succeeded"
					       "failed")))))
			 (insert " "))
		       (when (and (not (equal .status "canceled"))
				  t)
			 (package-make-button
			  "Cancel"
			  'action (lambda (_button)
				    (aa-query-show-subscription
				     (format "Cancel %s"
					     (if (aa-query-cancel-subscription)
						 "succeeded"
					       "failed")))))
			 (insert " "))
		       (when (and (not (equal .status "paused"))
				  (not (equal .status "canceled"))
				  (not .default_payment_method)
				  (not .default_source))
			 (package-make-button
			  "Add Card"
			  'action (lambda (_button)
				    (aa-query-show-subscription
				     (format "Payment update %s"
					     (if (aa-query-update-payment)
						 "succeeded"
					       "failed")))))
			 (insert " "))))))
    (message "aa-query-show-subscription: Subscribe first.")))

(cl-defun aa-query--subscription (&key (action "get"))
  "Return alist of subscription data from shmelpa."
  (let* (url-registered-auth-schemes
	 (id-token (aa-query-token "id_token"))
	 (url-request-method "POST")
	 (url-request-extra-headers `(("Content-Type" .
				       "application/x-www-form-urlencoded")
				      ;; Dummy authorization so that
				      ;; `url-http-handle-authentication'
				      ;; doesn't keep trying.
				      ("Authorization" .
				       "Basic")))
	 (url-request-data (url-build-query-string
			    `(("id_token" ,id-token)))))
    (let ((buffer (url-retrieve-synchronously
		   (concat aa-query-subscription-url "?action=" action))))
      (unwind-protect
	  (with-current-buffer buffer
	    (goto-char url-http-end-of-headers)
	    (let ((status (buffer-local-value 'url-http-response-status buffer))
		  (decoded (condition-case nil
			       (json-parse-buffer :false-object nil
						  :null-object nil
						  :array-type 'list
						  :object-type 'alist)
			     (json-end-of-file (error "aa-query--subscription: server error")))))
	      (prog1 decoded
		(unless (eq 200 status)
		  (error "aa-query--subscription: http status %s" status)))))
	(when (buffer-live-p buffer)
	  (kill-buffer buffer))))))

(defvar aa-query--package-names nil "For `completing-read'")
(defun aa-query-get-package-names ()
  (unless aa-query--package-names
    (setq aa-query--package-names
	  (cl-some (lambda (obj) (alist-get 'package_names obj))
		   (aa-query--get-dance :package-names t))))
  aa-query--package-names)

(defmacro aa-query--to-spec (dance)
  "Convert ((SPEC . ALIST)...) to (NAME . PLIST)."
  (declare (indent defun))
  `(when-let ((alist (cl-some (lambda (obj)
				(alist-get 'spec obj))
			      ,dance))
	      (name-pair (assq 'name alist))
	      (nameless-alist (cl-remove-if
			       (apply-partially #'equal name-pair) alist))
	      (plist (json-parse-string (json-encode nameless-alist)
					:false-object nil
					:null-object nil
					:array-type 'list
					:object-type 'plist)))
     (cons (cdr name-pair) plist)))

(cl-defun aa-query-get-spec (name &aux (name (if (symbolp name)
						 (symbol-name name)
					       name)))
  "Like `aa-query-get-spec-by-url' but key off NAME.
Primary entry to specs outside aa-disc."
  (let ((spec (aa-query--to-spec (aa-query--get-dance :name name))))
    (cond (spec spec)
	  ((package-built-in-p (intern name)) nil)
	  (t aa-query-prospective-spec))))

(defun aa-query-get-name-by-url (url)
  "Ask shmelpa for repo name given URL."
  (cl-some (lambda (obj)
	     (alist-get 'name obj))
	   (aa-query--get-dance :only-name url)))

(defun aa-query-get-spec-by-url (url)
  "Like `aa-query-get-spec' but key off URL.
Primarily called from aa-disc."
  (aa-query--to-spec (aa-query--get-dance :url url)))

(defun aa-query--get-dance (&rest args)
  (catch 'done
    (dotimes (_i 2)
      (let ((result
	     (cl-destructuring-bind (code . buffer)
		 (apply #'aa-query--do-get args)
	       (unwind-protect
		   (progn
		     (cl-case (/ code 100)
		       (4 (if (y-or-n-p "Open browser to permission All Access? ")
			      (when (aa-query-hosted-ui-flow)
				(cl-destructuring-bind (code* . buffer*)
				    (apply #'aa-query--do-get args)
				  (setq code code*
					buffer (prog1 buffer* (when (buffer-live-p buffer)
								(kill-buffer buffer))))))
			    (user-error "That's too bad")))
		       (5 (error "Server error.  Contact commandlinesystems.com.")))
		     (if (= 2 (/ code 100))
			 (with-current-buffer buffer
			   (goto-char url-http-end-of-headers)
			   (cl-loop for response = (condition-case nil
						       (json-parse-buffer :false-object nil
									  :null-object nil
									  :array-type 'list
									  :object-type 'alist)
						     (json-end-of-file nil))
				    while response
				    for prompt = (if (alist-get 'lapsed response)
						     "Re-up subscription? "
						   (when (alist-get 'trial_eligible response)
						     "Initiate free trial? "))
				    if (aa-squirrel-tokens response)
				    return 'again
				    else if prompt
				    return (prog1 'again
					     (when (or (not (y-or-n-p prompt))
						       (not (aa-query-checkout)))
					       (user-error "That's too bad")))
				    else collect response
				    end))
		       (error "aa-query--get-dance %s"
			      (buffer-substring-no-properties
			       (point-min)
			       (if (bound-and-true-p url-http-end-of-headers)
				   url-http-end-of-headers
				 (point-max))))))
		 (when (buffer-live-p buffer)
		   (kill-buffer buffer))))))
	(unless (eq result 'again)
	  (throw 'done result))))))

(cl-defun aa-query--do-get (&key only-name name url package-names &allow-other-keys)
  "Return cons of response code and buffer response.
Caller must clean it up."
  (let* (url-registered-auth-schemes
	 (access-token (aa-query-token "access_token"))
	 (id-token (aa-query-token "id_token"))
	 (url-request-method "POST")
	 (url-request-extra-headers `(("Content-Type" .
				       "application/x-www-form-urlencoded")
				      ;; Dummy authorization so that
				      ;; `url-http-handle-authentication'
				      ;; doesn't keep trying.
				      ("Authorization" .
				       "Basic")))
	 (url-request-data (url-build-query-string
			    `(,@(when name
				  (list `("name" ,name)))
			      ,@(when url
				  (list `("url" ,url)))
			      ,@(when package-names
				  (list '("package_names")))
			      ,@(when only-name
				  (list `("only_name" ,only-name)))
			      ,@(when access-token
				  (list `("access_token" ,access-token)))
			      ,@(when id-token
				  (list `("id_token" ,id-token)))
			      ("redirect_uri" ,aa-query-redirect-url)))))
    (let ((buffer (url-retrieve-synchronously aa-query-spec-url)))
      (cons (buffer-local-value 'url-http-response-status buffer) buffer))))

(provide 'aa-query)

;; (equal (aa-query-get-spec "org") (aa-query-get-spec-by-url "https://git.savannah.gnu.org/git/emacs/org-mode.git"))

;;; aa-query.el ends here
