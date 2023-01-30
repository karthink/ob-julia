(require 'ob-julia)
(require 'ess)
(require 'ess-julia)

(cl-defmethod org-babel-julia--get-live-session
  (session &context (org-babel-julia-backend (eql 'ess-julia)))
  (org-babel-comint-buffer-livep session))

(defun org-julia-ess-async-process-filter (process output)
  "A function that is called when new output is available on the
  Julia buffer, which waits until the async execution is
  completed.  Replace julia-async: tags with async results.
  This version is specific to ESS."
  (org-julia-async-process-filter process output t))

(cl-defmethod org-babel-julia-prep-session ((_ (eql 'ess-julia)) session params)
  "Prepare an ess-julia SESSION according to the header arguments specified in PARAMS."
  (let ((dir (or (alist-get :dir params)
		 (inferior-ess--maybe-prompt-startup-directory session "julia")))
        ;; We manyally ask for starting directory, don't ask twice
        (ess-ask-for-ess-directory nil))
    (save-window-excursion
      (let* ((start-script-arg
              (concat
               (mapconcat #'identity org-babel-julia-command-arguments " ")
               (format " --load=%s" ob-julia-startup-script)))
             (inferior-julia-args (if inferior-julia-args
                                      (concat inferior-julia-args " " start-script-arg)
                                    start-script-arg)))
        (switch-to-buffer (run-ess-julia)))
      (setq session
            (or session
                (rename-buffer
                 (if (bufferp session)
                     (buffer-name session)
                   (if (stringp session)
                       session
                     (buffer-name))))))
      ;; Register the async callback. Important to do this before
      ;; running any command
      (set-process-filter
       (get-buffer-process
        (org-babel-comint-buffer-livep session))
       'org-julia-ess-async-process-filter)
      (get-buffer session))))

(cl-defmethod org-babel-julia--get-live-session
  (session &context (org-babel-julia-backend (eql 'ess-julia)))
  (org-babel-comint-buffer-livep session))

(cl-defmethod org-babel-julia-evaluate-in-session:sync
  ((_ (eql 'ess-julia)) session body block output-file _)
  "Run BODY in session SESSION synchronously with ess-julia."
  (org-babel-comint-eval-invisibly-and-wait-for-file
   session output-file body 0.1)
  (with-current-buffer session
    (comint-add-to-input-history block))
  output-file)

(cl-defmethod org-babel-julia-evaluate-in-session:async
    (backend session uuid body block output properties)
  "Run BODY in session SESSION asynchronously with ess-julia."
  (process-send-string session (concat body "\n"))
  (with-current-buffer session
    (comint-add-to-input-history block))
  (org-babel-julia--async-add uuid properties)
  (concat "julia-async:" uuid))

(provide 'ob-julia-ess)
