(require 'ob-julia)
(require 'julia-snail)

(cl-defmethod org-babel-julia-prepare-format-call
    ((_ (eql 'julia-snail)) src-file out-file params &optional uuid)
  "Format a call to OrgBabelEval

OrgBabelEval is the entry point of the Julia code defined in
the startup script."
  (format
   ;; "Main.JuilaSnail.Extensions.ObJulia.OrgBabelEval(%S,%S,%S,%s;print_output=false);"
   "ObJulia.OrgBabelEval(%S,%S,%S,%s;print_output=false,catch_errors=false);"
   src-file out-file (org-babel-julia-params->named-tuple params)
   (or (when uuid (format "%S" uuid)) "nothing")))

(cl-defmethod org-babel-julia-prep-session ((_ (eql 'julia-snail)) session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (let ((dir (or (alist-get :dir params)
		 default-directory))
        (repl-buffer
         (org-babel-julia-get-session-name params)))
    (save-window-excursion
      (setq-local julia-snail-repl-buffer repl-buffer)
      (julia-snail)
      (message "Loading ObJulia...")
      (julia-snail--send-to-server
        ;; '("JuliaSnail" "Extensions")
        '("Main")
        (format "include(\"%s\")" ob-julia-startup-script)
        :repl-buf repl-buffer
        :async nil)
      (message "Loading ObJulia... done")
      julia-snail-repl-buffer)))

(cl-defmethod org-babel-julia--get-live-session
  (session &context (org-babel-julia-backend (eql 'julia-snail)))
  (and-let*
      ((repl-buffer (get-buffer julia-snail-repl-buffer)) 
       ((buffer-live-p repl-buffer))
       ((buffer-local-value 'julia-snail-repl-mode repl-buffer)))
    repl-buffer))

(cl-defmethod org-babel-julia-evaluate-in-session:sync
  ((_ (eql 'julia-snail)) session OrgBabelEval-call _ output-file params)
  "Run ORGBABELEVAL-CALL in session SESSION synchronously with julia-snail."
  (when-let ((mime-type
              (julia-snail--send-to-server
                '("Main")
                OrgBabelEval-call
                :async nil
                :display-error-buffer-on-failure? t)))
    ;; Rename the output file heuristically by mime-type
    (setq output-file (org-babel-julia--maybe-rename-output output-file mime-type params))
    (if (file-exists-p output-file)
        output-file
      (error "No output produced."))))

(cl-defmethod org-babel-julia-evaluate-in-session:async
  ((_ (eql 'julia-snail)) session uuid OrgBabelEval-call _ output properties)
  "Run ORGBABELEVAL-CALL in session SESSION asynchronously with julia-snail."
  (let ((reqid 
         (julia-snail--send-to-server
           '("Main") ;TODO: Use `julia-snail--module-at-point'
           OrgBabelEval-call
           :async t
           :display-error-buffer-on-failure? t
           :callback-success #'org-babel-julia-snail-success-callback
           ;; Currently never called:
           :callback-failure #'org-babel-julia-snail-failure-callback))))
  (org-babel-julia--async-add uuid properties)
  (concat "julia-async:" uuid))

(defun org-babel-julia-snail-success-callback (request-info result-data)
  "A function that is called when julia-snail response is available."
  (if (not result-data)
      (message "Code block produced no output.")
    (pcase-let ((`(,uuid-string . ,mime-type) (read result-data)))
      (if (string-match ".*ob_julia_async_\\([0-9a-z\\-]+\\).*" uuid-string)
          (let* ((uuid (match-string-no-properties 1 uuid-string))
                 (org-buffer (julia-snail--request-tracker-originating-buf request-info))
                 (display-errors (julia-snail--request-tracker-display-error-buffer-on-failure?
                                  request-info))
                 (properties (org-babel-julia--async-get-remove uuid))
                 (vals (cdr properties))
                 (params (elt vals 0))
                 (output-file (elt vals 1))
                 ;; (org-buffer (elt vals 2))
                 (src-file (elt vals 3)))
            (unwind-protect
                (progn
                  ;; Rename the output file heuristically by mime-type
                  (setq output-file
                        (org-babel-julia--maybe-rename-output output-file mime-type params))
                  (org-babel-julia--place-result output-file org-buffer uuid params))
              (when (and src-file (file-exists-p src-file))
                (delete-file src-file))))))))

(defun org-babel-julia--maybe-rename-output (output-file mime-type params)
  "Possibly rename OUTPUT-FILE with a more suitable extension.

MIME-TYPE is chosen by ObJulia. PARAMS is the list of block
parameters.
 
ObJulia can pick a mime-type better suited to the type of result
generated - for instance, png when writing a GR plot object.
Unless an output file is explicitly specified with the header arg
`:file', we rename the output file to a more suitable extension."
  (if-let* (((not (alist-get :file params)))
            (required-ext (alist-get mime-type org-babel-julia-mimes->exts
                                     nil nil #'equal))
            ((not (string= (file-name-extension output-file) required-ext)))
            (new-output-file (concat (file-name-sans-extension output-file)
                                     "." required-ext)))
      (progn (rename-file output-file new-output-file 'force)
             new-output-file)
    output-file))

;; NOTE: because we catch errors in ObJulia this is never actually called.
;;
;; TODO: Provide an option to not catch errors when using julia-snail?
;; julia-snail's error reporting is pretty slick.
;; NOTE: In that event, we can't access the UUID! Need to think more about this.
(defun org-babel-julia-snail-failure-callback (request-info)
  (pcase-let ((`(,uuid-string . ,mime-type) (read result-data)))
    (if (string-match ".*ob_julia_async_\\([0-9a-z\\-]+\\).*" uuid-string)
        (let* ((uuid (match-string-no-properties 1 uuid-string))
               (properties (org-babel-julia--async-get-remove uuid))
               (src-file (elt vals 3)))
          (when (and src-file (file-exists-p src-file))
            (delete-file src-file))))))

(provide 'ob-julia-snail)
