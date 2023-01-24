(require 'ob-julia)
(require 'julia-snail)

(cl-defmethod org-babel-julia-prep-session ((_ (eql 'julia-snail)) session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  ;; TODO
  )

(cl-defmethod org-babel-julia-session-live-p
  (session &context (org-babel-julia-backend (eql 'julia-snail)))
  ;; TODO
  )

(cl-defmethod org-babel-julia-evaluate-in-session:sync
  ((_ (eql 'julia-snail)) session body block output)
  "Run BODY in session SESSION synchronously with julia-snail."
  ;; TODO
  )

(cl-defmethod org-babel-julia-evaluate-in-session:async
    (backend session uuid body block output properties)
  "Run BODY in session SESSION asynchronously with julia-snail."
  ;; TODO
  (concat "julia-async:" uuid))

(provide 'ob-julia-snail)
