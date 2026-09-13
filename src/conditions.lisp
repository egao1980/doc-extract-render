(in-package #:doc-extract-render)

(define-condition render-error (doc-extract-protocol:doc-extract-error)
  ()
  (:report (lambda (c s)
             (format s "doc-extract-render error~@[ (~a)~]~@[: ~a~]"
                     (doc-extract-protocol:doc-extract-error-format c)
                     (doc-extract-protocol:doc-extract-error-message c)))))

(define-condition unknown-render-format (render-error
                                         doc-extract-protocol:doc-extract-unsupported)
  ()
  (:report (lambda (c s)
             (format s "unknown render/parse format ~s~@[: ~a~]"
                     (unknown-render-format-format c)
                     (doc-extract-protocol:doc-extract-error-message c)))))

(defun unknown-render-format-format (condition)
  (doc-extract-protocol:doc-extract-error-format condition))

(defun render-error-message (condition)
  (doc-extract-protocol:doc-extract-error-message condition))
