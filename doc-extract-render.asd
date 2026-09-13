(defsystem "doc-extract-render"
  :version "0.1.0"
  :description "Markdown and HTML render/parse for extracted-document (C3e)"
  :author "egao1980"
  :license "MIT"
  :depends-on ("doc-extract-protocol" "html-protocol")
  :properties (:cl-repo (:ci (:with ("html-backend-plump"))))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "render")
               (:file "html")
               (:file "parse-html")
               (:file "parse-markdown"))
  :in-order-to ((test-op (test-op "doc-extract-render/tests"))))

(defsystem "doc-extract-render/tests"
  :depends-on ("doc-extract-render" "html-backend-plump" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "helpers")
               (:file "render-test")
               (:file "render-property-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
