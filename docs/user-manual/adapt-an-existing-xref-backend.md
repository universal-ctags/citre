# Adapt an Existing Xref Backend

Most of the code reading packages (built-in or 3rd party) have chosen to
integrate with xref, the built-in UI for code reading. Citre offers an adapter
to convert any xref backend to a Citre backend, so more advanced tools like
`citre-peek` and `citre-jump` could be used.

As an example, the built-in `emacs-lisp-mode` contains an xref backend called
`elisp`. Let's convert it to a Citre backend:

``` elisp
(defvar citre-elisp-backend
  (citre-xref-backend-to-citre-backend
   ;; This is the xref backend name
   'elisp
   ;; A function to tell if the backend is usable
   (lambda () (derived-mode-p 'emacs-lisp-mode))))

;; Register the backend, which means to bind it with the symbol `elisp'.
(citre-register-backend 'elisp citre-elisp-backend)
;; Add Elisp to the backend lists.
(setq citre-find-definition-backends '(elisp eglot tags global))
(setq citre-find-reference-backends '(elisp eglot global))
```

Now you can use all Citre tools for finding definitions/references in elisp
files! The `citre-query-*` commands aren't supported due to limitations of
xref.
