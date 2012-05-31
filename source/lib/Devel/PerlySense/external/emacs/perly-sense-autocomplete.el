

(defun ps/ac-candidates ()
  (save-excursion
    (let* (
           (sub-name
            (if (search-backward-regexp "sub \\([a-zA-Z_]+\\)" nil t)
                (match-string-no-properties 1)
              "N/A")
            )
           ;; (format "buffer(%s) point(%s) prefix(%s)" ac-buffer ac-point ac-prefix)
           )
      (mapcar (lambda (candidate)
                (format "%s-%s" candidate sub-name))
              '("Foo" "Bar" "Baz" "Boo" "Boolean" "Bookshelf")
              )
      )
    )
  )

(defun ps/candidate-documentation (symbol-name)
  (let ((symbol-name (substring-no-properties symbol-name)))
    "textClassInheritance(oClass)

Return string representing the class hierarchy of $oClass."))

(defvar ps/ac-source-method-calls
  '(
    (prefix . "->\\(.*\\)")
    (candidates . ps/ac-candidates)
    (summary . (lambda () "Help text"))
    (document . ps/candidate-documentation)
    )
  )


; prefix: use 



(setq ac-sources '(ps/ac-source-method-calls))


