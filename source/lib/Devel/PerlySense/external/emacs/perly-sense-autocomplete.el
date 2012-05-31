

(defun ps/ac-candidates ()
  (let* (
         (buffer-text (buffer-string))
         (buffer-point (point))
         (sub-name
          ;; (with-temp-buffer
          ;;            (insert buffer-text)
          ;;            (goto-char buffer-point)
          ;;            (sub-name (if (search-backward "sub +\\([_a-z0-9]+\\)" nil t)
          ;;                          (match-string 1)
          ;;                        "N/A")
          ;;                      )
          ;;            )
          (format "buffer(%s) point(%s) prefix(%s)" ac-buffer ac-point ac-prefix)
          )
         )
    (mapcar (lambda (candidate)
              (format "%s-%s" candidate sub-name))
            '("Foo" "Bar" "Baz" "Boo" "Boolean" "Bookshelf")
            )
    )
  )

(defvar ps/ac-source
  '(
    (candidates . ps/ac-candidates)
    (summary . (lambda () "Help text"))
    )
  )



(setq ac-sources '(ps/ac-source))


