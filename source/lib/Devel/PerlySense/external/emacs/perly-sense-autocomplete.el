

(defun ps/ac-candidates ()
  (let (
        (sub-name (save-excursion
                    (if (search-backward "sub +\\([_a-z0-9]+\\)" nil t)
                        (match-string 1)
                      "N/A")
                    )
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
    )
  )



(setq ac-sources '(ps/ac-source))


