
(defun ps/ac-candidates ()
  (save-excursion
    (goto-char (point-min))
    (let ( (sub-names (list ) ) )
      (while (search-forward-regexp "sub +\\([a-zA-Z_]+\\)" nil t)
        (push (match-string-no-properties 1) sub-names)
        )
      sub-names
      )
    )
  )

(defun ps/candidate-documentation (symbol-name)
  (save-excursion
    (goto-char (point-min))
    (when (search-forward-regexp (format "sub +%s " symbol-name) nil t)
      (when (search-backward-regexp "=cut\\|}" nil t)
        (when (looking-at "=cut")
          (previous-line)
          (let* ((pod-end-pos (point)))
            (when (search-backward-regexp "=head" nil t)
              (forward-word) (forward-char)
              (let* ((pod-docs
                      (buffer-substring-no-properties (point) pod-end-pos)))
                pod-docs
                )
              )
            )
          )
        )
      )
    )
  )

(defvar ps/ac-source-method-calls
  '(
    (prefix . "->\\(.*\\)")
    (candidates . ps/ac-candidates)
    (summary . (lambda () "Help text"))
    (document . ps/candidate-documentation)
    )
  )


; prefix: use



; (setq ac-sources '(ps/ac-source-method-calls))

(provide 'perly-sense-autocomplete)

