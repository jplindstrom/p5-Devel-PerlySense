
(defun ps/ac-get-current-class-name ()
  (save-excursion
    (if (search-backward-regexp "package +[^;]*?\\([:a-zA-Z0-0]+\\)" nil t)
        (match-string 1)
      nil
      )
    )
  )

(defun ps/call-repository-server (path args)
  (let (
        (response-buffer
         (url-retrieve-synchronously (format "http://localhost:3496%s?%s" path args)))
        )
    (if response-buffer
        (with-current-buffer response-buffer
          (goto-char (point-min))
          (if (search-forward-regexp "\n\n" nil t)
              (delete-region (point-min) (point))
            )
          (buffer-string))
      (message "Could not call PerlySense Repository Server")
      nil
      )
    )
  )

(defun ps/call-repository-server-parse-sexp (path args)
  (let ((response-text (ps/call-repository-server path args)))
    (if response-text
        (ps/parse-sexp response-text)
      '()
      )))

(defun ps/call-repository-server-parse-sexp-get-key (path args key)
  (let ((response-alist (ps/call-repository-server-parse-sexp path args)))
    (alist-value response-alist key)
    ))

(defun ps/completions-list-for-self ()
  (let ((current-class-name (ps/ac-get-current-class-name)))
    (if current-class-name
        (ps/call-repository-server-parse-sexp-get-key
         "/method/complete"
         (format "class_name=%s" current-class-name)
         "completions")
      (message "Could not find package declaration")
      '()
      )
    )
  )

(defun ps/ac-candidates ()
  (interactive) ;; JPL
  (let ((completions-list
         (or
          (progn
            (when (looking-back "$self->\\(.*\\)")
              (ps/completions-list-for-self)
              )
            )
          '()
          ))
        )
    (mapcar (lambda (completion-alist)
              (let ((sub-name (alist-value completion-alist "method_name"))
                    (package-name (alist-value completion-alist "api_package")))
                (propertize sub-name 'summary package-name)
                )
              )
            completions-list)
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

;; when calling the GET
  ;; (let ((buf (get-buffer-create "*clang-output*"))
  ;;       res)
  ;;   (with-current-buffer buf (erase-buffer))


;; When parsing strings, attach the doc string to the candidate
                ;; (setq match (propertize match
                ;;                         'ac-clang-help
                ;;                         (concat
                ;;                          (get-text-property 0 'ac-clang-help (car lines))
                ;;                          "\n"
                ;;                          detailed_info)))


;; (defun ac-clang-document (item)
;;   (if (stringp item)
;;       (let (s)
;;         (setq s (get-text-property 0 'ac-clang-help item))
;;         (ac-clang-clean-document s)))
;;   )

(defvar ps/ac-source-method-calls
  '(
    ;; init to ensure server is started and indexer is started
    (prefix . "->\\(.*\\)")
    (candidates . ps/ac-candidates)
    (document . ps/candidate-documentation)
    )
  )

(defun ps/smart-complete-at-point ()
  (interactive)
  (auto-complete '(ps/ac-source-method-calls)) ;; For now
  )
(global-set-key (format "%s\C-c" ps/key-prefix) 'ps/smart-complete-at-point)


; prefix: use



; (setq ac-sources '(ps/ac-source-method-calls))

(provide 'perly-sense-autocomplete)

