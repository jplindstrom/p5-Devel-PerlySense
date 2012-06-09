
(defun ps/ac-get-current-class-name ()
  (save-excursion
    (if (search-backward-regexp "package +[^;]*?\\([:a-zA-Z0-0]+\\)" nil t)
        (match-string 1)
      nil
      )
    )
  )

(defun ps/call-repository-server (url args)


  )

(defun ps/ac-candidates ()
  (interactive) ;; JPL
  (let* (
        (sub-names (list ) )
        (current-class-name (ps/ac-get-current-class-name))
        (response-buffer
         (and current-class-name
              (url-retrieve-synchronously
               (format "http://localhost:3496/method/complete?class_name=%s" current-class-name)
               )
              )
         )
        )
    (if response-buffer
        (let* (
               (response-text
                (with-current-buffer response-buffer
                  (goto-char (point-min))
                  (if (search-forward-regexp "\n\n" nil t)
                      (delete-region (point-min) (point))
                    )
                  (buffer-string)))
               (result-alist (ps/parse-sexp response-text))
               ;; (dummy (prin1 result-alist))
               (completions-list (alist-value result-alist "completions"))
               )
          (mapcar
           (lambda (completion-alist)
             (let (
                   (sub-name (alist-value completion-alist "method_name"))
                   (package-name (alist-value completion-alist "package"))
                   )
               (push (propertize sub-name 'summary package-name) sub-names)
               )
             )
           completions-list)
          )
      (message "Could not call PerlySense Repository Server")
      )
    sub-names
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

