
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
        ;; (dummy (message "Calling with (%s) (%s)" args path))
        (response-buffer
         (flet ((message (arg &rest) nil)) ;; Supress (message) in the web request
           (url-retrieve-synchronously (format "http://localhost:3496%s?%s" path args)))
         )
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
  ;; (message "JPL: In ps/completions-list-for-self")
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

;; Needs to be run on symbol_server
;; Needs to look in base classes too
(defun ps/find-tip-method-calls-list (tip-string current-completion-string)
  (save-excursion
    (goto-char (point-min))
    (let ((calls-list))
      (while (search-forward-regexp
              (format "%s->\\([a-zA-Z0-9_]+\\)" (regexp-quote tip-string))
              nil
              t)
        (let ((current-call (match-string-no-properties 1)))
          (unless (string= current-call current-completion-string)
            (push current-call calls-list)))
        )
      (setq calls-list (delete-dups calls-list))
      ;; (message "JPL: Methods: %s" (prin1-to-string calls-list))
      calls-list
      )
    )
  )

(defun ps/completions-list-for-chain-tip (tip-string current-completion-string)
  "Get completion list for '$tip'->acb or
$chain_root->'tip'->abc, i.e. the last piece of a call chain.

This is a very naivielme match to start with, assuming anything that
is called from the tip can be used to distinguish the type,
regardless of the scope of the $tip or $chain_root."
  ;; (message "JPL: ps/completions-list-for-chain-tip")
  (let ((tip-method-calls (ps/find-tip-method-calls-list tip-string current-completion-string)))
    (if tip-method-calls
        (ps/call-repository-server-parse-sexp-get-key
         "/method/complete"
         (format
          "method_call_starts_with=%s&%s"
          current-completion-string
          (mapconcat (lambda (class-name)
                       (format "method_call=%s" class-name))
                     tip-method-calls
                     "&")
          )
         "completions"
         )
      (message "Could not figure out what (%s) is." tip-string)
      '()
      )
    )
  )

(defun ps/ac-candidates-from-completions-list (completions-list)
  ;; (message "JPL: completions: %s" (prin1-to-string completions-list))
  (mapcar
   (lambda (completion-alist)
     (let ((sub-name     (alist-value completion-alist "method_name"))
           (package-name (alist-value completion-alist "api_package")))
       (propertize sub-name 'summary package-name)
       )
     )
   completions-list))

(defun ps/completions-list-for-thing-at-point ()
  (cond
   ((looking-back "$self->\\([a-zA-Z0-9_]+\\)")
    (ps/completions-list-for-self)
    )
   ((looking-back "\\($?[a-zA-Z0-9_]+\\)->\\([a-zA-Z0-9_]+\\)" nil t)
    (ps/completions-list-for-chain-tip (match-string-no-properties 1) (match-string-no-properties 2))
    )
   (t
    '()
    )
   )
  )

(defun ps/ac-candidates ()
  (interactive) ;; JPL
  ;; (message "JPL: ps/ac-candidates")
  (ps/ac-candidates-from-completions-list
   (ps/completions-list-for-thing-at-point)))

(defun ps/candidate-documentation (symbol-name)
  ;; (save-excursion
  ;;   (goto-char (point-min))
  ;;   (when (search-forward-regexp (format "sub +%s " symbol-name) nil t)
  ;;     (when (search-backward-regexp "=cut\\|}" nil t)
  ;;       (when (looking-at "=cut")
  ;;         (previous-line)
  ;;         (let* ((pod-end-pos (point)))
  ;;           (when (search-backward-regexp "=head" nil t)
  ;;             (forward-word) (forward-char)
  ;;             (let* ((pod-docs
  ;;                     (buffer-substring-no-properties (point) pod-end-pos)))
  ;;               pod-docs
  ;;               )
  ;;             )
  ;;           )
  ;;         )
  ;;       )
  ;;     )
  ;;   )
  ""
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
    (prefix . "->\\([a-zA-Z0-9_]+\\)")
    (candidates . ps/ac-candidates)
    (requires . 0)
    (limit . 100) ;; JPL
    (document . ps/candidate-documentation)
    )
  )

(defun ps/smart-complete-at-point ()
  (interactive)
  (auto-complete '(ps/ac-source-method-calls)) ;; For now
  )
(global-set-key (format "%s\C-c" ps/key-prefix) 'ps/smart-complete-at-point)


; prefix: use


;; For well-behaved (save-buffer-excursion) in a live buffer
;; (let ((inhibit-point-motion-hooks t))) ;; if there are point-entered hooks


; (setq ac-sources '(ps/ac-source-method-calls))

(provide 'perly-sense-autocomplete)

