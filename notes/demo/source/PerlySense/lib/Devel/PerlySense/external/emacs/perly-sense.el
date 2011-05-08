
;;
;; INSTALLATION
;;
;; See the instructions at:
;; http://search.cpan.org/dist/Devel-PerlySense/lib/Devel/PerlySense.pm#Emacs_installation
;;




;; for the faces
(require 'compile)
(require 'grep)  



(defun perly-sense-log (msg)
  "log msg in a message and return msg"
;;  (message "LOG(%s)" msg)
  )


(defun perly-sense-current-line ()
  "Return the vertical position of point"
  (+ (count-lines 1 (point))
     (if (= (current-column) 0) 1 0)
     )
  )


(defun perly-sense-find-source-for-module (module)
  (let ((file (shell-command-to-string (format "perly_sense find_module_source_file --module=%s" module))))
    (if (not (string-equal file ""))
        (find-file file)
      (message "Module (%s) source file not found" module)
      nil
      )
    )
  )


(defun perly-sense-find-source-for-module-at-point ()
  "Find the source file for the module at point."
  (interactive)
  (let ((module (cperl-word-at-point)))
    (if module
        (perly-sense-find-source-for-module module)
      )
    )
  )



; should use something that fontifies
(defun perly-sense-display-pod-for-module (module)
  (shell-command (format "perly_sense display_module_pod --module=%s" module)
                 (format "*%s POD*" module)
                 )
  )

; should use something that fontifies
(defun perly-sense-display-pod-for-file (file name-buffer)
  (shell-command (format "perly_sense display_file_pod --file=%s" file)
                 (format "*%s POD*" name-buffer)
                 )
  )


(defun perly-sense-display-pod-for-module-at-point ()
  "Display POD for the module at point."
  (interactive)
  (let ((module (cperl-word-at-point)))
    (if module
        (perly-sense-display-pod-for-module module)
      )
    )
  )




(defun perly-sense-smart-docs (word)
  "Display documentation for word."
  (if word
      (perly-sense-display-pod-for-module word)
    (perly-sense-display-pod-for-file (buffer-file-name) (buffer-name))
    )
  )





; Split result into first line and the rest and return the first line
(defun perly-sense-result-status (result)
  (car (split-string result "[\n]"))
)

; Split result into first line and the rest and return the rest
(defun perly-sense-result-text (result)
  (mapconcat 'identity (cdr (split-string result "[\n]")) "\n")
)

; Split the Split result into first line and the rest and return those two
(defun perly-sense-result-properties (result)
  (split-string (car (split-string result "[\n]")) "[\t]")
  )





(defun perly-sense-display-text-in-buffer (type name text)
  (let ((buffer-name (format "*%s %s*" type name)))
    (with-current-buffer (get-buffer-create buffer-name)
      (erase-buffer)
      (insert text)
      (goto-char 1)
      (perly-sense-fontify-pod-buffer buffer-name)
      (display-buffer (current-buffer))
      )
    )
  )


(defun perly-sense-display-doc-message-or-buffer (doc-type name text)
  (cond ((string= doc-type "hint")
         (message "%s" text))
        ((string= doc-type "document")
         (perly-sense-display-text-in-buffer "POD" name text)
         (message nil)
         )
        )
  t
  )





(defun perly-sense-fontify-pod-buffer (buffer-name)
  "Mark up a buffer with text from pod2text."
  (interactive)
  (save-excursion
    (set-buffer buffer-name)
    (goto-char (point-min))
    (while (search-forward-regexp "
 \\{4,\\}" nil t)
      (let* ((point-start (point)))
        (search-forward-regexp "
")
        (backward-char)
        (put-text-property point-start (point) 'face '(:foreground "Gray50"))   ;;TODO: Move to config variable
        )
      )
    )
  )




(defun perly-sense-run-file ()
  "Run the current file"
  (interactive)

  ;;If it's the compilation buffer, recompile, else run file
  (if (string= (buffer-name) "*compilation*")
      (progn
        (message "Recompile file...")
        (recompile)
        )
    (progn
      (message "Run File...")
      (let ((result
             (shell-command-to-string
              (format "perly_sense run_file --file=%s" (buffer-file-name)))
             ))
        (let* (
               (result-hash (perly-sense-parse-sexp result))
               (dir-run-from (cdr (assoc "dir-run-from" result-hash)))
               (command-run (cdr (assoc "command-run" result-hash)))
               (type-source-file (cdr (assoc "type-source-file" result-hash)))
               (message-string (cdr (assoc "message" result-hash)))
               )
          (if command-run
              (perly-sense-run-file-run-command
               ;;             (perly-sense-run-file-get-command command-run type-source-file)
               command-run
               dir-run-from
               )
            )
          (if message-string
              (message message-string)
            )
          )
        )
      )
    )
  )

(defun perly-sense-run-file-run-command (command dir-run-from)
  "Run command from dir-run-from using the compiler function"
  (with-temp-buffer
    (cd dir-run-from)
    (compile command)
    )
  )



(defun perly-sense-rerun-file ()
  "Rerun the current compilation buffer"
  (interactive)
  (let* ((compilation-buffer (get-buffer "*compilation*")))
    (if compilation-buffer
        (let* ((compilation-window (get-buffer-window compilation-buffer "visible")))
          (progn
            (if compilation-window (select-window compilation-window))
            (switch-to-buffer "*compilation*")
            (recompile))
          )
      (message "Can't re-run: No Run File in progress.")
      )
    )
  )






;found   method  name    levelIndent     docType hint
(defun perly-sense-smart-docs-at-point ()
  "Display documentation for the code at point."
  (interactive)
  (message "Smart docs...")
  (let* (
         (result (shell-command-to-string
                  (format "perly_sense smart_doc --file=%s --row=%s --col=%s"
                          (buffer-file-name) (perly-sense-current-line) (+ 1 (current-column))
                          )
                  ))
         (result-text (perly-sense-result-text result))
         )
    (if (not (string= result ""))
        (let*
            (
             (properties (perly-sense-result-properties result))
             (found    (nth 1 properties))
             (name     (nth 3 properties))
             (doc-type (nth 5 properties))
             )
          (perly-sense-display-doc-message-or-buffer doc-type name result-text)
          )
      (message "Nothing found")
      )
    )
  )





(defun perly-sense-find-file-location (file row col)
  "Find the file and go to the row col location"
  (set-mark-command nil)
  (find-file file)
  (goto-line row)
  (beginning-of-line)
  (forward-char (- col 1))
  )




(defun perly-sense-smart-go-to-at-point ()
  "Go to the original symbol in the code at point."
  (interactive)
  (message "Smart goto...")
  (let ((result (shell-command-to-string
               (format "perly_sense smart_go_to --file=%s --row=%s --col=%s"
                       (buffer-file-name) (perly-sense-current-line) (+ 1 (current-column))
                       )
               ))
        )
    (if (string-match "[\t]" result)
        (let ((value (split-string result "[\t]")))
          (let ((file (pop value)))
            (perly-sense-find-file-location file (string-to-number (pop value)) (string-to-number (pop value)))
            (message "Went to: %s" file)
            )
          )
      (message nil)
      )
    )
  )




(defun perly-sense-class-overview-for-class-at-point ()
  "Display the Class Overview for the current class"
  (interactive)
  (perly-sense-class-overview-with-argstring
   (format
    "perly_sense class_overview --file=%s --row=%s --col=%s"
    (buffer-file-name)
    (perly-sense-current-line)
    (+ 1 (current-column)))))


(defun perly-sense-class-overview-with-argstring (argstring)
  "Call perly_sense class_overview with argstring and display Class Overview with the response"
  (interactive)
  (message "Class Overview...")
  (let ((result (shell-command-to-string
                 (format
                  "perly_sense class_overview %s --width_display=%s"
                  argstring (- (window-width) 2) ))))
    (let* (
           (result-hash (perly-sense-parse-sexp result))
           (class-name (cdr (assoc "class-name" result-hash)))
           (class-overview (cdr (assoc "class-overview" result-hash)))
           (message-string (cdr (assoc "message" result-hash)))
           (dir (cdr (assoc "dir" result-hash)))
           )
;;      (perly-sense-log msg)
      (if class-name
          (perly-sense-display-class-overview class-name class-overview dir)
        )
      (if message-string
          (message message-string)
        )
      )
    )
  )




(defun perly-sense-parse-sexp (result)
;;  (message "%s" result)
  (eval (car (read-from-string result)))
  )


;;;(perly-sense-parse-result-into-alist "'((\"class-overview\" . \"Hej baberiba [ Class::Accessor ]\") (\"class-name\" . \"Class::Accessor\") (\"message\" . \"Whatever\"))")
;;(perly-sense-parse-result-into-alist "'((\"class-name\" . \"alpha\"))")





(defun perly-sense-display-api-for-class-at-point ()
  "Display the likely API of the class at point."
  (interactive)
  (message "Class API...")
  (let* ((text (shell-command-to-string
               (format "perly_sense class_api --file=%s --row=%s --col=%s"
                       (buffer-file-name) (perly-sense-current-line) (+ 1 (current-column))
                       )
               ))
         (package (perly-sense-result-status text))
         (package-doc (perly-sense-result-text text))
         )
                                        ; TODO: if package eq ""
    (perly-sense-display-text-in-buffer "API" package package-doc)
    (other-window 1)
    (compilation-mode)
    (goto-line 2)
    )
  )





;; PerlySense Class major mode

;;;



(defun perly-sense-display-class-overview (class-name overview-text dir)
  (let ((buffer-name "*Class Overview*"))
    (with-current-buffer (get-buffer-create buffer-name)
(message "dir (%s)" dir)
      (setq default-directory dir)
      (toggle-read-only t)(toggle-read-only)  ; No better way?
      (erase-buffer)
      (insert overview-text)
      (perly-sense-class-mode)
      (perly-sense-fontify-class-overview-buffer buffer-name)
      (perly-sense-search-current-class-name class-name)
      (switch-to-buffer (current-buffer))  ;; before: display-buffer
      (toggle-read-only t)
      )
    )
  )



;; Set point where class-name is mentioned in brackets
(defun perly-sense-search-class-name (class-name)
  (let ((class-name-box (format "[ %s " class-name)))
    (goto-char (point-min))
    (search-forward class-name-box)
    (search-backward "[ ")
    (forward-char)
    )
  )



;; Set point where class-name is mentioned in current [< xxx >]
(defun perly-sense-search-current-class-name (class-name)
  (let ((class-name-box (format "[<%s" class-name)))
    (goto-char (point-min))
    (search-forward class-name-box nil t)
    (search-backward "[<" nil t)
    (forward-char)
    )
  )



(defun perly-sense-fontify-class-overview-buffer (buffer-name)
  "Mark up a buffer with Class Overview text."
  (interactive)
  (save-excursion
    (set-buffer buffer-name)

    (goto-char (point-min))
    (while (search-forward-regexp "\\[ \\w+ +\\]" nil t)
      (put-text-property (match-beginning 0) (match-end 0) 'face cperl-array-face)) ;'(:background "Gray80"))   ;;TODO: Move to config variable

    (goto-char (point-min))
    (while (search-forward-regexp "\\[<\\w+ *>\\]" nil t)
      (put-text-property (match-beginning 0) (match-end 0) 'face cperl-hash-face)) ;'(:background "Gray80"))   ;;TODO: Move to config variable

    (goto-char (point-min))
    (while (search-forward-regexp "^[^:\n]+:[0-9]+:" nil t)
      (let
          ((file-beginning (match-beginning 0))
           (row-end (- (match-end 0) 1)))
        (search-backward-regexp ":[0-9]+:" nil t)
        (let
            ((file-end (match-beginning 0))
             (row-beginning (+ (match-beginning 0) 1)))
          (put-text-property file-beginning file-end 'face grep-hit-face)
          (put-text-property row-beginning row-end 'face 'compilation-line-number)
          )))

;;grep-context-face
;;     (goto-char (point-min))
;;     (while (search-forward-regexp "\\* \\w+ +\\*" nil t)
;;       (put-text-property (match-beginning 0) (match-end 0) 'face cperl-hash-face))
;;     )
    )
  )




(defun perly-sense-compile-goto-error-file-line ()
  "Go to the file + line specified on the row at point, or ask for a
text to parse for a file + line."
  (interactive)
  (let* ((file_row (perly-sense-compile-get-file-line-from-buffer) )
         (file (nth 0 file_row))
         (row (nth 1 file_row)))
    (if file
        (perly-sense-find-file-location file (string-to-number row) 1)
      (let* ((file_row (perly-sense-compile-get-file-line-from-user-input) )
             (file (nth 0 file_row))
             (row (nth 1 file_row)))
        (if file
            (perly-sense-find-file-location file (string-to-number row) 1)
          (message "No 'FILE line N' found")
          )
        )
      )
    )
  )


(defun perly-sense-compile-get-file-line-from-user-input ()
  "Ask for a text to parse for a file + line, parse it using
'perly-sense-compile-get-file-line-from-buffer'. Return what it
returns."
  (with-temp-buffer
    (insert (read-string "FILE, line N text: " (current-kill 0 t)))
    (perly-sense-compile-get-file-line-from-buffer)
    )
  )


(defun perly-sense-compile-get-file-line-from-buffer ()
  "Return a two item list with (file . row) specified on the row at
point, or an empty list () if none was found."
  (save-excursion
    (end-of-line)
    (set-mark (point))
    (beginning-of-line)
    (if (search-forward-regexp
         "\\(file +`\\|at +\\)\\([/a-zA-Z0-9._ ]+\\)'? +line +\\([0-9]+\\)[.,]"
         (region-end) t)
        (let* ((file (match-string 2))
               (row (match-string 3)))
          (list file row)
          )
      (list)
      )
    )
  )





;;;;;


(defun perly-sense-class-goto-at-point ()
  "Go to the class/method/bookmark at point"
  (interactive)
  (message "Goto at point")
  (let* ((class-name (perly-sense-find-class-name-at-point)))
         (if class-name
             (progn
               (message (format "Going to class (%s)" class-name))
               (perly-sense-find-source-for-module class-name)
               )
           (if (not (perly-sense-class-goto-bookmark-at-point))
               (message "No Class or Bookmark at point")
             )
           )
         )
  )


(defun perly-sense-class-docs-at-point ()
  "Display docs for the class/method at point"
  (interactive)
  (message "Docs at point")
  (let* ((class-name (perly-sense-find-class-name-at-point)))
         (if class-name
             (progn
               (message (format "Finding docs for class (%s)" class-name))
               (perly-sense-display-pod-for-module class-name)
               )
           (message "No Class at point")
           )
         )
  )


(defun perly-sense-class-goto-bookmark-at-point ()
  "Go to the bookmark at point, if there is any.
Return t if there was any, else nil"
  (interactive)
  (message "Goto bookmark at point")
  (save-excursion
    (beginning-of-line)
    (if (search-forward-regexp "^\\([^:\n]+\\):\\([0-9]+\\):" (point-at-eol) t)
        (let ((file (match-string 1)) (row (string-to-number (match-string 2))))
          (message "file (%s) row (%s)" file row)
          (perly-sense-find-file-location file row 1)
          t
          )
      nil
      )
    )
  )




(defun perly-sense-class-class-overview-at-point ()
  "Display Class Overview for the class/method at point"
  (interactive)
  (message "Class Overview at point")
  (let* ((class-name (perly-sense-find-class-name-at-point)))
    (if class-name
        (progn
          (message (format "Class Overview for class (%s)" class-name))
          (perly-sense-class-overview-with-argstring
           (format "--class_name=%s --dir_origin=." class-name)))
      (message "No Class at point")
      )
    )
  )



(defun perly-sense-class-class-overview-or-goto-at-point ()
  "Display Class Overview for the class/method at point,
or go to the Bookmark at point"
  (interactive)
  (message "Class Overview at point")
  (let* ((class-name (perly-sense-find-class-name-at-point)))
    (if class-name
        (progn
          (message (format "Class Overview for class (%s)" class-name))
          (perly-sense-class-overview-with-argstring
           (format "--class_name=%s --dir_origin=." class-name)))
      (if (not (perly-sense-class-goto-bookmark-at-point))
          (message "No Class or Bookmark at point")
        )
      )
    )
  )



(defun perly-sense-class-quit ()
  "Quit the Class Overview buffer"
  (interactive)
  (message "Quit")
  (kill-buffer nil)
  )



(defun perly-sense-class-find-inheritance ()
  "Navigate to the * Inheritance * in the Class Overview"
  (interactive)
  (goto-char (point-min))
  (search-forward "* Inheritance *" nil t)
  (search-forward "[<" nil t)
  (backward-char)
  )



(defun perly-sense-class-find-neighbourhood ()
  "Navigate to the * NeighbourHood * in the Class Overview"
  (interactive)
  (goto-char (point-min))
  (search-forward "* NeighbourHood *" nil t)
  (search-forward "[<" nil t)
  (backward-char)
  )



(defun perly-sense-class-find-used ()
  "Navigate to the * Uses * in the Class Overview"
  (interactive)
  (goto-char (point-min))
  (search-forward "* Uses *" nil t)
  (beginning-of-line 2)
  (forward-char)
  )



(defun perly-sense-class-find-bookmarks ()
  "Navigate to the * Bookmarks * in the Class Overview"
  (interactive)
  (goto-char (point-min))
  (search-forward "* Bookmarks *" nil t)
  (beginning-of-line 2)
  (if (looking-at "-")
      (beginning-of-line 2)
    )
  )



(defun perly-sense-class-find-structure ()
  "Navigate to the * Structure * in the Class Overview"
  (interactive)
  (goto-char (point-min))
  (search-forward "* Structure *" nil t)
  (search-forward "-" nil t)
  (beginning-of-line 2)
  )



(defun perly-sense-find-class-name-at-point ()
  "Return the class name at point, or nil if none was found"
  (save-excursion
    (if (looking-at "[\\[]")
        (forward-char) ;; So we can search backwards without fear of missing the current char
      )
    (if (search-backward-regexp "[][]" nil t)
        (if (looking-at "[\\[]")
            (progn
              (search-forward-regexp "\\w+" nil t)
              (match-string 0)
              )
          )
      )
    )
  )



(defvar perly-sense-class-mode-map nil
  "Keymap for `PerlySense Class overview major mode'.")
(if perly-sense-class-mode-map
    ()
  (setq perly-sense-class-mode-map (make-sparse-keymap)))
(define-key perly-sense-class-mode-map "q" 'perly-sense-class-quit)
(define-key perly-sense-class-mode-map "I" 'perly-sense-class-find-inheritance)
(define-key perly-sense-class-mode-map "H" 'perly-sense-class-find-neighbourhood)
(define-key perly-sense-class-mode-map "U" 'perly-sense-class-find-used)
(define-key perly-sense-class-mode-map "B" 'perly-sense-class-find-bookmarks)
(define-key perly-sense-class-mode-map "S" 'perly-sense-class-find-structure)
;;(define-key perly-sense-class-mode-map "M" 'perly-sense-class-find-methods)
(define-key perly-sense-class-mode-map "d" 'perly-sense-class-docs-at-point)
(define-key perly-sense-class-mode-map "g" 'perly-sense-class-goto-at-point)
(define-key perly-sense-class-mode-map "c" 'perly-sense-class-class-overview-at-point)
(define-key perly-sense-class-mode-map [return] 'perly-sense-class-class-overview-or-goto-at-point)



(defvar perly-sense-class-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Treat _ and :: as part of a word
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?: "w" st)
    st)
  "Syntax table for `perly-sense-class-mode'.")


;; (Defvar perly-sense-class-imenu-generic-expression
;;   ...)

;; (defvar perly-sense-class-outline-regexp
;;   ...)

 ;;;###autoload
(define-derived-mode perly-sense-class-mode fundamental-mode "PerlySense Class Overview"
  "A major mode for viewing PerlySense Class overview buffers."
  :syntax-table perly-sense-class-mode-syntax-table
;;   (set (make-local-variable 'comment-start) "# ")
;;   (set (make-local-variable 'comment-start-skip) "#+\\s-*")

;;   (set (make-local-variable 'font-lock-defaults)
;;        '(perly-sense-class-font-lock-keywords))

;;   (set (make-local-variable 'indent-line-function) 'perly-sense-class-indent-line)
;;   (set (make-local-variable 'imenu-generic-expression)
;;        perly-sense-class-imenu-generic-expression)
;;   (set (make-local-variable 'outline-regexp) perly-sense-class-outline-regexp)
  )

;;; Indentation

;; (defun perly-sense-class-indent-line ()
;;   "Indent current line of Perly-Sense-Class code."
;;   (interactive)
;;   (let ((savep (> (current-column) (current-indentation)))
;;         (indent (condition-case nil (max (perly-sense-class-calculate-indentation) 0)
;;                   (error 0))))
;;     (if savep
;;         (save-excursion (indent-line-to indent))
;;       (indent-line-to indent))))

;; (defun perly-sense-class-calculate-indentation ()
;;   "Return the column to which the current line should be indented."
;;   ...)



;; Key bindings
;;;; TODO: move some of these to cperl-mode local bindings

(global-set-key (format "%smf" perly-sense-key-prefix) 'perly-sense-find-source-for-module-at-point)
(global-set-key (format "%smp" perly-sense-key-prefix) 'perly-sense-display-pod-for-module-at-point)

(global-set-key (format "%s\C-d" perly-sense-key-prefix) 'perly-sense-smart-docs-at-point)
(global-set-key (format "%s\C-g" perly-sense-key-prefix) 'perly-sense-smart-go-to-at-point)
(global-set-key (format "%s\C-c" perly-sense-key-prefix) 'perly-sense-class-overview-for-class-at-point)
;; (global-set-key (format "%s\C-c" perly-sense-key-prefix) 'perly-sense-display-api-for-class-at-point)

(global-set-key (format "%s\C-r" perly-sense-key-prefix) 'perly-sense-run-file)
(global-set-key (format "%srr" perly-sense-key-prefix) 'perly-sense-rerun-file)

(global-set-key (format "%sge" perly-sense-key-prefix) 'perly-sense-compile-goto-error-file-line)





(provide 'perly-sense)





;; EOF
