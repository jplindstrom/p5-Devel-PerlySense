;;; lang-refactor-perl.el --- Simple refactorings, primarily for Perl

;; Copyright © 2013 Johan Linsdtrom
;;
;; Author: Johan Lindstrom <buzzwordninja not_this_bit@googlemail.com>
;; URL: https://github.com/jplindstrom/emacs-lang-refactor-perl
;; Version: 0.1.3
;; Keywords: languages, refactoring, perl

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;
;;; Commentary:
;;
;; Provides commands for simple refactorings for Perl, currently:
;; * extract variable.
;;

;;
;;; Installation:
;;
;; Put in load-path and require.
;;

;;
;;; Usage:
;;
;; Mark a region of code you want to extract and then
;;     M-x lr-extract-variable
;;
;; All edits are highlighted. Once you've eye-balled the refactoring,
;; run
;;     M-x lr-remove-highlights
;; to remove them.
;;
;; For more details, see the function documentation:
;;     M-h f lr-extract-variable
;;
;;
;; Suggested key bindings, forwards compatible with future
;; refactorings and other features (like "Toggle Highlight"):
;;    (global-set-key (kbd "\C-c r e v") 'lr-extract-variable)
;;    (global-set-key (kbd "\C-c r h r") 'lr-remove-highlights)
;;
;;



;;; Code:


;; TODO: defcustom
(defvar lr-extract-variable-face
  '(:background "bisque"))

;; (defvar lr/extract-variable-restore-face
;;   '(:background 'inherit))

;; to reset
;; (setq lr-extract-variable-face
;;   '(:background "bisque"))
;; (setq lr/extract-variable-restore-face
;;   '(:background "red"))


(defun lr/open-line-above ()
  "Insert a newline above the current line and put point at beginning."
  (interactive)
  (unless (bolp)
    (beginning-of-line))
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(defun lr/get-variable-name (expression)
    (if (string-match "\\([[:alnum:]_]+?\\)\\([^[:alnum:]_]+?\\)?$" expression)
        (format  "$%s" (match-string-no-properties 1 expression))
      (error "Could not find a variable name in (%s)" expression)
      )
  )

(defun lr/replace-all-buffer (search-for replace-with)
  (goto-char (point-min))
  ;; TODO: match word boundary to avoid substring matches
  (while (search-forward search-for nil t)
    (replace-match replace-with nil nil))
  )

(defun lr/goto-earliest-usage (variable-name)
  (goto-char (point-min))
  ;; TODO: match word boundary to avoid substring matches
  (search-forward variable-name nil t)

  ;; if possible, find previous statement terminator ; or closing block }
  (when (search-backward-regexp "[;}]" nil t)
      (forward-line)
      (while (and
              (looking-at "\n")
              (not (eobp)))
        (forward-line))
      )
  )

(defun lr/insert-declaration (variable-declaration)
  (lr/open-line-above)
  (insert variable-declaration)
  (beginning-of-line)
  (search-forward "= " nil t)
  )

;;;###autoload
(defun lr-extract-variable (beg end)
  "Do refactoring 'extract Perl variable' of active region.

Ask the user for a variable name to extract the active region
into.

Replace all occurences in the current defun with the variable and
insert a variable declarion (initialized with the region text).

Push the mark and then leave point at the new variable
declaration (you'll need to ensure this is a reasonable location
before jumping back).

By default, only the current defun is changed. Invoke with the
prefix arg to change the entire buffer.

Both replacements and the declaration are highlighted."
  (interactive "r")
  ;; TODO: timer to remove highlighting after x seconds
  ;; TODO:     using a nice fade
  (unless (and transient-mark-mode mark-active)
    (error "Select a self-contained piece of code to extract"))
  (set-mark-command nil)
  (let*
      ((should-narrow-to-defun (not current-prefix-arg))
       (expression (buffer-substring-no-properties beg end))
       (variable-name-suggestion (lr/get-variable-name expression))
       (variable-name (read-string
                       (format "Extract (%s) to variable: " expression)
                       variable-name-suggestion nil))
       (formatted-variable-name (propertize variable-name
                                            ;; 'font-lock-face lr-extract-variable-face
                                            'category 'lr-edit
                                            ))
       (variable-declaration (format "my %s = %s;" formatted-variable-name expression))
       )
    (save-restriction
      (when should-narrow-to-defun
        (narrow-to-defun))
      (lr/replace-all-buffer expression formatted-variable-name)
      (lr/goto-earliest-usage variable-name)
      (lr/insert-declaration variable-declaration)
      (lr/highlight 'lr-edit)
      )
    )
  )


;;;###autoload
(defun lr-remove-highlights ()
  (interactive)
  (lr/remove-highlights 'lr-edit)
  )

(defun lr/remove-highlights (category)
  "Remove all lr highlights from sections with CATEGORY"
  (lr/do-fn-for-catgory
   category
   (lambda (begin end)
     ;; Restore face
     (lr/set-face-property-at begin end font-lock-variable-name-face)
     ;; Remove category tag, so it doesn't get highlighted again
     (lr/remove-category-at begin end 'lr-edit)
     )
   )
  )

(defun lr/set-face-property-at (begin end face)
  "Set the font-lock-face property to FACE between BEGIN and END"
  (add-text-properties begin end (list 'font-lock-face face))
  )

(defun lr/remove-category-at (begin end category)
  "Remove the category property CATEGORY between BEGIN and END"
  (remove-text-properties begin end (list 'category category))
  )

(defun lr/highlight (category)
  "Highlight all sections with category CATEGORY"
  (lr/do-fn-for-catgory
   category
   (lambda (begin end)
     (lr/set-face-property-at begin end lr-extract-variable-face)
     )))

(defun lr/do-fn-for-catgory (category do-fn)
  "Call 'do-fn begin end' for sections tagged with CATEGORY"
  (save-excursion
    (goto-char (point-min))
    (while
        (let* (
               (begin (text-property-any (point) (point-max) 'category category))
               (safe-begin (or begin (point-max)))
               (end (or ;; End of section, or end of buffer
                     (text-property-not-all safe-begin (point-max) 'category category)
                     (point-max)))
               )
          (if (and begin (not (eq begin (point-max))))
              (progn
                (funcall do-fn begin end)
                (goto-char (+ 1 end))
                )
            nil
            ))
      )
    ))



(provide 'lang-refactor-perl)

;;; lang-refactor-perl.el ends here
