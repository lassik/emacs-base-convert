;;; base-convert-live.el --- Number base conversion as you type -*- lexical-binding: t -*-
;;
;; SPDX-License-Identifier: ISC
;; Author: Lassi Kortela <lassi@lassi.io>
;; URL: https://github.com/lassik/emacs-base-convert-live
;; Package-Requires: ((emacs "25.1"))
;; Version: 0.1.0
;; Keywords: convenience
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Convert number from one base (aka radix) to others.
;;
;; Binary (base 2), octal (base 8), decimal (base 10) and hexadecimal
;; (base 16) are supported for both input and output.
;;
;; The converter doesn't ask you which input and output base you want
;; to use. Instead, it accepts free-form text input, tries to parse it
;; using all supported bases, and displays successful conversions of
;; those numbers to all supported bases.
;;
;; The display is updated live as you type.
;;
;;; Code:

(defun base-convert-live--format-binary (uint)
  "Internal helper to format the unsigned integer UINT as binary."
  (with-temp-buffer
    (let ((done nil))
      (while (not done)
        (insert (+ ?0 (logand uint 1)))
        (setq uint (ash uint -1))
        (setq done (= uint 0))))
    (nreverse (buffer-string))))

(defun base-convert-live--table-row (input radix)
  "Internal helper to generate one row of the conversion table.

INPUT is any string. RADIX is the number base to try parsing as."
  (let ((integer (condition-case _ (cl-parse-integer input :radix radix)
                   (error nil))))
    (and integer
         (let ((sign (if (< integer 0) "-" "")))
           (setq integer (abs integer))
           (list (cons (format "%s%s" sign
                               (base-convert-live--format-binary integer))
                       (= radix 2))
                 (cons (format "%s%o" sign integer)
                       (= radix 8))
                 (cons (format "%s%d" sign integer)
                       (= radix 10))
                 (cons (format "%s%x" sign integer)
                       (= radix 16)))))))

(defun base-convert-live--table-rows (input)
  "Internal helper to generate all rows of the conversion table.

INPUT is any string."
  (cl-remove-if #'null
                (list '(("Bin" . t)
                        ("Oct" . t)
                        ("Dec" . t)
                        ("Hex" . t))
                      (base-convert-live--table-row input 2)
                      (base-convert-live--table-row input 8)
                      (base-convert-live--table-row input 10)
                      (base-convert-live--table-row input 16))))

(defun base-convert-live--display (input)
  "Ingernal helper to display conversion window for INPUT."
  (with-current-buffer-window
      "*Base-Convert*" nil nil
    (let* ((inhibit-read-only t)
           (rows (base-convert-live--table-rows input))
           (col-count (length (car rows)))
           (max-widths (make-list col-count 0)))
      (dolist (row rows)
        (dotimes (col col-count)
          (let* ((col-pair (elt row col))
                 (col-text (car col-pair))
                 (col-width (length col-text))
                 (max-width (elt max-widths col)))
            (setcar (nthcdr col max-widths)
                    (max max-width col-width)))))
      (erase-buffer)
      (dolist (row rows)
        (dotimes (col col-count)
          (let* ((col-pair (elt row col))
                 (col-text (car col-pair))
                 (col-highlight-p (cdr col-pair))
                 (col-width (length col-text))
                 (max-width (elt max-widths col))
                 (pad-amount (max 0 (- max-width col-width)))
                 (pad (make-string pad-amount ? ))
                 (start (point)))
            (insert col-text)
            (when col-highlight-p
              (let ((overlay (make-overlay start (point))))
                (overlay-put overlay 'face 'highlight)))
            (insert pad "  ")))
        (insert "\n\n")))))

(defun base-convert-live--display-from-minibuffer (&rest _ignored)
  "Internal helper to display conversion window for minibuffer input."
  (base-convert-live--display (minibuffer-contents)))

;;;###autoload
(defun base-convert-live ()
  "Show base conversions for number typed into the minibuffer.

All possible conversions are displayed in a pop-up window and are
updated live as you type."
  (interactive)
  (minibuffer-with-setup-hook
      (lambda () (add-hook 'after-change-functions
                           #'base-convert-live--display-from-minibuffer
                           nil 'local))
    (read-string "Digits to convert: " "")))

(provide 'base-convert-live)

;;; base-convert-live.el ends here
