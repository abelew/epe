
;;; epe-genbank.el --- Copying seqel's genbank-mode and playing with it

;; I hope that this will significantly different from Zech Xu's code,
;; but for the moment it is entirely derivative of it, thus:

;; Copyright (C) 2021  Zech Xu

;; Author: Zech Xu
;; Version: 1.0
;; License: BSD-3
;; URL: https://github.com/RNAer/seqel

;;; Commentary:

;; Major mode to edit genbank files

;;; Code:

;; Some things I would like to change/add:
;; 1.  Write out fasta files in the 80 character/line format that I prefer.
;; 2.  Play with the ways one may navigate a genbank file and output
;;     subsequences thereof.
;; 3.  Ideally make this the basis for collecting features and annotations
;;     in a fashion similar to how ApE handles genbank files.
;; 4.  I would like to have some datastructures a bit similar to Perl's
;;     Bio::SeqIO/Bio::SeqFeature to make it possible to create new seq/features.
;; 5.  I would like to have a way to highlight the appropriate region of the
;;     primary sequence when the cursor is over a feature (which I think requires #4).
;; 6.  Add more font-lock information for sequence features (to make #5 more fun).
;; 7.  I want to use code similar to this:
;;     https://kitchingroup.cheme.cmu.edu/blog/2017/04/07/Object-oriented-font-locking-in-emacs-lisp/#orga752041


(defvar epe-genbank-mode-hook nil
  "*Hook to setup `epe-genbank-mode'.")


(defvar epe-genbank-mode-map
  ;; use `make-keymap' if there are lots of keybindings
  (let ((map (make-sparse-keymap)))
    ;; Ctrl bindings
    (define-key map "\C-c\C-a"  'epe-genbank-first)
    (define-key map "\C-c\C-z"  'epe-genbank-last)
    (define-key map "\C-c\C-c"  'epe-genbank-count)
    (define-key map "\C-c\C-d"  'epe-genbank-delete)
    (define-key map "\C-c\C-f"  'epe-genbank-forward) ; it also binds to M-}
    (define-key map "\C-c\C-b"  'epe-genbank-backward); it also binds to M-}
    (define-key map "\C-c\C-m"  'epe-genbank-mark)    ; it also binds to M-h
    map)
 "The local keymap for `epe-genbank-mode'.")


;;;###autoload
(add-to-list 'auto-mode-alist
             '("\\.\\(genbank\\|gb\\|gbk\\)\\'" . epe-genbank-mode))


;; map the paragraph key bindings to corresponding functions
(let ((equivs
       '((epe-genbank-forward  . forward-paragraph)
         (epe-genbank-backward . backward-paragraph)
         (epe-genbank-mark     . mark-paragraph))))
  (dolist (x equivs)
    (substitute-key-definition (cdr x)
                               (car x)
                               epe-genbank-mode-map
                               (current-global-map))))


(defvar epe-genbank-font-lock-keywords
  '(("^\\(LOCUS\\) +\\([-_.a-zA-Z_0-9]+\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face))
    ("^[[:space:]]+\\(source\\)[[:space:]]+\\([[:digit:]]\\)\\.+\\([[:digit:]]\\)+$"
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face)
     (3 font-lock-function-name-face))
    ("^\\(VERSION\\) +\\([-_.a-zA-Z_0-9]+\\) +\\([Gg][Ii]\\):\\([0-9]+\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face)
     (3 font-lock-keyword-face)
     (4 font-lock-function-name-face))
    "DEFINITION" "ACCESSION" "DBLINK" "ORIGIN"
    "KEYWORDS" "SOURCE" "REFERENCE" "FEATURES"
    "COMMENT"
    ("^ +\\(ORGANISM\\)" . font-lock-doc-face)
    ("^ +\\(AUTHORS\\)"  . font-lock-doc-face)
    ("^ +\\(TITLE\\)"    . font-lock-doc-face)
    ("^ +\\(JOURNAL\\)"  . font-lock-doc-face)
    ("^ +\\(PUBMED\\)"   . font-lock-doc-face)

    ;; line numbers at the beginning of sequence...
    ("^[ \t]*\\([0-9]+\\)"
     (1 font-lock-string-face)))
  "Expressions to highlight in `epe-genbank-mode'.")


;;;###autoload
(define-derived-mode epe-genbank-mode text-mode "genbank"
  "Major mode for editing sequences in genbank format.

Special commands:
￼\\{epe-genbank-mode-map}
  \\{epe-nuc-mode-map}
  \\{epe-pro-mode-map}"
  ;; This runs the normal hook change-major-mode-hook, then gets rid of
  ;; the buffer-local variables of the major mode previously in effect.
  ;; (kill-all-local-variables)
  ;; (setq mode-name "genbank")
  ;; (setq major-mode 'epe-genbank-mode)
  ;; (use-local-map epe-genbank-mode-map)
  ;; The above are automatically done if the mode is defined using
  ;; `define-derived-mode'.
  ;; the variable automatically becomes buffer-local when set
  (setq font-lock-defaults '(epe-genbank-font-lock-keywords))
  (flyspell-mode -1)
  ;; (set-syntax-table genbank-mode-syntax-table)
  (run-hooks 'epe-genbank-mode-hook))


(defvar epe-genbank-record-regexp "^LOCUS[ \t]+"
  "Genbank records always start with \"LOCUS\".")


(defvar epe-genbank-record-end "^//[ \t]*"
  "Genbank records always end with \"//\".")


(defun epe-genbank-forward (count)
  "Move forward to the end genbank record.

It works in the style of `forward-paragraph'.  COUNT needs to be
positive integer.  Return current point if it moved over COUNT of
records; otherwise return nil."
  (interactive "p")
  (epe-entry-forward count epe-genbank-record-regexp))

(defun epe-genbank-backward (count)
  "Move the point the beginning of the genbank record.

It works in the style of `backward-paragraph'.  COUNT needs to be
positive integer.  Return current point if it moved over COUNT of
records; otherwise return nil."
  (interactive "p")
  (epe-entry-backward count epe-genbank-record-regexp))


(defun epe-genbank-first ()
  "Go to the beginning of first genbank record."
  (interactive)
  (epe-entry-first epe-genbank-record-regexp))


(defun epe-genbank-last ()
  "Go to the beginning of last genbank record."
  (interactive)
  (epe-entry-last epe-genbank-record-regexp))


(defun epe-genbank-count ()
  "Count the number of genbank records in the file."
  (interactive)
  (epe-entry-count epe-genbank-record-regexp))


(defun epe-genbank-mark (&optional whole)
  "Put point at the beginning of the sequence and mark the end.

If a prefix arg is provided or WHOLE is t, then put the point at
the beginning of the genbank entry instead of the sequence."
  (interactive "P")
  (if (epe-genbank-forward 1)
      (backward-char))
  (epe-entry-backward 1 epe-genbank-record-end)
  (forward-line)
  (push-mark nil nil t)
  (epe-genbank-backward 1)
  (or whole
      (progn (re-search-forward "^ORIGIN *$" nil)
             (forward-line))))


(defun epe-genbank-delete ()
  "Delete the current genbank record."
  (interactive)
  (epe-genbank-mark 'whole)
  (delete-region (region-beginning) (region-end)))


(defun epe-genbank--2-fasta ()
  "Convert current genbank record to fasta format."
  (let (str seq)
    (epe-genbank-mark)
    (setq str (buffer-substring-no-properties (region-beginning) (region-end)))
    (if epe-nuc-mode
        (setq seq (mapcan (lambda (i) (if (gethash i epe-nuc-alphabet-set) (list (upcase i)))) str))
      (setq seq (mapcan (lambda (i) (if (gethash i epe-pro-alphabet-set) (list (upcase i)))) str)))
    (epe-genbank-mark 'whole)
    (if (re-search-forward epe-genbank-record-regexp nil t)
        (replace-match ">" nil nil))
    (forward-line)
    (delete-region (region-beginning) (region-end))
    (insert (concat seq))
    (insert "\n")))


(defun epe-genbank-2-fasta ()
  "Convert current genbank record to fasta format."
  (interactive)
  (save-excursion
    (epe-genbank--2-fasta)))


(defun epe-genbank-2-fasta-all ()
  "Convert all genbank records to fasta format."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (while (epe-genbank-backward 1)
      (epe-genbank--2-fasta))))

(provide 'epe-genbank-mode)
;;; epe-genbank.el ends here
