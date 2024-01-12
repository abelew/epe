;;; epe.el --- An emacs plasmid editor.
;;
;; Copyright (c) 2023 Ashton Trey Belew
;;
;; Author: Ashton Trey Belew <abelew@gmail.com> and Aaron S. Hawley <aaron.s.hawley@gmail.com>
;; Homepage: nil
;; URL: nil
;; Version: 202312
;; Last-Updated: 202312
;; Keywords: Biology
;;
;; GPLv3
;;
;;; Commentary:
;;
;; Thank you Aaron for the changes for byte compilation!
;;
;; Quickstart
;;
;;     (require 'epe)
;;
;;     (epe-mode 1)
;;
;; Explanation
;;
;; I think ApE is awesome.  Ever since I first used it I wished I had
;; it in emacs.  This is my attempt to implement bits and pieces of it
;; to make my life easier when messing with sequences in emacs.
;;
;; In its current state, the authors of simpleclip.el (Roland Walker)
;; and ApE (M. Wayne Davis) should probably be considered authors of
;; this, given that everything I am writing is taken from some
;; combination of their work:
;;
;; https://github.com/rolandwalker/simpleclip
;; https://www.frontiersin.org/articles/10.3389/fbinf.2022.818619/full
;;
;; While poking around to find suggestions for better fasta file formatting,
;; I stumbled into seqel, which does a lot of what I want:
;; https://github.com/rnaer/seqel
;; as well as fasta-mode.el:
;; https://github.com/vaiteaopuu/emacs-fasta-mode


;;; Code:
;;

;;; customizable variables

(require 'dom)
(eval-when-compile
  (require 'subr-x)) ; string-join, string-empty-p

;;;###autoload
(defgroup epe nil
  "Emacs plasmid editor."
  :version "202312"
  :link '(emacs-commentary-link :tag "Commentary" "epe")
  :link '(url-link :tag "GitHub" "http://github.com/abelew/epe")
  :prefix "epe-"
  :group 'convenience)


(defcustom epe-less-feedback nil
  "Give less echo area feedback."
  :type 'boolean
  :group 'epe)


;;; variables


(defvar epe-mode nil
  "Mode variable for `epe-mode'.")


;;; keymaps


(defvar epe-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "C-S" #'epe-search)
    map)
  "Keymap for `epe-mode' minor-mode.")


;;; utility functions


;;;###autoload
(defun epe-make-fasta (subseq name)
  "Given a subsequence, make a buffer containing it as a fasta file.

  Example:
 (epe-make-fasta \"ATGATGATG\" \"bob\")
  Result: A buffer named bob.fasta containing the header and sequence split to 80 characters/line.

  TODO: make the 80 into a defcustom"
  (let* ((abuf (get-buffer-create (format "%s.fasta" name)))
         (split-subseq (epe-split-sequence subseq 80))
         (fasta-header (format ">%s\n" name))
         (merged-subseq (string-join split-subseq "\n")))
    (with-current-buffer abuf
      (insert fasta-header)
      (insert merged-subseq)
      (insert "\n"))))


;; Taken pretty much verbatim from:
;; https://emacs.stackexchange.com/questions/36200/split-line-every-n-characters
;; It makes me very happy that the person who posted this wrote a recursive cons,
;; though this may be a bad idea for larger fasta sequences.
(defun epe-split-sequence (subseq chars)
  "Split SUBSEQ into a list of substrings of length CHARS characters.

  Example:
 (epe-split-sequence \"ATGATGATG\" 3)
  Result:
 '(\"ATG\" \"ATG\" \"ATG\")

  TODO: make CHARS into a defcustom."
  (cond ((string-empty-p subseq)
         nil)
        ((< (length subseq) chars)
         (list subseq))
        (t (cons (substring subseq 0 chars)
                 (epe-split-sequence (substring subseq chars) chars)))))


;;; minor-mode definition


;;;###autoload
(define-minor-mode epe-mode
  "Turn on `epe-mode'."
  :group 'epe
  :global t)


;;; interactive commands


;; Taken directly from:
;; https://github.com/vaiteaopuu/emacs-fasta-mode/blob/master/fasta-mode.el
;; which I stumbled into while hunting for a selector of fasta sequences.
;;;###autoload
(defun epe-get-fasta ()
  "Yank the current fasta header and sequence into the kill-ring.

  Example:
 > C-x C-f some fasta file, navigate to an arbitrary entry.
 > M-x epe-get-fasta
 > C-x b to another buffer/file.
 > C-y
  Result: The entire fasta entry.
"
  (interactive)
  (let ((start-seq (save-excursion (forward-char) (re-search-backward ">" nil 't)))
        (end-seq (save-excursion (forward-char) (re-search-forward ">" nil 't)))
        (selected-seq ""))
    (if (and start-seq end-seq)
        (progn
          (message (format "%S to %S" start-seq (- end-seq 100)))
          (setq selected-seq (buffer-substring-no-properties start-seq (- end-seq 1))))
      (progn
        (when start-seq
          (message (format "last sequence"))
          (setq selected-seq (buffer-substring-no-properties start-seq (point-max))))
        (when end-seq
          (message (format "first sequence"))
          (setq selected-seq (buffer-substring-no-properties (point-min) end-seq)))))
    (kill-new selected-seq)
    selected-seq))


;; This adds a little logic to epe-get-fasta in order to remove the header.
;;;###autoload
(defun epe-get-fasta-seq ()
  "Extract _only_ the sequence of the current fasta entry to the kill ring.

  Example:
 > C-x C-f some fasta file, navigate to an arbitrary entry.
 > M-x epe-get-fasta-seq
 > C-x b to another buffer/file.
 > C-y
  Result: The fasta sequence as a single line."
  (interactive)
  (let* ((initial (epe-get-fasta))
         (without-header (replace-regexp-in-string "^>.*" "" initial))
         (without-newlines (replace-regexp-in-string "\n" "" without-header)))
    (kill-new without-newlines)
    without-newlines))


;;;###autoload
(defun epe-make-revcomp-fasta ()
  "Reverse complement the current nucleotide entry and send it to the kill ring.

  Example:
 > C-x C-f some fasta file, navigate to an arbitrary entry.
 > M-x epe-make-revcomp-fasta
 > C-x b to another buffer/file.
 > C-y
  Result: The reverse complemented nucleotide sequence."
  (interactive)
  (let*
      ;; One might reasonably wonder what is going on here.
      ;; When using a translation-table, if one defines a->b and b->a
      ;; Then all a's and b's will be translated to a.
      ;; Thus I first send AUGCaugc to a numeric value and then translate
      ;; back to the complement sequence.
      ;; My assumption is that this wasteful process is still likely quite fast
      ;; and the closest thing I could find to Perl's tr operator.
      ((to-numeric '((?a . ?1) (?A . ?2)
                     (?g . ?3) (?G . ?4)
                     (?u . ?5) (?U . ?6)
                     (?t . ?7) (?T . ?8)
                     (?c . ?9) (?C . ?0)))
       (from-numeric '((?1 . ?t) (?2 . ?T)
                       (?3 . ?c) (?4 . ?C)
                       (?5 . ?a) (?6 . ?A)
                       (?7 . ?a) (?8 . ?A)
                       (?9 . ?g) (?0 . ?G)))
       (initial (epe-get-fasta))
       ;; In theory I am pretty confident with regular expressions, but getting a properly
       ;; not-greedy match to the fasta header line almost caused me to pull out my hair.
       ;; The following, gross, two-step regex is the result.
       ;; I am 100% certain there is a vastly better way to do this.
       (fasta-header (replace-regexp-in-string
                      "\n" ""
                      (replace-regexp-in-string "^>\\(.+\\)\\(\n.+\\)+" "\\1" initial)))
       (new-header (format ">%s-reverse-complemented" fasta-header))
       (fasta-seq (epe-get-fasta-seq))
       (merged "")
       (translated ""))
    (define-translation-table 'epe-numeric to-numeric)
    (define-translation-table 'epe-complement from-numeric)
    (with-temp-buffer
      (insert (reverse fasta-seq))
      (translate-region (point-min) (point-max) 'epe-numeric)
      (translate-region (point-min) (point-max) 'epe-complement)
      (setq translated (buffer-substring-no-properties (point-min) (point-max))))
    (setq translated
          (string-join
           (cons new-header (epe-split-sequence translated 80)) "\n"))
    (kill-new translated)
    translated))


(defun epe-revcomp-entire ()
  "Not yet implemented: reverse complement every sequence in a file."
  )


(provide 'epe)


;;
;; Emacs
;;
;; Local Variables:
;; indent-tabs-mode: nil
;; require-final-newline: t
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions redefine)
;; End:
;;
;; LocalWords: epe ARGS alist devel callf
;; LocalWords: epe revcomp
;;

;;; epe.el EOF
