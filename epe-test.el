;;; epe-test.el --- Unit test  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Ashton Trey Belew

;; Author: Aaron S. Hawley <aaron.s.hawley@gmail.com> and Ashton Trey Belew <abelew@gmail.com>
;; Keywords: data

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Testing of epe.el

;;; Code:

(require 'ert)

(ert-deftest epe-make-fasta-ATGATGATG ()
  "Test \\[epe-make-fasta] for ATGATGATG."
  (epe-make-fasta "ATGATGATG" "fubar")
  (epe-make-fasta "A^GATCT" "BglII")
  (switch-to-buffer "fubar.fasta")
  (goto-char (point-min))
  (forward-line 1)
  (unwind-protect
      (should
       (string-equal "ATGATGATG"
                     (buffer-substring-no-properties
                      (point) (line-end-position 1))))
    (kill-buffer "fubar.fasta")))

(ert-deftest epe-split-sequence-ATGATGATG ()
  "Test \\[epe-split-sequence] for ATGATGATG."
  (should
   (equal (epe-split-sequence "ATGATGATG" 3)
          (list "ATG" "ATG" "ATG"))))

(ert-deftest epe-make-revcomp-fasta-A^GATCT ()
  "Test \\[epe-make-revcomp-fasta] for A^GATCT."
  (epe-make-fasta "A^GATCT" "BglII")
  (switch-to-buffer "BglII.fasta")
  (should
   (string-equal ">BglII-reverse-complemented\nAGATC^T"
                 (epe-make-revcomp-fasta))))

(provide 'epe-test)
;;; epe-test.el ends here
