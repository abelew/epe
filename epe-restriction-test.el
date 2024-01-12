;;; epe-restriction-test.el --- Unit test  -*- lexical-binding: t; -*-

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

;; Testing of epe-restriction.el

;;; Code:

(require 'ert)

(ert-deftest get-rebase-cutsite-BglII ()
  "Test \\[get-rebase-cutsite] for BglII."
  (should
   (equal (get-rebase-cutsite "BglII")
          (list "AGATCT" "A GATCT" "cutdown3"))))

(provide 'epe-restriction-test)
;;; epe-restriction-test.el ends here
