
;; According to my browser's console, the reb_get.pl request is a POST
;; with form data:
;; searchkey: "BglII", x: "52" y: "13", searchcat: "enzyme+name+or+number"
(require 'dash)
(defun get-rebase-cutsite (enzyme)
  "Download the information about a restriction enzyme from rebase.neb.com."
  (interactive "sRestriction enzyme:")
  ;; use the url-library to send a POST with my desired enzyme.
  (let ((url "http://rebase.neb.com/cgi-bin/reb_get.pl")
        (url-request-method "POST")
        (url-request-extra-headers `(("Content-Type" . "application/x-www-form-urlencoded")))
        (url-request-data (format "searchkey=%s&x=52&y=13&searchcat=enzyme+name+or+number" enzyme))
        (result "")
        (dom "")
        (cut "")
        (seq "")
        (direction ""))
    ;; Send the result from NEB to a temporary buffer and dump the entire response to the
    ;; variable 'dom'.
    (with-temp-buffer
      (url-insert-file-contents url)
      (setq dom (libxml-parse-html-region (point-min) (point-max))))
    ;; Use the --> function from dash to create a pipe which first extracts all the TDs,
    ;; then pulls the 12th entry from the resulting list, gets the cddr and the text.
    ;; The resulting string goes to the variable 'cut'.
    (setq cut (--> dom
                  (dom-by-tag it 'td)
                  (nth 12 it)
                  cddr
                  dom-text))
    ;; Use the same logic to extract the string describing the cut direction from the img tag
    ;; which is inside the same TD element and use a regexp to drop the extraneous text.
    (setq direction (--> dom
                         (dom-by-tag it 'td)
                         (nth 12 it)
                         (dom-by-tag it 'img)
                         (dom-attr it 'src)
                         (replace-regexp-in-string "^\/rebase\/\\(.+\\)\.gif" "\\1" it)
                         ))
    ;; Remove the space from the cut string to get the recognition sequence.
    (setq seq (replace-regexp-in-string " " "" cut))
    ;; and return a 3 element list containing the recognition sequence, the cut, and direction.
    (list seq cut direction)
    ))

(defun read-perl-enzymes ()
  "Read the data from the perl library Bio::Restriction to extract some REs."
  (let (
        ;; Use perldoc to find the file containing the restriction enzyme definitions.
        (re-db (string-trim-right (shell-command-to-string "perldoc -lm Bio::Restriction::IO::base")))
        ;; make empty hashes containing the enzymes and their cut locations.
        (base-enzymes (make-hash-table :test 'equal))
        (base-cuts (make-hash-table :test 'equal))
        (enzyme-name "")
        (enzyme-site "")
        (enzyme-cut "")
        (current-line "")
        (start-reading nil))
    (with-temp-buffer
      (insert-file-contents-literally re-db)
      ;; Read the temporary buffer until it ends.
      (while (not (eobp))
        (progn
          ;; Read in the current line, note that looking-at-p does not do a string-trim
          ;; With that in mind, the following iterates over every line and waits until
          ;; it finds the __DATA__ section.
          (setq current-line (string-trim-right (thing-at-point 'line t)))
          (forward-line 1)
          (if (looking-at-p "__DATA__\n")
              (setq start-reading t))
          (if start-reading
              ;; Once we get to the __DATA__, use a simple regex to get the name and values of each
              ;; enzyme and send them to my two hashes.
              (progn
                (save-match-data
                  (progn (string-match "^\\([[:alnum:]]+\\)[[:space:]]+\\([[:alnum:]]+\\)[[:space:]]+\\([[:digit:]]+\\)$" current-line)
                         (setq enzyme-name (match-string 1 current-line))
                         (setq enzyme-site (match-string 2 current-line))
                         (setq enzyme-cut (match-string 3 current-line))
                         (puthash enzyme-name enzyme-site base-enzymes)
                         (puthash enzyme-name enzyme-cut base-cuts)))))
          )))
    ;; When finished reading the file, return a list containing the
    ;; enzyme identities and cut locations.
    (list base-enzymes base-cuts)))

(provide 'epe-restriction)
;;; epe-restriction.el ends here.
