(load-library "eieio")

(defvar nucleotides '() "The eieio instance into which the nucleotides will be stored.")
(setq nucleotides '()) ;; ??

(defclass nucleotide (eieio-instance-tracker)
  ((tracking-symbol :initform nucleotides
                    :documentation "Variable that holds all class instances.")
   (name :initarg :name
         :documentation "The name of the Nucleotide")
   (synonyms :initarg :synonyms :initform '()
             :documentation "List of regular expressions that match the element.")
   (face :initarg :face :initform 'font-lock-type-face
         :documentation "The face to use with font-lock."))
  "Base class for a nucleotide.")

(defclass purine (nucleotide)
  ((face :initform '(:background "darkblue" :underline nil)))
  "Purines have two rings!  Adenine and Guanine.")

(defclass pyrimidine (nucleotide)
  ((face :initform '(:background "darkgreen" :underline nil)))
  "Pyrimidines have one ring, Thymine/Uracil and Cytosine.")

(defclass strong (nucleotide)
  ((face :initform '(:foreground "white" :underline nil)))
  "The strong nucleotides make 3 hydrogen bonds: Guanine and Cytosine.")

(defclass weak (nucleotide)
  ((face :initform '(:foreground "red" :underline nil)))
  "The weak nucleotides make 2 hydrogen bonds: Adenine and Thymine.")

(defclass adenine (purine weak)
  ((face :initform '(:foreground "#ff00ff" :background "darkblue" :underline nil)))
  "")

(defclass thymine (pyrimidine weak)
  ((face :initform '(:foreground "red" :background "darkgreen" :underline nil)))
  "")

(defclass guanine (purine strong)
  ((face :initform '(:foreground "white" :background "darkblue" :underline nil)))
  "")

(defclass cytosine (pyrimidine strong)
  ((face :initform '(:foreground "#ff00ff" :background "darkgreen" :underline nil)))
  "")

;; I am going to put the DNA masses for nucleotides in a ssDNA chain; so if one wants the weight
;; of something like a normal PCR primer; then take the sum of these and add the PO4 (79)
;; Similarly, if one wants a ssRNA, add 16 for ACG, add 2 for U, and 159 for the 5' triphosphate.
(defvar nucleotide-masses
  '(("Adenine" . 313.2)
    ("Thymine" . 304.2)
    ("Cytosine" . 305.2)
    ("Guanine" . 345.2)
    ("Uracil" . 324.2)
    ("Inosine" . 300.0) ; I don't actually know the weight of inosine, look that up!
    )
  "a-list of daltons per nucleotide.")


(defvar nucleotide-names
  '(("A" . "adenine")
    ("T" . "threonine")
    ("C" . "cytosine")
    ("G" . "guanine")
    ("I" . "inosine")
    ("W" . "A|T|U")
    ("S" . "G|C")
    ("R" . "A|G")
    ("Y" . "C|T|U")
    ("B" . "!A")
    ("D" . "!C")
    ("H" . "!G")
    ("V" . "!T")
    ("N" . "unknown")
    )
  "a-list of abbreviations to full names.")


(cl-defmethod get-nucleotide-mass ((x nucleotide))
  "Return atomic mass from `nucleotide-masses'."
  (let ((the-name (oref x :name)))
    (format "The name is: %s " the-name)
    (cdr (assoc (oref x :name) nucleotide-masses))))


(cl-defmethod nucleotide-help-echo ((x nucleotide))
  "A tooltip for the nucleotide.
It will look like class (inherited classes) mass=atomic-mass"
  (format "%s is a %s %s with mass=%s"
          (oref x :name)
          (eieio-class-name (cadr (eieio-class-parents (eieio-object-class x))))
          (eieio-class-name (car (eieio-class-parents (eieio-object-class x))))
          (or (get-nucleotide-mass x) "unknown")))

(cl-defmethod nucleotide-search ((x nucleotide))
  "Search google for the nucleotide."
  (google-this-string nil (oref x :name) t))


(cl-defmethod nucleotide-font-lock-rule ((x nucleotide))
  "Return font-lock rule for the nucleotide."
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1]
      (lambda ()
        "Construct the object and run `nucleotide-search' on it."
        (interactive)
        (nucleotide-search
         (eieio-instance-tracker-find
          (get-text-property (point) 'nucleotide-name)
          :name 'nucleotides))))
    (list
     ;; Construct the pattern to match
     ;;(rx-to-string `(: bow
     ;;                  (or  ,(oref x :name)
     ;;                       ,@(cl-loop for sy in (oref x :synonyms)
     ;;                               collect `(regexp ,sy)))
     ;;                  eow))
     (rx-to-string `(:
                     (or  ,(oref x :name)
                          ,@(cl-loop for sy in (oref x :synonyms)
                                     collect `(regexp ,sy)))))
     0  ;; font-lock the whole match
     ;; These are the properties to put on the matches
     `(quote (face ,(oref x :face)
                   nucleotide-name ,(oref x :name)
                   local-map ,map
                   mouse-face 'highlight
                   help-echo ,(nucleotide-help-echo x))))))

(adenine :name "Adenine" :synonyms '("[aA]" "[aA]denine"))
(thymine :name "Thymine" :synonyms '("[tT]" "[tT]hymine"))
(cytosine :name "Cytosine" :synonyms '("[cC]" "[cC]ytosine"))
(guanine :name "Guanine" :synonyms '("[gG]" "[gG]uanine"))

;(purine :name "Adenine" :synonyms '("A"))
;(purine :name "Guanine" :synonyms '("G"))
;(pyrimidine :name "Thymine" :synonyms '("T"))
;(pyrimidine :name "Cytosine" :synonyms '("C"))
;(weak :name "Adenine")
;(weak :name "Thymine")
;(strong :name "Guanine")
;(strong :name "Cytosine")

(font-lock-add-keywords
 nil
 (mapcar 'nucleotide-font-lock-rule nucleotides))

(font-lock-fontify-buffer)
