(load-library "eieio")

(defvar molecules '() "The eieio instance into which the molecules will be stored.")
(setq molecules '()) ;; ??

(defclass molecule (eieio-instance-tracker)
  ((tracking-symbol :initform molecules
                    :documentation "Variable that holds all class instances.")
   (name :initarg :name
         :documentation "The name of the Molecule")
   (synonyms :initarg :synonyms :initform '()
             :documentation "List of regular expressions that match the element.")
   (face :initarg :face :initform 'font-lock-type-face
         :documentation "The face to use with font-lock."))
  "Base class for a molecule.")

(defclass nucleotide (molecule)
  ()
  "They end in ine.")


(defclass purine (molecule nucleotide)
  ((face :initform '(:background "darkblue" :underline nil)))
  "Purines have two rings!  Adenine and Guanine.")

(defclass pyrimidine (molecule nucleotide)
  ((face :initform '(:background "darkgreen" :underline nil)))
  "Pyrimidines have one ring, Thymine/Uracil and Cytosine.")

(defclass strong (molecule nucleotide)
  ((face :initform '(:foreground "white" :underline nil)))
  "The strong molecules make 3 hydrogen bonds: Guanine and Cytosine.")

(defclass weak (molecule nucleotide)
  ((face :initform '(:foreground "yellow" :underline nil)))
  "The weak molecules make 2 hydrogen bonds: Adenine and Thymine.")

(defclass adenine (molecule nucleotide purine weak)
  "")

(defclass thymine (molecule nucleotide pyrimidine weak)
  "")

(defclass guanine (molecule nucleotide purine strong)
  "")

(defclass cytosine (molecule nucleotide purine strong)
  "")

;; I am going to put the DNA masses for nucleotides in a ssDNA chain; so if one wants the weight
;; of something like a normal PCR primer; then take the sum of these and add the PO4 (79)
;; Similarly, if one wants a ssRNA, add 16 for ACG, add 2 for U, and 159 for the 5' triphosphate.
(defvar molecule-masses
  '(("Adenine" . 313.2)
    ("Thymine" . 304.2)
    ("Cytosine" . 305.2)
    ("Guanine" . 345.2)
    ("Uracil" . 324.2)
    ("Inosine" . 300.0) ; I don't actually know the weight of inosine, look that up!
    ("Asp" . 133.11) ;; Aspartic acid: D, acidic with 2 likely modifications.
    ("Glu" . 147.13) ;; Glutamic acid: E, acidic with 1 likely modification.
    ("Gly" . 75.07) ;; Glycine: G, polar.
    ("Phe" . 165.19) ;; Phenylalanine: F, Nonpolar.
    ("Leu" . 131.18) ;; Leucine: L, Nonpolar.
    ("Ser" . 105.09) ;; Serine: S, Polar with 2 modifications.
    ("Tyr" . 181.19) ;; Tyrosine: Y, Polar with 2 modifications.
    ("Cys" . 121.16) ;; Cysteine: C, Polar with a sulfur!
    ("Trp" . 204.23) ;; Tryptophan: W, Nonpolar and the biggest.
    ("Pro" . 115.13) ;; Proline: P, Nonpolar with one modification and a backbone ring.
    ("His" . 155.16) ;; Histidine: H, Basic with 2 modifications and a funky ring.
    ("Gln" . 146.15) ;; Glutamine: Q, Polar with 1 modification.
    ("Arg" . 174.20) ;; Arginine: R, Basic with 2 modifications a the longest chain.
    ("Ile" . 131.18) ;; Isoleucine: I, Nonpolar and leucine's evil twin.
    ("Thr" . 119.12) ;; Threonine: T, Polar and boring with 2 modifications.
    ("Asn" . 132.12) ;; Asparagine: N, Polar with 1 modification.
    ("Lys" . 146.19) ;; Lysine: K: Basic, long chain, 4 modifications, hates leucine for stealing L
    ("Val" . 117.15) ;; Valine: V: Nonpolar and boring.
    ("Ala" . 89.09) ;; Alanine: A: Nonpolar and the 2nd smallest after glycine.
    )
  "a-list of daltons per molecule.")


(defvar molecular-names
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
    ("Asp" . "aspartic acid")
    ("Glu" . "glutamic acid")
    ("Gly" . "glycine")
    ("Phe" . "phenylalanine")
    ("Leu" . "leucine")
    ("Ser" . "serine")
    ("Tyr" . "tyrosine")
    ("Cys" . "cysteine")
    ("Trp" . "tryptophan")
    ("Pro" . "proline")
    ("His" . "histidine")
    ("Gln" . "glutamine")
    ("Arg" . "arginine")
    ("Ile" . "isoleucine")
    ("Thr" . "threonine")
    ("Asn" . "asparagine")
    ("Lys" . "lysine")
    ("Val" . "valine")
    ("Ala" . "alanine")
    )
  "a-list of abbreviations to full names.")


(defvar molecule-abbreviations
  '(("Asp" . "D")
    ("Glu" . "E")
    ("Gly" . "G")
    ("Phe" . "F")
    ("Leu" . "L")
    ("Ser" . "S")
    ("Tyr" . "Y")
    ("Cys" . "C")
    ("Trp" . "W")
    ("Pro" . "P")
    ("His" . "H")
    ("Gln" . "Q")
    ("Arg" . "R")
    ("Ile" . "I")
    ("Thr" . "T")
    ("Asn" . "N")
    ("Lys" . "K")
    ("Val" . "V")
    ("Ala" . "A")
    )
  "a-list of daltons per molecule.")


(cl-defmethod get-molecule-mass ((x molecule))
  "Return atomic mass from `molecular-masses'."
  (let ((the-name (oref x :name)))
    (format "The name is: %s " the-name)
    (cdr (assoc (oref x :name) molecule-masses))))

(cl-defmethod molecule-help-echo ((x molecule))
  "A tooltip for the molecule.
It will look like class (inherited classes) mass=atomic-mass"
  (format "%s %s: mass=%s"
          (eieio-object-class x)
          (mapcar 'eieio-class-name (eieio-class-parents (eieio-object-class x)))
          (or (get-molecule-mass x) "unknown")))

(cl-defmethod molecule-search ((x molecule))
  "Search google for the molecule."
  (google-this-string nil (oref x :name) t))


(cl-defmethod molecule-font-lock-rule ((x molecule))
  "Return font-lock rule for the molecule."
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1]
      (lambda ()
        "Construct the object and run `molecule-search' on it."
        (interactive)
        (molecule-search
         (eieio-instance-tracker-find
          (get-text-property (point) 'molecule-name)
          :name 'molecules))))
    (list
     ;; Construct the pattern to match
     (rx-to-string `(: bow
                       (or  ,(oref x :name)
                            ,@(cl-loop for sy in (oref x :synonyms)
                                    collect `(regexp ,sy)))
                       eow))
     0  ;; font-lock the whole match
     ;; These are the properties to put on the matches
     `(quote (face ,(oref x :face)
                   molecule-name ,(oref x :name)
                   local-map ,map
                   mouse-face 'highlight
                   help-echo ,(molecule-help-echo x))))))

(adenine :name "Adenine" :synonyms '("[aA]" "[aA]denine"))
(thymine :name "Thymine" :synonyms '("[tT]" "[tT]hymine"))
(cytosine :name "Cytosine" :synonyms '("[cC]" "[cC]ytosine"))
(guanine :name "Guanine" :synonyms '("[gG]" "[gG]uanine"))

(purine :name "Adenine" :synonyms '("A"))
(purine :name "Guanine" :synonyms '("G"))
(pyrimidine :name "Thymine" :synonyms '("T"))
(pyrimidine :name "Cytosine" :synonyms '("C"))
(weak :name "Adenine")
(weak :name "Thymine")
(strong :name "Guanine")
(strong :name "Cytosine")

(font-lock-add-keywords
 nil
 (mapcar 'molecule-font-lock-rule molecules))

(font-lock-fontify-buffer)
