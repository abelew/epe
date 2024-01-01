;;; epe-faces.el --- Text colors which work in the terminal.

;;; Commentary:

;; Ideally I would just use seqel for this, it is better and more
;; thought-out than anything I am likely to write.  Unfortunately, I
;; work almost exclusively within the confines of a screen session so
;; that I may most easily share my work between arbitrary computers.

;; On my computer, the face for 'T' is very difficult to read.
;; For my taste, I would like to have the option of colors reminiscent
;; from what I remember in clustalx: G: orange, A: red, T: green, C:
;; blue; with the caveat that these colors (T: magenta, C: cyan,
;; A: white, G: pink) are better for color blind people and should at
;; least be kept as an option.

;;; Code:

(require 'color)

;; For the moment, I am taking this directly from seqel until I can
;; see how well the various combinations work in my screen session.
(defvar epe-color-pairs
  '(("#ffffff" "#000000")  ; white    on black
    ("#ff0000" "#000000")  ; red
    ("#00ff00" "#000000")  ; green
    ("#00ffff" "#000000")  ; cyan
    ("#ff00ff" "#000000")  ; magenta
    ("#ffff00" "#000000")  ; yellow
    ("#ff6600" "#000000")  ; orange
    ("#0066ff" "#000000")  ; ~blue
    ("#000000" "#00ff00")  ; on green
    ("#ff0000" "#00ff00")
    ("#ffff00" "#00ff00")
    ("#000000" "#00aaff")  ; on ~blue
    ("#ffffff" "#00aaff")
    ("#00ff00" "#00aaff")
    ("#000000" "#ff0000")  ; on red
    ("#00ff00" "#ff0000")
    ("#ffffff" "#ff0000")
    ("#000000" "#ff00ff")  ; on magenta
    ("#ffffff" "#ff00ff")
    ("#000000" "#ffff00")  ; on yellow
    ("#ff0000" "#ffff00")
    ("#000000" "#00ffff")  ; on cyan
    ("#ff0000" "#00ffff")
    ("#000000" "#ffffff")  ; black    on white
    ("#ff0000" "#ffffff")  ; red
    ("#0000ff" "#ffffff")  ; blue
    ("#ff00ff" "#ffffff")  ; magenta
    ("#00ffff" "#ffffff")  ; cyan
    )
  "Color pairs that pass WCAG AAA test.

The first one is the text color and the second is the background.")
