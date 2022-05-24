(defparameter chromatic-scale '("C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B"))
(defparameter major-scale-ix '(0 2 4 5 7 9 11))
(defparameter solfege '("Do" "Re" "Mi" "Fa" "So" "La" "Ti"))

(defun note (major-scale-root solfege-note)
  (let* ((solfege-ix (position solfege-note solfege :test #'equal))
	 (major-scale-pos (nth solfege-ix major-scale-ix))
	 (major-scale-root-ix (position major-scale-root chromatic-scale :test #'equal))
	 (note-ix
	   (mod (+ major-scale-root-ix major-scale-pos) (length chromatic-scale)))
    )
    (nth note-ix chromatic-scale)))
