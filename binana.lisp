;;;; binana.lisp

(in-package #:binana)

;    (let ((bytes (make-array 4 :initial-element 0)))
 ;     (read-sequence bytes stream :start start :end (+ start 4))


(defun read-32-bit-word (input start)
  (logior (ash (aref input (+ start 3)) 24)
          (ash (aref input (+ start 2)) 16)
          (ash (aref input (+ start 1)) 8)
          (aref input start)))

