;;;; utils.lisp

(in-package #:binana)

(defun read-string (input start size)
  (concatenate 'string (map 'cons 'code-char (subseq input start (+ start size)))))

(defun read-32-bit-word (input start)
  (logior (ash (aref input (+ start 3)) 24)
          (ash (aref input (+ start 2)) 16)
          (ash (aref input (+ start 1)) 8)
          (aref input start)))

(defun read-64-bit-word (input start)
  (logior (ash (aref input (+ start 7)) 56)
	  (ash (aref input (+ start 6)) 48)
	  (ash (aref input (+ start 5)) 40)
	  (ash (aref input (+ start 4)) 32)
	  (ash (aref input (+ start 3)) 24)
          (ash (aref input (+ start 2)) 16)
          (ash (aref input (+ start 1)) 8)
          (aref input start)))

