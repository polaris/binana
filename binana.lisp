;;;; binana.lisp

(in-package #:binana)

(defun read-32-bit-word (input start)
  (logior (ash (aref input (+ start 3)) 24)
          (ash (aref input (+ start 2)) 16)
          (ash (aref input (+ start 1)) 8)
          (aref input start)))

(defun read-mach-header (stream)
  (let ((bytes (make-array 32 :initial-element 0)))
    (read-sequence bytes stream :start 0 :end 32)
    (let ((magic (read-32-bit-word bytes 0))
          (cputype (read-32-bit-word bytes 4))
          (cpusubtype (read-32-bit-word bytes 8))
          (filetype (read-32-bit-word bytes 12)))
      (values magic cputype cpusubtype filetype))))

(defun print-mach-header (filename)
  (with-open-file (in filename :element-type '(unsigned-byte 8))
    (multiple-value-bind (magic cputype cpusubtype filetype) (binana::read-mach-header in)
      (format t "magic      0x~d~%" (write-to-string magic :base 16))
      (format t "cputype    ~d~%" cputype)
      (format t "cpusubtype ~d~%" cpusubtype)
      (format t "filetype   ~d~%" filetype))))
