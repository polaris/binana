;;;; binana.lisp

(in-package #:binana)

(defconstant MH_OBJECT       1) ; relocatable object file
(defconstant MH_EXECUTE      2) ; demand paged executable file
(defconstant MH_FVMLIB       3) ; fixed VM shared library file
(defconstant MH_CORE         4) ; core file
(defconstant MH_PRELOAD      5) ; preloaded executable file
(defconstant MH_DYLIB	     6) ; dynamically bound shared library
(defconstant MH_DYLINKER     7) ; dynamic link editor
(defconstant MH_BUNDLE	     8) ; dynamically bound bundle file
(defconstant MH_DYLIB_STUB   9) ; shared library stub for static linking only, no section contents
(defconstant MH_DSYM        10) ; companion file with only debug sections
(defconstant MH_KEXT_BUNDLE 11) ; x86_64 kexts

(defun map-to-filetype (value)
  (cond ((= value MH_OBJECT) "Relocatable object file")
	((= value MH_EXECUTE) "Executable file")
	((= value MH_FVMLIB) "Fixed VM shared library file")
	((= value MH_CORE) "Core file")
	((= value MH_PRELOAD) "Preload executable file")
	((= value MH_DYLIB) "Dynamically bound shared file")
	((= value MH_DYLINKER) "Dynamic link editor")
	((= value MH_BUNDLE) "Dynamically bound bundle file")
	((= value MH_DYLIB_STUB) "Shared library stub for static linking")
	((= value MH_DSYM) "Companinon file with only debug sections")
	((= value MH_KEXT_BUNDLE) "x86-64 Kexts")
	(t "Other")))

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
          (filetype (read-32-bit-word bytes 12))
	  (ncmds (read-32-bit-word bytes 16))
	  (sizeofcmds (read-32-bit-word bytes 20))
	  (flags (read-32-bit-word bytes 24)))
      (values magic cputype cpusubtype filetype ncmds sizeofcmds flags))))

(defun print-mach-header (filename)
  (with-open-file (in filename :element-type '(unsigned-byte 8))
    (multiple-value-bind (magic cputype cpusubtype filetype ncmds sizeofcmds flags) (binana::read-mach-header in)
      (format t "magic      0x~d~%" (write-to-string magic :base 16))
      (format t "cputype    ~d~%" cputype)
      (format t "cpusubtype ~d~%" cpusubtype)
      (format t "filetype   ~d~%" (map-to-filetype filetype))
      (format t "ncmds      ~d~%" ncmds)
      (format t "sizeofcmds ~d~%" sizeofcmds)
      (format t "flags      0x~d~%" (write-to-string flags :base 16)))))
