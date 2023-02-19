;;;; binana.lisp

(in-package #:binana)

(defconstant MH_OBJECT      #x1) ; relocatable object file
(defconstant MH_EXECUTE     #x2) ; demand paged executable file
(defconstant MH_FVMLIB      #x3) ; fixed VM shared library file
(defconstant MH_CORE        #x4) ; core file
(defconstant MH_PRELOAD     #x5) ; preloaded executable file
(defconstant MH_DYLIB       #x6) ; dynamically bound shared library
(defconstant MH_DYLINKER    #x7) ; dynamic link editor
(defconstant MH_BUNDLE	    #x8) ; dynamically bound bundle file
(defconstant MH_DYLIB_STUB  #x9) ; shared library stub for static linking only, no section contents
(defconstant MH_DSYM        #xa) ; companion file with only debug sections
(defconstant MH_KEXT_BUNDLE #xb) ; x86_64 kexts

; After MacOS X 10.1 when a new load command is added that is required to be
; understood by the dynamic linker for the image to execute properly the
; LC_REQ_DYLD bit will be or'ed into the load command constant.  If the dynamic
; linker sees such a load command it it does not understand will issue a
; "unknown load command required for execution" error and refuse to use the
; image.  Other load commands without this bit that are not understood will
; simply be ignored.
(defconstant LC_REQ_DYLD #x80000000)

(defconstant LC_SEGMENT	          #x1) ; segment of this file to be mapped
(defconstant LC_SYMTAB	          #x2) ; link-edit stab symbol table info
(defconstant LC_SYMSEG	          #x3) ; link-edit gdb symbol table info (obsolete)
(defconstant LC_THREAD	          #x4) ; thread
(defconstant LC_UNIXTHREAD        #x5) ; unix thread (includes a stack)
(defconstant LC_LOADFVMLIB        #x6) ; load a specified fixed VM shared library
(defconstant LC_IDFVMLIB          #x7) ; fixed VM shared library identification
(defconstant LC_IDENT             #x8) ; object identification info (obsolete)
(defconstant LC_FVMFILE	          #x9) ; fixed VM file inclusion (internal use)
(defconstant LC_PREPAGE           #xa) ; prepage command (internal use)
(defconstant LC_DYSYMTAB          #xb) ; dynamic link-edit symbol table info
(defconstant LC_LOAD_DYLIB        #xc) ; load a dynamically linked shared library
(defconstant LC_ID_DYLIB          #xd) ; dynamically linked shared lib ident
(defconstant LC_LOAD_DYLINKER     #xe) ; load a dynamic linker
(defconstant LC_ID_DYLINKER       #xf) ; dynamic linker identification
(defconstant LC_PREBOUND_DYLIB    #x10) ; modules prebound for a dynamically linked shared library
(defconstant LC_ROUTINES          #x11) ; image routines
(defconstant LC_SUB_FRAMEWORK     #x12) ; sub framework
(defconstant LC_SUB_UMBRELLA      #x13) ; sub umbrella
(defconstant LC_SUB_CLIENT        #x14) ; sub client
(defconstant LC_SUB_LIBRARY       #x15) ; sub library
(defconstant LC_TWOLEVEL_HINTS    #x16) ; two-level namespace lookup hints
(defconstant LC_PREBIND_CKSUM     #x17) ; prebind checksum

; load a dynamically linked shared library that is allowed to be missing
; (all symbols are weak imported).
(defconstant LC_LOAD_WEAK_DYLIB       (logior #x18 LC_REQ_DYLD))

(defconstant LC_SEGMENT_64            #x19) ; 64-bit segment of this file to be mapped
(defconstant LC_ROUTINES_64           #x1a) ; 64-bit image routines
(defconstant LC_UUID                  #x1b) ; the uuid
(defconstant LC_RPATH                 (logior #x1c LC_REQ_DYLD)) ; runpath additions
(defconstant LC_CODE_SIGNATURE        #x1d) ; local of code signature
(defconstant LC_SEGMENT_SPLIT_INFO    #x1e) ; local of info to split segments
(defconstant LC_REEXPORT_DYLIB        (logior #x1f LC_REQ_DYLD)) ; load and re-export dylib
(defconstant LC_LAZY_LOAD_DYLIB       #x20) ; delay load of dylib until first use
(defconstant LC_ENCRYPTION_INFO       #x21) ; encrypted segment information
(defconstant LC_DYLD_INFO             #x22) ; compressed dyld information
(defconstant LC_DYLD_INFO_ONLY        (logior #x22 LC_REQ_DYLD)) ; compressed dyld information only
(defconstant LC_LOAD_UPWARD_DYLIB     (logior #x23 LC_REQ_DYLD)) ; load upward dylib
(defconstant LC_VERSION_MIN_MACOSX    #x24  ) ; build for MacOSX min OS version
(defconstant LC_VERSION_MIN_IPHONEOS  #x25) ; build for iPhoneOS min OS version
(defconstant LC_FUNCTION_STARTS       #x26) ; compressed table of function start addresses
(defconstant LC_DYLD_ENVIRONMENT      #x27) ; string for dyld to treat like environment variable
(defconstant LC_MAIN                  (logior #x28 LC_REQ_DYLD)) ; replacement for LC_UNIXTHREAD
(defconstant LC_DATA_IN_CODE          #x29) ; table of non-instructions in __text
(defconstant LC_SOURCE_VERSION        #x2A) ; source version used to build binary
(defconstant LC_DYLIB_CODE_SIGN_DRS   #x2B) ; Code signing DRs copied from linked dylibs

(defun map-to-filetype (value)
  (cond ((= value MH_OBJECT) "OBJECT")
	((= value MH_EXECUTE) "EXECUTE")
	((= value MH_FVMLIB) "FVMLIB")
	((= value MH_CORE) "CORE")
	((= value MH_PRELOAD) "PRELOAD")
	((= value MH_DYLIB) "DYLIB")
	((= value MH_DYLINKER) "DYLINKER")
	((= value MH_BUNDLE) "BUNDLE")
	((= value MH_DYLIB_STUB) "DYLIB_STUB")
	((= value MH_DSYM) "DYSM")
	((= value MH_KEXT_BUNDLE) "KEXT_BUNDLE")
	(t "Unknown")))

(defun map-to-load-command (value)
  (cond ((= value LC_SEGMENT) "SEGMENT")
        ((= value LC_SYMTAB) "SYMTAB")
        ((= value LC_SYMSEG) "SYMSEG")
        ((= value LC_THREAD) "THREAD")
        ((= value LC_UNIXTHREAD) "UNIXTHREAD")
        ((= value LC_LOADFVMLIB) "LOADFVMLIB")
        ((= value LC_IDFVMLIB) "IDFVMLIB")
        ((= value LC_IDENT) "IDENT")
        ((= value LC_FVMFILE) "FVMFILE")
        ((= value LC_PREPAGE) "PREPAGE")
        ((= value LC_DYSYMTAB) "DYSYMTAB")
        ((= value LC_LOAD_DYLIB) "LOAD_DYLIB")
        ((= value LC_ID_DYLIB) "ID_DYLIB")
        ((= value LC_LOAD_DYLINKER) "LOAD_DYLINKER")
        ((= value LC_ID_DYLINKER) "ID_DYLINKER")
        ((= value LC_PREBOUND_DYLIB) "PREBOUND_DYLIB")
        ((= value LC_ROUTINES) "ROUTINES")
        ((= value LC_SUB_FRAMEWORK) "SUB_FRAMEWORK")
        ((= value LC_SUB_UMBRELLA) "SUB_UMBRELLA")
        ((= value LC_SUB_CLIENT) "SUB_CLIENT")
        ((= value LC_SUB_LIBRARY) "SUB_LIBRARY")
        ((= value LC_TWOLEVEL_HINTS) "TWOLEVEL_HINTS")
        ((= value LC_PREBIND_CKSUM) "PREBIND_CKSUM")
        ((= value LC_LOAD_WEAK_DYLIB) "LOAD_WEAK_DYLIB")
        ((= value LC_SEGMENT_64) "SEGMENT_64")
        ((= value LC_ROUTINES_64) "ROUTINES_64")
        ((= value LC_UUID) "UUID")
        ((= value LC_RPATH) "RPATH")
        ((= value LC_CODE_SIGNATURE) "CODE_SIGNATURE")
        ((= value LC_SEGMENT_SPLIT_INFO) "SEGMENT_SPLIT_INFO")
        ((= value LC_REEXPORT_DYLIB) "REEXPORT_DYLIB")
        ((= value LC_LAZY_LOAD_DYLIB) "LAZY_LOAD_DYLIB")
        ((= value LC_ENCRYPTION_INFO) "ENCRYPTION_INFO")
        ((= value LC_DYLD_INFO) "DYLD_INFO")
        ((= value LC_DYLD_INFO_ONLY) "DYLD_INFO_ONLY")
        ((= value LC_LOAD_UPWARD_DYLIB) "LOAD_UPWARD_DYLIB")
        ((= value LC_VERSION_MIN_MACOSX) "VERSION_MIN_MACOSX")
        ((= value LC_VERSION_MIN_IPHONEOS) "VERSION_MIN_IPHONEOS")
        ((= value LC_FUNCTION_STARTS) "FUNCTION_STARTS")
        ((= value LC_DYLD_ENVIRONMENT) "DYLD_ENVIRONMENT")
        ((= value LC_MAIN) "MAIN")
        ((= value LC_DATA_IN_CODE) "DATA_IN_CODE")
        ((= value LC_SOURCE_VERSION) "SOURCE_VERSION")
        ((= value LC_DYLIB_CODE_SIGN_DRS) "DYLIB_CODE_SIGN_DRS")
        (t "Unknown")))

(defun read-32-bit-word (input start)
  (logior (ash (aref input (+ start 3)) 24)
          (ash (aref input (+ start 2)) 16)
          (ash (aref input (+ start 1)) 8)
          (aref input start)))

(defun read-block-from-stream (stream start size)
  (let* ((bytes (make-array size :initial-element 0)))
    (file-position stream start)
    (read-sequence bytes stream :start 0 :end size)
    bytes))

(defun read-mach-header (stream)
  (let* ((bytes (read-block-from-stream stream 0 32))
	 (magic (read-32-bit-word bytes 0))
         (cputype (read-32-bit-word bytes 4))
         (cpusubtype (read-32-bit-word bytes 8))
         (filetype (read-32-bit-word bytes 12))
	 (ncmds (read-32-bit-word bytes 16))
	 (sizeofcmds (read-32-bit-word bytes 20))
	 (flags (read-32-bit-word bytes 24)))
    (values magic cputype cpusubtype filetype ncmds sizeofcmds flags)))

(defun read-load-command-vars (stream)
  (let* ((bytes (read-block-from-stream stream 16 8))
	 (ncmds (read-32-bit-word bytes 0))
	 (sizeofcmds (read-32-bit-word bytes 4)))
    (values ncmds sizeofcmds)))

(defun read-load-commands (stream)
  (multiple-value-bind (ncmds sizeofcmds) (read-load-command-vars stream)
    (let ((bytes (read-block-from-stream stream 32 sizeofcmds))
	  (cmdstart 0))
      (loop repeat ncmds do
	(let ((cmd (read-32-bit-word bytes cmdstart))
	      (cmdsize (read-32-bit-word bytes (+ cmdstart 4))))
	  (setf cmdstart (+ cmdstart cmdsize))
	  (format t "~d ~d~%" (map-to-load-command cmd) cmdsize))))))

(defun print-mach-header (filename)
  (with-open-file (in filename :element-type '(unsigned-byte 8))
    (multiple-value-bind (magic cputype cpusubtype filetype ncmds sizeofcmds flags) (binana::read-mach-header in)
      (format t "magic      #x~d~%" (write-to-string magic :base 16))
      (format t "cputype    ~d~%" cputype)
      (format t "cpusubtype ~d~%" cpusubtype)
      (format t "filetype   ~d~%" (map-to-filetype filetype))
      (format t "ncmds      ~d~%" ncmds)
      (format t "sizeofcmds ~d~%" sizeofcmds)
      (format t "flags      #x~d~%" (write-to-string flags :base 16)))))
