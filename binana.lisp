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
(defconstant LC_BUILD_VERSION         #x32) ; build version

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

(defstruct segment-command
  (cmd "SEGMENT" :type string)
  (segname "" :type string)
  (vmaddr 0 :type integer)
  (vmsize 0 :type integer)
  (fileoff 0 :type integer)
  (filesize 0 :type integer)
  (maxprot 0 :type integer)
  (initprot 0 :type integer)
  (nsects 0 :type integer)
  (flags 0 :type integer))

(defun read-segment-command (input)
  (make-segment-command :segname (read-string input 8 16)
			:vmaddr (read-32-bit-word input 16)
			:vmsize (read-32-bit-word input 20)
			:fileoff (read-32-bit-word input 24)
			:filesize (read-32-bit-word input 28)
			:maxprot (read-32-bit-word input 32)
			:initprot (read-32-bit-word input 36)
			:nsects (read-32-bit-word input 40)
			:flags (read-32-bit-word input 44)))

(defstruct symtab-command
  (cmd "SYMTAB" :type string)
  (symoff 0 :type integer)
  (nsyms 0 :type integer)
  (stroff 0 :type integer)
  (strsize 0 :type integer))

(defun read-symtab-command (input)
  (make-symtab-command :symoff (read-32-bit-word input 8)
		       :nsyms (read-32-bit-word input 12)
		       :stroff (read-32-bit-word input 16)
		       :strsize (read-32-bit-word input 20)))

(defstruct dysymtab-command
  (cmd "DYSYMTAB" :type string)
  (ilocalsym 0 :type integer)
  (nlocalsym 0 :type integer)
  (iextdefsym 0 :type integer)
  (nextdefsym 0 :type integer)
  (iundefsym 0 :type integer)
  (nundefsym 0 :type integer)
  (tocoff 0 :type integer)
  (ntoc 0 :type integer)
  (modtaboff 0 :type integer)
  (nmodtab 0 :type integer)
  (extrefsymoff 0 :type integer)
  (nextrefsyms 0 :type integer)
  (indirectsymoff 0 :type integer)
  (nindirectsyms 0 :type integer)
  (extreloff 0 :type integer)
  (nextrel 0 :type integer)
  (locreloff 0 :type integer)
  (nlocrel 0 :type integer))

(defun read-dysymtab-command (input)
  (make-dysymtab-command :ilocalsym (read-32-bit-word input 8)
			 :nlocalsym (read-32-bit-word input 12)
			 :iextdefsym (read-32-bit-word input 16)
			 :nextdefsym (read-32-bit-word input 20)
			 :iundefsym (read-32-bit-word input 24)
			 :nundefsym (read-32-bit-word input 28)
			 :tocoff (read-32-bit-word input 32)
			 :ntoc (read-32-bit-word input 36)
			 :modtaboff (read-32-bit-word input 40)
			 :nmodtab (read-32-bit-word input 44)
			 :extrefsymoff (read-32-bit-word input 48)
			 :nextrefsyms (read-32-bit-word input 52)
			 :indirectsymoff (read-32-bit-word input 56)
			 :nindirectsyms (read-32-bit-word input 60)
			 :extreloff (read-32-bit-word input 64)
			 :nextrel (read-32-bit-word input 68)
			 :locreloff (read-32-bit-word input 72)
			 :nlocrel (read-32-bit-word input 76)))

(defstruct dylib-info
  (name "" :type string)
  (timestamp 0 :type integer)
  (current-version "" :type string)
  (compatibility-version "" :type string))

(defstruct dylib-command
  (cmd "LOAD_DYLIB" :type string)
  dylib)

(defun read-dylib-command (input)
  (let ((cmdsize (read-32-bit-word input 4))
	(offset (read-32-bit-word input 8))
	(timestamp (read-32-bit-word input 12))
	(current-version (read-version input 16))
	(compatibility-version (read-version input 20)))
    (make-dylib-command :dylib (make-dylib-info :name (read-string input offset (- cmdsize offset))
						:timestamp timestamp
						:current-version current-version
						:compatibility-version compatibility-version))))

(defstruct load-dylinker-command
  (cmd "LOAD_DYLINKER" :type string)
  (name "" :type string))

(defun read-load-dylinker-command (input)
  (let ((cmdsize (read-32-bit-word input 4))
	(offset (read-32-bit-word input 8)))
    (make-load-dylinker-command :name (read-string input offset (- cmdsize offset)))))

(defstruct dyld-info-only-command
  (cmd "DYLD_INFO_ONLY" :type string)
  (rebase-off 0 :type integer)
  (rebase-size 0 :type integer)
  (bind-off 0 :type integer)
  (bind-size 0 :type integer)
  (weak-bind-off 0 :type integer)
  (weak-bind-size 0 :type integer)
  (lazy-bind-off 0 :type integer)
  (lazy-bind-size 0 :type integer)
  (export-off 0 :type integer)
  (export-size 0 :type integer))

(defun read-dyld-info-only-command (input)
  (make-dyld-info-only-command :rebase-off (read-32-bit-word input 8)
			       :rebase-size (read-32-bit-word input 12)
			       :bind-off (read-32-bit-word input 16)
			       :bind-size (read-32-bit-word input 20)
			       :weak-bind-off (read-32-bit-word input 24)
			       :weak-bind-size (read-32-bit-word input 28)
			       :lazy-bind-off (read-32-bit-word input 32)
			       :lazy-bind-size (read-32-bit-word input 36)
			       :export-off (read-32-bit-word input 40)
			       :export-size (read-32-bit-word input 44)))


(defstruct dyld-info-command
  (cmd "DYLD_INFO" :type string)
  (rebase-off 0 :type integer)
  (rebase-size 0 :type integer)
  (bind-off 0 :type integer)
  (bind-size 0 :type integer)
  (weak-bind-off 0 :type integer)
  (weak-bind-size 0 :type integer)
  (lazy-bind-off 0 :type integer)
  (lazy-bind-size 0 :type integer)
  (export-off 0 :type integer)
  (export-size 0 :type integer))

(defun read-dyld-info-command (input)
  (make-dyld-info-command :rebase-off (read-32-bit-word input 8)
			  :rebase-size (read-32-bit-word input 12)
			  :bind-off (read-32-bit-word input 16)
			  :bind-size (read-32-bit-word input 20)
			  :weak-bind-off (read-32-bit-word input 24)
			  :weak-bind-size (read-32-bit-word input 28)
			  :lazy-bind-off (read-32-bit-word input 32)
			  :lazy-bind-size (read-32-bit-word input 36)
			  :export-off (read-32-bit-word input 40)
			  :export-size (read-32-bit-word input 44)))

(defstruct segment-command-64
  (cmd "SEGMENT_64" :type string)
  (segname "" :type string)
  (vmaddr 0 :type integer)
  (vmsize 0 :type integer)
  (fileoff 0 :type integer)
  (filesize 0 :type integer)
  (maxprot 0 :type integer)
  (initprot 0 :type integer)
  (nsects 0 :type integer)
  (flags 0 :type integer))

(defun read-segment-command-64 (input)
  (make-segment-command-64 :segname (read-string input 8 16)
			   :vmaddr (read-32-bit-word input 16)
			   :vmsize (read-32-bit-word input 20)
			   :fileoff (read-32-bit-word input 24)
			   :filesize (read-32-bit-word input 28)
			   :maxprot (read-32-bit-word input 32)
			   :initprot (read-32-bit-word input 36)
			   :nsects (read-32-bit-word input 40)
			   :flags (read-32-bit-word input 44)))

(defstruct uuid-command
  (cmd "UUID" :type string)
  (uuid "" :type string))

(defun read-uuid (input start)
  (format nil "~2,'0X~2,'0X~2,'0X~2,'0X-~2,'0X~2,'0X-~2,'0X~2,'0X-~2,'0X~2,'0X-~2,'0X~2,'0X~2,'0X~2,'0X~2,'0X~2,'0X"
	  (aref input start)
	  (aref input (+ start 1))
	  (aref input (+ start 2))
	  (aref input (+ start 3))
	  (aref input (+ start 4))
	  (aref input (+ start 5))
	  (aref input (+ start 6))
	  (aref input (+ start 7))
	  (aref input (+ start 8))
	  (aref input (+ start 9))
	  (aref input (+ start 10))
	  (aref input (+ start 11))
	  (aref input (+ start 12))
	  (aref input (+ start 13))
	  (aref input (+ start 14))
	  (aref input (+ start 15))))

(defun read-uuid-command (input)
  (make-uuid-command :uuid (read-uuid input 8)))

(defstruct source-version-command
  (cmd "SOURCE_VERSION" :type string)
  (version "" :type string))

(defun read-source-version (input start)
  (let ((d (read-64-bit-word input start)))
    (format nil "~d.~d.~d.~d.~d"
	    (ash (logand d #xFFFFFF0000000000) -40)
	    (ash (logand d #x000000FFC0000000) -30)
	    (ash (logand d #x000000003FF00000) -20)
	    (ash (logand d #x00000000000FFC00) -10)
	    (logand d #x00000000000003FF))))

(defun read-source-version-command (input)
  (make-source-version-command :version (read-source-version input 8)))

(defstruct build-version-command
  (cmd "BUILD_VERSION" :type string)
  (platform 0 :type integer)
  (minos 0 :type string)
  (sdk 0 :type string)
  (ntools 0 :type integer)
  (tool 0 :type integer)
  (version 0 :type string))

(defun read-build-version (input start)
  (let ((d (read-32-bit-word input start)))
    (format nil "~d.~d.~d"
	    (ash (logand d #xFFFF0000) -16)
	    (ash (logand d #x0000FF00)  -8)
	    (logand d #x000000FF))))

(defun read-build-version-command (input)
  (make-build-version-command :platform (read-32-bit-word input 8)
			      :minos (read-build-version input 12)
			      :sdk (read-build-version input 16)
			      :ntools (read-32-bit-word input 20)
			      :tool (read-32-bit-word input 24)
			      :version (read-build-version input 28)))

(defstruct entry-point-command
  (cmd "MAIN" :type string)
  (entryoff 0 :type integer)
  (stacksize 0 :type integer))

(defun read-entry-point-command (input)
  (make-entry-point-command :entryoff (read-64-bit-word input 8)
			    :stacksize (read-64-bit-word input 16)))

(defstruct function-start-command
  (cmd "FUNCTION_START" :type string)
  (dataoff 0 :type integer)
  (datasize 0 :type integer))

(defun read-function-start-command (input)
  (make-function-start-command :dataoff (read-32-bit-word input 8)
			       :datasize (read-32-bit-word input 12)))

(defstruct data-in-code-command
  (cmd "DATA_IN_CODE" :type string)
  (dataoff 0 :type integer)
  (datasize 0 :type integer))

(defun read-data-in-code-command (input)
  (make-data-in-code-command :dataoff (read-32-bit-word input 8)
			     :datasize (read-32-bit-word input 12)))

(defun map-to-load-command (cmd input)
  (cond ((= cmd LC_SEGMENT) (read-segment-command input))
        ((= cmd LC_SYMTAB) (read-symtab-command input))
        ((= cmd LC_SYMSEG) "SYMSEG")
        ((= cmd LC_THREAD) "THREAD")
        ((= cmd LC_UNIXTHREAD) "UNIXTHREAD")
        ((= cmd LC_LOADFVMLIB) "LOADFVMLIB")
        ((= cmd LC_IDFVMLIB) "IDFVMLIB")
        ((= cmd LC_IDENT) "IDENT")
        ((= cmd LC_FVMFILE) "FVMFILE")
        ((= cmd LC_PREPAGE) "PREPAGE")
        ((= cmd LC_DYSYMTAB) (read-dysymtab-command input))
        ((= cmd LC_LOAD_DYLIB) (read-dylib-command input))
        ((= cmd LC_ID_DYLIB) "ID_DYLIB")
        ((= cmd LC_LOAD_DYLINKER) (read-load-dylinker-command input))
        ((= cmd LC_ID_DYLINKER) "ID_DYLINKER")
        ((= cmd LC_PREBOUND_DYLIB) "PREBOUND_DYLIB")
        ((= cmd LC_ROUTINES) "ROUTINES")
        ((= cmd LC_SUB_FRAMEWORK) "SUB_FRAMEWORK")
        ((= cmd LC_SUB_UMBRELLA) "SUB_UMBRELLA")
        ((= cmd LC_SUB_CLIENT) "SUB_CLIENT")
        ((= cmd LC_SUB_LIBRARY) "SUB_LIBRARY")
        ((= cmd LC_TWOLEVEL_HINTS) "TWOLEVEL_HINTS")
        ((= cmd LC_PREBIND_CKSUM) "PREBIND_CKSUM")
        ((= cmd LC_LOAD_WEAK_DYLIB) "LOAD_WEAK_DYLIB")
        ((= cmd LC_SEGMENT_64) (read-segment-command-64 input))
        ((= cmd LC_ROUTINES_64) "ROUTINES_64")
        ((= cmd LC_UUID) (read-uuid-command input))
        ((= cmd LC_RPATH) "RPATH")
        ((= cmd LC_CODE_SIGNATURE) "CODE_SIGNATURE")
        ((= cmd LC_SEGMENT_SPLIT_INFO) "SEGMENT_SPLIT_INFO")
        ((= cmd LC_REEXPORT_DYLIB) "REEXPORT_DYLIB")
        ((= cmd LC_LAZY_LOAD_DYLIB) "LAZY_LOAD_DYLIB")
        ((= cmd LC_ENCRYPTION_INFO) "ENCRYPTION_INFO")
        ((= cmd LC_DYLD_INFO) (read-dyld-info-command input))
        ((= cmd LC_DYLD_INFO_ONLY) (read-dyld-info-only-command input))
        ((= cmd LC_LOAD_UPWARD_DYLIB) "LOAD_UPWARD_DYLIB")
        ((= cmd LC_VERSION_MIN_MACOSX) "VERSION_MIN_MACOSX")
        ((= cmd LC_VERSION_MIN_IPHONEOS) "VERSION_MIN_IPHONEOS")
        ((= cmd LC_FUNCTION_STARTS) (read-function-start-command input))
        ((= cmd LC_DYLD_ENVIRONMENT) "DYLD_ENVIRONMENT")
        ((= cmd LC_MAIN) (read-entry-point-command input))
        ((= cmd LC_DATA_IN_CODE) (read-data-in-code-command input))
        ((= cmd LC_SOURCE_VERSION) (read-source-version-command input))
        ((= cmd LC_DYLIB_CODE_SIGN_DRS) "DYLIB_CODE_SIGN_DRS")
	((= cmd LC_BUILD_VERSION) (read-build-version-command input))
        (t "Unknown")))

(defun read-version (input start)
  (let ((patch (aref input start))
	(minor (aref input (+ start 1)))
	(major (logior (ash (aref input (+ start 3)) 8)
		       (aref input (+ start 2)))))
    (let ((output-string (make-array 0
				     :element-type 'character
				     :adjustable T
				     :fill-pointer 0)))
      (declare (type string output-string))
      (format output-string "~d.~d.~d" major minor patch)
      output-string)))

(defun read-block (stream start size)
  (let* ((bytes (make-array size :initial-element 0)))
    (file-position stream start)
    (read-sequence bytes stream :start 0 :end size)
    bytes))

(defun read-mach-header (stream)
  (let* ((bytes (read-block stream 0 32))
	 (magic (read-32-bit-word bytes 0))
         (cputype (read-32-bit-word bytes 4))
         (cpusubtype (read-32-bit-word bytes 8))
         (filetype (read-32-bit-word bytes 12))
	 (ncmds (read-32-bit-word bytes 16))
	 (sizeofcmds (read-32-bit-word bytes 20))
	 (flags (read-32-bit-word bytes 24)))
    (values magic cputype cpusubtype filetype ncmds sizeofcmds flags)))

(defun read-load-command-vars (stream)
  (let* ((bytes (read-block stream 16 8))
	 (ncmds (read-32-bit-word bytes 0))
	 (sizeofcmds (read-32-bit-word bytes 4)))
    (values ncmds sizeofcmds)))

(defun read-load-commands (stream)
  (multiple-value-bind (ncmds sizeofcmds) (read-load-command-vars stream)
    (let ((bytes (read-block stream 32 sizeofcmds))
	  (cmdstart 0))
      (loop repeat ncmds collect
			 (let* ((cmd (read-32-bit-word bytes cmdstart))
				(cmdsize (read-32-bit-word bytes (+ cmdstart 4)))
				(s (subseq bytes cmdstart (+ cmdstart cmdsize))))
			   (setf cmdstart (+ cmdstart cmdsize))
			   (list (map-to-load-command cmd s)))))))

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
