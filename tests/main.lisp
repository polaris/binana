;;;; main.lisp

(in-package #:binana/tests)

(def-suite :binana)

(in-suite :binana)

(def-suite* :binana/tests :in :binana)

(test test-read-32-bit-word
  (is (= (binana::read-32-bit-word #(207 250 237 254) 0) 4277009103)))
