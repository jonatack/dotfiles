;;;;
;;;; .ccl-init.lisp
;;;;
;;;; Config file for CCL - Clozure Common Lisp.
;;;;
;;;; Copyright (C) 2019 Jon Atack <jon@atack.com>

(load "~/.lisprc")

(setf *quit-on-eof* t)
(setf *load-preserves-optimization-settings* t)

(defun jon-lisp-prompt-format (stream level)
  (if (zerop level)
    (format stream "~%[ClozureCL] ~A> " (package-name *package*))
    (format stream "~%[~d] > " level)))

(setf ccl:*listener-prompt-format* #'jon-lisp-prompt-format)

(setf ccl:*default-file-character-encoding* :utf-8)

(require "asdf")

;;; ======================================================
;;; Configure CCL with Quicksearch and selected libraries
;;; ======================================================

;; Readline-style line-editor for Common Lisp
;; Repository and more info at: https://github.com/sharplispers/linedit
;; (ql:quickload :linedit)
;; (linedit:install-repl :wrap-current t :eof-quits t)

(handler-bind
    ((style-warning #'muffle-warning)
     (warning #'muffle-warning))

  ;;; =======================================
  ;;; Quicksearch for finding libraries
  ;;; =======================================
  (asdf:load-system 'quicksearch)

  ;; Quicksearch looks for CL projects in Quicklisp, Cliki, GitHub and BitBucket.
  ;;
  ;; Example:
  ;;
  ;; (qs:? 'cell :du 5)
  ;;
  ;; => searches all sources for "cell"
  ;;    and displays first 5 results with descriptions + URLs
  ;;
  ;; The search word must be a string, number or symbol.
  ;; A symbol is converted to a downcased string.
  ;;
  ;; If the search word contains a space (i.e. "foo bar")
  ;; the Quicklisp-search is OR-searched (foo or bar)
  ;; and Cliki-search, GitHub, and BitBucket are AND-searched (foo and bar).
  ;;
  ;; Options:
  ;;
  ;;   d -- show descriptions (or QuickDocs URLs for Quicklisp search)
  ;;   u -- show URLs
  ;;   q -- search in Quicklisp
  ;;   c -- search in Cliki
  ;;   g -- search in GitHub
  ;;   b -- search in Bitbucket
  ;;   Cut-Off: Max number of displayed results (default is 50).
  ;;
  ;; - Options are idempotent: :dd <=> :d.
  ;; - The order of character options doesn't matter: :du <=> :ud.
  ;; - If options contain more than 2 cut-offs, only the last one is applied.
  ;; - If no search space is specified, default is all (e.g. :d <=> :dqcgb)
  ;; - If any search spaces are specified, the others are not specified.
  ;;
  ;; Config:
  ;;
  ;; (qs:config :maximum-columns-of-description 104
  ;;            :maximum-number-of-fetching-repositories 50 :max-repos-supplied 50
  ;;            :cache-size 4 :threading? t :quicklisp-verbose? t)
  ;;
  ;; To see this information:
  ;; C-c C-d a or M-x slime-apropos, followed by "quicksearch".


  ;;; =======================================
  ;;; Utilities
  ;;; =======================================
  (asdf:load-system 'parse-float)


  ;;; =======================================
  ;;; Functional programming
  ;;; =======================================

  ;; Pattern Matching
  ;; (asdf:load-system 'optima)
  ;; (asdf:load-system 'trivia) ; newer, faster, drop-in replacement for optima
  ;; (asdf:load-system 'cl-algebraic-data-type)
  ;; (asdf:load-system 'algebraic-data-library)
  ;; (asdf:load-system 'abacus)
  ;; (asdf:load-system 'cl-unification)

  ;; FRP (Functional Reactive Programming Framework)
  ;; (asdf:load-system 'cells)

  ;; Functional Data Structures
  ;; (asdf:load-system 'sycamore)
  ;; (asdf:load-system 'fset)

  ;; Functional Idioms Library
  ;; (asdf:load-system 'folio)
  ;; (asdf:load-system 'folio2)
  ;; (asdf:load-system 'taps)


  ;;; =======================================
  ;;; Concurrent Programming
  ;;; =======================================

  ;; Actor Model
  ;; (asdf:load-system 'cl-actors)

  ;; CSP Concurrent Process Algebra
  ;; (asdf:load-system 'chanl)
  ;; (asdf:load-system 'calispel)

  ;; Software Transactional Memory
  ;; (asdf:load-system 'cl-stm)   ; original
  ;; (asdf:load-system 'cl-stmx)  ; high performance


  ;;; =======================================
  ;;; Static Code Analysis
  ;;; =======================================

  (asdf:load-system 'lisp-critic)

  ;; Appease Paren Critics
  ;; (asdf:load-system 'readable)

  ;;; =======================================
  ;;; Benchmarking
  ;;; =======================================

  ;; (asdf:load-system 'trivial-benchmark)

  ;;; =======================================
  ;;;  Default Namespaces
  ;;; =======================================

  ;; (use-package :trivial-benchmark)
  ;; (use-package :optima)
  ;; (use-package :trivia) ; newer, faster, drop-in replacement for optima
  ;; (use-package :algebraic-data-library)
  ;; (use-package :abacus)
  ;; (use-package :cells)
  ;; (use-package :sycamore)
  ;; (use-package :fset-user)
  ;; (use-package :folio2)
  ;; (use-package :taps)
  ;; (use-package :cl-actors)
  ;; (use-package :lisp-critic)


  ;;; =======================================
  ;;; Unit testing
  ;;; =======================================

  (asdf:load-system 'rove)
  ;; (asdf:load-system 'prove)
  ;; (asdf:load-system 'fiveam)
  ;; (asdf:load-system 'parachute)
  ;; (ql:quickload "clunit")
  )

;;; =======================================
;;; Load current side project
;;; =======================================

(ql:quickload :cl-kraken)
