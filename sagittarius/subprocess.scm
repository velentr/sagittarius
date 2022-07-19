;;; SPDX-FileCopyrightText: 2022 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-only

(define-module (sagittarius subprocess)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:export (call
            check-call))

;;; Commentary:
;;;
;;; Helper utilities for calling other programs.
;;;
;;; This module aims to make it easier to execute another program as a
;;; subprocess, capture its output, and examine its return value.
;;;
;;; Code:

(define (call arg0 . argv)
  "Call the program ARG0 with arguments ARGV, capturing its output. If the
program exits successfully, the result is the subprocess's stdout broken up as a
list of lines. If the program exits abnormally, the result is #f."
  (let ((port (apply open-pipe* (cons OPEN_READ (cons arg0 argv)))))
    (define (read-output so-far)
      (let ((line (read-line port)))
        (if (eof-object? line)
            so-far
            (read-output (cons line so-far)))))
    (let ((output-rev (read-output '()))
          (status (close-pipe port)))
      (if (and (status:exit-val status)
               (= 0 (status:exit-val status)))
          (reverse output-rev)
          #f))))

(define (check-call arg0 . argv)
  "Call the program ARG0 with arguments ARGV, capturing its output. The return
value is the subprocess's stdout broken up as a list of lines. If the program
exits abnormally, print an error and exit."
  (let ((output (apply call (cons arg0 argv))))
    (if output
        output
        (begin
          (format (current-error-port)
                  "command ~a failed!~%"
                  (cons arg0 argv))
          (exit #f)))))
