;;; SPDX-FileCopyrightText: 2022 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-only

(define-module (sagittarius path)
  #:export (git
            git-receive-pack
            home
            sagittarius-bin))

;;; Commentary:
;;;
;;; Path manipulation and constants.
;;;
;;; Code:

;; FIXME
(define git
  "/home/bkubisiak/.guix-home/profile/bin/git")
(define git-receive-pack
  "/home/bkubisiak/.guix-home/profile/bin/git-receive-pack")
(define sagittarius-bin
  "/home/bkubisiak/src/sagittarius/pre-inst-env.sh sagittarius")

(define (home . directories)
  "Return the path with DIRECTORIES components in the user's home directory."
  (let ((HOME (getenv "HOME")))
    (string-join (cons HOME directories) "/")))
