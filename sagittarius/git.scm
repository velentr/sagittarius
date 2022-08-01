;;; SPDX-FileCopyrightText: 2022 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-only

(define-module (sagittarius git)
  #:use-module (sagittarius path)
  #:use-module (sagittarius subprocess)
  #:export (git-init
            git-repo?))

;;; Commentary:
;;;
;;; Git helper functions.
;;;
;;; Code:

(define (git-repo? path)
  "Determine if PATH is a valid non-bare git repository."
  (let ((git-dir (join path ".git")))
    (directory? git-dir)))

(define (git-init path)
  "Create a new git repository at PATH, if one does not already exist."
  (if (not (git-repo? path))
      (check-call git "init" path)))
