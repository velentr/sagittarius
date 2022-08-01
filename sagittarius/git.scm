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
  "Determine if PATH is a valid bare git repository."
  (let ((obj-dir (join path "objects"))
        (head-file (join path "HEAD")))
    (and (directory? obj-dir)
         (file? head-file))))

(define (git-init path)
  "Create a new git repository at PATH, if one does not already exist."
  (if (not (git-repo? path))
      (check-call git "init" "--bare" path)))
