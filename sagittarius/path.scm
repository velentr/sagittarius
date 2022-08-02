;;; SPDX-FileCopyrightText: 2022 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-only

(define-module (sagittarius path)
  #:export (directory?
            file?
            git
            git-receive-pack
            git-upload-pack
            home
            join
            sagittarius-bin))

;;; Commentary:
;;;
;;; Path manipulation and constants.
;;;
;;; This module has roughly 3 functions relating to filesystem paths:
;;;
;;; 1. Define paths for all executables called via subprocess. Normally these
;;;    are found from $PATH, but on systems like guix or nix it is convenient to
;;;    replace these with absolute paths to package inputs.
;;;
;;; 2. Functionality for examining files and directories.
;;;
;;; 3. Code for creating and manipulating path strings given directory
;;;    components.
;;;
;;; Code:

(define git "git")
(define git-receive-pack "git-receive-pack")
(define git-upload-pack "git-upload-pack")
(define sagittarius-bin "sagittarius")

(define (stat-is-type path type)
  "Determine if the inode at PATH has the given TYPE."
  (let ((st (stat path #f)))
    (and st (eq? (stat:type st) type))))

(define (directory? path)
  "Determine if PATH is a directory."
  (stat-is-type path 'directory))

(define (file? path)
  "Determine if PATH is a regular file."
  (stat-is-type path 'regular))

(define (join . directories)
  "Return the DIRECTORIES components joined together as a path."
  (string-join directories "/"))

(define (home . directories)
  "Return the path with DIRECTORIES components in the user's home directory."
  (let ((HOME (getenv "HOME")))
    (apply join (cons HOME directories))))
