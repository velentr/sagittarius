;;; SPDX-FileCopyrightText: 2022 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-only

(define-module (sagittarius)
  #:use-module (ice-9 optargs)
  #:use-module (sagittarius config)
  #:use-module (sagittarius git)
  #:use-module (sagittarius path)
  #:export (sagittarius-main))

(define (init)
  (git-init (home "sagittarius-config")))

(define (strip-quotes s)
  (let* ((len (string-length s))
         (first (string-ref s 0))
         (last  (string-ref s (- len 1))))
    (if (or (and (eq? first #\")
                 (eq? last  #\"))
            (and (eq? first #\')
                 (eq? last  #\')))
        (substring s 1 (- len 1))
        s)))

(define (receive-pack args config)
  (let* ((user (car args))
         (profile (config->profile config user))
         (SSH_ORIGINAL_COMMAND (string-split (getenv "SSH_ORIGINAL_COMMAND")
                                             #\space))
         (command (car SSH_ORIGINAL_COMMAND))
         (repo-name (strip-quotes (cadr SSH_ORIGINAL_COMMAND)))
         (repository (config->repository config repo-name)))
    (if (or (not (equal? command "git-receive-pack"))
            (not repository)
            (not profile)
            (not (can-write-repository repository profile)))
        (exit #f)
        (execlp git-receive-pack "git-receive-pack" repo-name))))

(define (reconfigure-authorized-keys config)
  (let ((profiles (config->profiles config)))
    (call-with-output-file (config->authorized-keys config)
      (lambda (output-port)
        (for-each
         (lambda (profile)
           (let ((email (profile->email profile)))
             (for-each
              (lambda (key)
                (format output-port
                        "command=\"~a receive-pack ~a\" ~a~%"
                        sagittarius-bin
                        email
                        key))
              (profile->keys profile))))
         profiles)))))

(define (reconfigure-repositories config)
  (for-each
   (lambda (repo)
     (git-init (home (repository->name repo))))
   (config->repositories config)))

(define (reconfigure config)
  (reconfigure-authorized-keys config)
  (reconfigure-repositories config))

(define (sagittarius-main load-config . command-line)
  (let ((command (cadr command-line))
        (args (cddr command-line)))
    (cond ((equal? command "init")
           (init))
          ((equal? command "receive-pack")
           (receive-pack args (load-config)))
          ((equal? command "reconfigure")
           (reconfigure (load-config))))))
