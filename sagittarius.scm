;;; SPDX-FileCopyrightText: 2022 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-only

(define-module (sagittarius)
  #:use-module (ice-9 optargs)
  #:use-module (sagittarius config)
  #:use-module (sagittarius git)
  #:use-module (sagittarius path)
  #:export (reconfigure
            sagittarius-main))

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

(define (handle-git-remote args config)
  "Handle a git command on a remote, with cli ARGS and the sagittarius CONFIG."
  (let* ((user (car args))
         (profile (config->profile config user))
         (SSH_ORIGINAL_COMMAND (string-split (getenv "SSH_ORIGINAL_COMMAND")
                                             #\space))
         (command (car SSH_ORIGINAL_COMMAND))
         (repo-name (strip-quotes (cadr SSH_ORIGINAL_COMMAND)))
         (repository (config->repository config repo-name)))
    (if (or (not repository)
            (not profile))
        (exit #f)
        (cond
         ((and (equal? command "git-receive-pack")
               (can-write-repository repository profile))
          (execlp git-receive-pack "git-receive-pack" repo-name))
         ((and (equal? command "git-upload-pack")
               (can-read-repository repository profile))
          (execlp git-upload-pack "git-upload-pack" repo-name))
         (else
          (exit #f))))))

(define (hook args config)
  "Run a git hook on a repository."
  (let* ((repo-name (car args))
         (repository (config->repository config repo-name))
         (hook-name (cadr args))
         (hook (and repository
                    (assoc-ref (repository->hooks repository) hook-name))))
    (if hook
        (hook))))

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
                        "command=\"~a handle-git-remote ~a\" ~a~%"
                        sagittarius-bin
                        email
                        key))
              (profile->keys profile))))
         profiles)))))

(define (init-hooks repo repo-path)
  (let ((hooks (repository->hooks repo))
        (hooks-path (join repo-path "hooks"))
        (repo-name (repository->name repo)))
    (for-each
     (lambda (hook-spec)
       (let* ((hook-name (car hook-spec))
              (hook-path (join hooks-path hook-name))
              (hook-string
               (format #f
                       "#!/usr/bin/env sh~%~a hook ~a ~a~%"
                       sagittarius-bin
                       repo-name
                       hook-name)))
         (with-output-to-file hook-path
           (lambda ()
             (display hook-string)))
         (chmod hook-path #o755)))
     hooks)))

(define (reconfigure-repositories config)
  (for-each
   (lambda (repo)
     (let ((repo-path (home (repository->name repo))))
       (git-init repo-path)
       (init-hooks repo repo-path)))
   (config->repositories config)))

(define (reconfigure config)
  (reconfigure-authorized-keys config)
  (reconfigure-repositories config))

(define (sagittarius-main load-config . command-line)
  (let ((command (cadr command-line))
        (args (cddr command-line)))
    (cond ((equal? command "init")
           (init))
          ((equal? command "handle-git-remote")
           (handle-git-remote args (load-config)))
          ((equal? command "hook")
           (hook args (load-config)))
          ((equal? command "reconfigure")
           (reconfigure (load-config))))))
