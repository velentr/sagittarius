;;; SPDX-FileCopyrightText: 2022 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-only

(define-module (sagittarius config)
  #:use-module (srfi srfi-9)
  #:use-module (sagittarius path)
  #:export (can-read-repository
            can-write-repository
            config
            config->authorized-keys
            config->profile
            config->profiles
            config->repositories
            config->repository
            profile
            profile?
            profile->email
            profile->keys
            repository
            repository?
            repository->hooks
            repository->name))

;;; Commentary:
;;;
;;; Data structures for building sagittarius config files.
;;;
;;; The main sagittarius configuration file is at
;;; ~/sagittarius-config/config.scm and should evaluate to a single CONFIG
;;; structure that specifies all profiles and repositories. The profiles are
;;; used to set up ~/.ssh/authorized_keys for access control and the
;;; repositories are created if they do not yet exist.
;;;
;;; Code:

(define-record-type <profile>
  (profile-constructor email keys)
  profile?
  (email profile->email)
  (keys profile->keys))

(define* (profile #:key email (keys '()))
  "Create an identity for a single user with the specified EMAIL and public
KEYS."
  (profile-constructor email keys))

(define-record-type <repository>
  (repository-constructor ro rw name hooks)
  repository?
  (ro repository->ro-profiles)
  (rw repository->rw-profiles)
  (name repository->name)
  (hooks repository->hooks))

(define* (repository #:key (ro '()) (rw '()) name (hooks '()))
  ;; FIXME: validate name
  (repository-constructor ro rw name hooks))

(define (repository->rd-keys repo)
  (let ((readers (append (repository->ro-profiles repo)
                         (repository->rw-profiles repo))))
    (apply append (map profile->keys readers))))

(define (repository->wr-keys repo)
  (apply append (map profile->keys (repository->rw-profiles repo))))

(define (match-key repo-keys profile-keys)
  (cond ((eq? profile-keys '())
         #f)
        ((member (car profile-keys) repo-keys)
         #t)
        (#t
         (match-key repo-keys (cdr profile-keys)))))

(define (can-read-repository repo profile)
  "Check if REPO can be read by the user with PROFILE."
  (let ((read-keys (repository->rd-keys repo))
        (profile-keys (profile->keys profile)))
    (match-key read-keys profile-keys)))

(define (can-write-repository repo profile)
  "Check if REPO can be written by the user with PROFILE."
  (let ((write-keys (repository->wr-keys repo))
        (profile-keys (profile->keys profile)))
    (match-key write-keys profile-keys)))

(define-record-type <config>
  (config-constructor repositories profiles authorized-keys)
  config?
  (repositories config->repositories)
  (profiles config->profiles)
  (authorized-keys config->authorized-keys))

(define* (config #:key repositories
                 profiles
                 (authorized-keys (home ".ssh" "authorized_keys")))
  "Create a sagittarius configuration composed of REPOSITORIES with the given
user PROFILES. The configuration will write to AUTHORIZED-KEYS."
  (config-constructor repositories profiles authorized-keys))

(define (config->repository config repo-name)
  "Check CONFIG for the repository named REPO-NAME. Returns #f if the repository
is not found."
  (let ((maybe-repo (filter (lambda (repo)
                              (equal? (repository->name repo) repo-name))
                            (config->repositories config))))
    (if (eq? maybe-repo '())
        #f
        (car maybe-repo))))

(define (config->profile config profile-name)
  "Check CONFIG for the profile with email PROFILE-NAME. Returns #f if the
profile is not found."
  (let ((maybe-profile (filter (lambda (profile)
                                 (equal? (profile->email profile) profile-name))
                               (config->profiles config))))
    (if (eq? maybe-profile '())
        #f
        (car maybe-profile))))
