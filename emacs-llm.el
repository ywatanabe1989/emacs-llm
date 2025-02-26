;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 16:58:46>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm.el

(defun --lle-load-file-silent
    (file-path)
  "Load Emacs Lisp file at FILE-PATH silently.
Suppresses all messages, warnings and outputs during loading.
Only error messages will be shown if any.

Arguments:
- FILE-PATH: Path to the Emacs Lisp file to load"
  (let
      ((inhibit-message t)
       (message-log-max nil))
    (with-temp-message ""
      (with-temp-buffer
        ;; Temporarily redirect stderr
        (let
            ((standard-output
              (current-buffer))
             (warning-minimum-level :error))
          (load-file file-path))))))

(defun --lle-add-subdirs-to-loadpath
    (parent)
  "Add all visible (non-hidden) subdirectories of PARENT to `load-path`.
Ignores any directories in `--lle-excluded-dirs`."
  (let
      ((candidates
        (directory-files parent t "\\`[^.]")))
    ;; skip hidden dirs: '^.'
    (dolist
        (dir candidates)
      (let
          ((name
            (file-name-nondirectory dir)))
        (when
            (file-directory-p dir)
          ;; (message "Adding to load-path: %s" dir)
          (add-to-list 'load-path dir))))))

;; 1) Figure out this file's directory
(let
    ((root
      (file-name-directory
       (or load-file-name buffer-file-name))))
  ;; 2) Add immediate subdirectories to load-path
  (--lle-add-subdirs-to-loadpath root))

(add-to-list 'load-path "/home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/")

(require 'emacs-llm-variables)
(require 'emacs-llm-providers)
(require 'emacs-llm-bindings)
(require 'emacs-llm-display)
(require 'emacs-llm-history)

(require 'emacs-llm-run)
(require 'emacs-llm-spinner)
(require 'emacs-llm-switch)

(provide 'emacs-llm)

(when
    (not load-file-name)
  (message "emacs-llm.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))