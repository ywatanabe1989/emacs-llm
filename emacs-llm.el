;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 22:07:50>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm.el

;; Path and Loading
;; ----------------------------------------

(defun --el-load-file-silent
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

(defun --el-add-subdirs-to-loadpath
    (parent)
  "Add all visible (non-hidden) subdirectories of PARENT to `load-path`.
Ignores any directories in `--el-excluded-dirs`."
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
  (--el-add-subdirs-to-loadpath root))

;; (add-to-list 'load-path "/home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/")

;; Emacs LLM package
;; ----------------------------------------

;; LLM
(require 'emacs-llm-variables)
(require 'emacs-llm-display)
(require 'emacs-llm-templates)
(require 'emacs-llm-call)
(require 'emacs-llm-switch)

;; Utils
(require 'emacs-llm-spinner)
(require 'emacs-llm-scroll)
(require 'emacs-llm-code-navigation)
(require 'emacs-llm-history)

;; Dired
(require 'emacs-llm-filter)
(require 'emacs-llm-dired)

;; Main
(require 'emacs-llm-run)
(require 'emacs-llm-keybindings)

(provide 'emacs-llm)

(when
    (not load-file-name)
  (message "emacs-llm.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))