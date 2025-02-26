;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-27 09:36:52>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-path-and-load.el

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

(provide 'emacs-llm-path-and-load)

(when
    (not load-file-name)
  (message "emacs-llm-path-and-load.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))