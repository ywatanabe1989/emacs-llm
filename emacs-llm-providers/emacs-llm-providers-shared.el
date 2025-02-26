;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 18:47:43>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-providers/emacs-llm-providers-shared.el

(defun sanitize-prompt
    (prompt)
  (replace-regexp-in-string "[;\\\"'`$()]" "\\\\\\&" prompt))

(provide 'emacs-llm-providers-shared)

(when
    (not load-file-name)
  (message "emacs-llm-providers-shared.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))