;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-03-01 21:09:09>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-run.el

(require 'emacs-llm-dired)
(require 'emacs-llm-process)

;;;###autoload

(defun el-run
    (&optional prompt template-name)
  "Run El command on selected region, dired files or prompt.
If a region is selected, use that text as the prompt.
If in dired-mode with marked files, concatenate their contents.
Otherwise, prompt the user to enter a prompt.
The response will be displayed in the *El* buffer."
  (interactive)
  (--el-history-load)
  (let*
      ((prompt
        (or prompt
            (cond
             ((use-region-p)
              (message "Using region text as prompt")
              (buffer-substring-no-properties
               (region-beginning)
               (region-end)))
             ((eq major-mode 'dired-mode)
              (message "Using dired contents as prompt")
              (--el-dired-get-contents))
             (t
              (message "Prompting user for input")
              (read-string "Enter prompt: " "")))))
       (template-name
        (or template-name
            (progn
              (--el-template-select)))))
    ;; Use the abstraction
    (el-llm-call prompt template-name --el-default-provider)))

(provide 'emacs-llm-run)

(when
    (not load-file-name)
  (message "emacs-llm-run.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))