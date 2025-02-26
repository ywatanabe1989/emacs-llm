;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 21:20:38>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-run.el

(require 'emacs-llm-dired)
(require 'emacs-llm-process)

;;;###autoload
(defun el-run
    (&optional prompt)
  "Run El command on selected region, dired files or prompt.
If a region is selected, use that text as the prompt.
If in dired-mode with marked files, concatenate their contents.
Otherwise, prompt the user to enter a prompt.
The response will be displayed in the *El* buffer."
  (interactive)
  (--el-load-history)
  (let*
      ((prompt
        (or prompt
            (cond
             ((use-region-p)
              (buffer-substring-no-properties
               (region-beginning)
               (region-end)))
             ((eq major-mode 'dired-mode)
              (--el-dired-get-contents))
             (t
              (read-string "Enter prompt: " "")))))
       ;; Still get template, but don't modify prompt here
       (template-name
        (--el-select-template)))
    ;; Template application will happen inside the provider stream function
    (message "DEBUG: Prompt is: %s" prompt)
    (message "DEBUG: Provider is: %s" --el-provider)
    ;; Use the abstraction
    (el-llm-call-stream prompt --el-provider template-name)))

(provide 'emacs-llm-run)

(when
    (not load-file-name)
  (message "emacs-llm-run.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))