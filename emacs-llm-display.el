;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 22:02:08>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-display.el

(defun --el-prepare-llm-buffer
    (prompt provider model &optional template-name)
  "Prepare the LLM buffer for displaying a response.
PROMPT is the user input.
PROVIDER is the name of the LLM provider.
MODEL is the model name.
Optional TEMPLATE-NAME is the name of the template used, if any."
  (let
      ((buffer-name
        (get-buffer-create --el-buffer-name)))
    (with-current-buffer buffer-name
      (unless
          (derived-mode-p 'markdown-mode)
        (markdown-mode))
      (goto-char
       (point-max))
      (unless
          (=
           (point-min)
           (point-max))
        (insert "\n\n"))
      (insert --el-separator)
      (insert
       (format "\n\n > %s [Template: %s]" model template-name))
      (insert
       (format "\n\n > %s\n\n" prompt))
      (display-buffer
       (current-buffer)))
    buffer-name))

(provide 'emacs-llm-display)

(when
    (not load-file-name)
  (message "emacs-llm-display.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))