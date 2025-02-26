;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 13:01:39>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-display.el

(defun el--display-response
    (content)
  "Display the AI CONTENT in the LLM buffer."
  (with-current-buffer
      (get-buffer-create el-buffer-name)
    (goto-char
     (point-max))
    (unless
        (=
         (point-min)
         (point-max))
      (insert "\n\n"))
    (insert el-separator)
    (insert "\n\n")
    (insert
     (propertize "AI:" 'face 'bold))
    (insert "\n\n")
    (insert content)
    (markdown-mode)
    (goto-char
     (point-max))
    (display-buffer
     (current-buffer))))

(provide 'emacs-llm-display)

(when
    (not load-file-name)
  (message "emacs-llm-display.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))