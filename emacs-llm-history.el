;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 14:19:05>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-history.el

;;;; Helper Functions

(defun --el--load-history
    ()
  "Load conversation history from `--el-history-file`."
  (when
      (file-exists-p --el-history-file)
    (with-temp-buffer
      (insert-file-contents --el-history-file)
      (setq --el-history
            (json-read-from-string
             (buffer-string))))))

(defun --el--save-history
    ()
  "Save conversation history to `--el-history-file`."
  (with-temp-file --el-history-file
    (insert
     (json-encode --el-history))))

(defun --el--append-to-history
    (role content)
  "Append a message with ROLE and CONTENT to the conversation history."
  (let
      ((entry
        `(("role" . ,role)
          ("content" . ,content))))
    (setq --el-history
          (append --el-history
                  (list entry)))
    (--el--save-history)))

(defun --el--clear-history
    ()
  "Clear the conversation history."
  (interactive)
  (setq --el-history
        '())
  (--el--save-history)
  (message "Emacs-LLM conversation history cleared."))

;;;###autoload
(defun --el-show-history
    ()
  "Display the conversation history in a buffer."
  (interactive)
  (--el--load-history)
  (let
      ((buffer
        (get-buffer-create "*Emacs-LLM History*")))
    (with-current-buffer buffer
      (erase-buffer)
      (dolist
          (entry --el-history)
        (let
            ((role
              (alist-get 'role entry))
             (content
              (alist-get 'content entry)))
          (insert
           (propertize
            (concat role ":")
            'face 'bold))
          (insert "\n\n")
          (insert content)
          (insert "\n\n"
                  (make-string 80 ?-)
                  "\n\n")))
      (goto-char
       (point-min))
      (view-mode 1)
      (display-buffer
       (current-buffer)))))

(provide 'emacs-llm-history)

(when
    (not load-file-name)
  (message "emacs-llm-history.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))