;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 14:57:35>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-display.el

;; (defun --el--display-response
;;     (content)
;;   "Display the AI CONTENT in the LLM buffer."
;;   (with-current-buffer
;;       (get-buffer-create --el-buffer-name)
;;     (goto-char
;;      (point-max))
;;     (unless
;;         (=
;;          (point-min)
;;          (point-max))
;;       (insert "\n\n"))
;;     (insert --el-separator)
;;     (insert "\n\n")
;;     (insert
;;      (propertize "AI:" 'face 'bold))
;;     (insert "\n\n")
;;     (insert content)
;;     (markdown-mode)
;;     (goto-char
;;      (point-max))
;;     (display-buffer
;;      (current-buffer))))

(defun --el--display-with-context
    (prompt template response provider model)
  "Display PROMPT, TEMPLATE, RESPONSE with PROVIDER and MODEL info in the LLM buffer."
  (with-current-buffer
      (get-buffer-create --el-buffer-name)
    (goto-char
     (point-max))
    (unless
        (=
         (point-min)
         (point-max))
      (insert "\n\n"))
    (insert --el-separator)
    (insert "\n\n")
    (insert
     (format "Provider: %s | Model: %s\n\n" provider model))

    ;; Display the prompt with template info if available
    (insert
     (propertize "User:" 'face 'bold))
    (when template
      (insert
       (format " [Template: %s]" template)))
    (insert "\n\n")
    (insert prompt)
    (insert "\n\n")

    ;; Display the response
    (insert
     (propertize "AI:" 'face 'bold))
    (insert "\n\n")
    (insert response)

    (markdown-mode)
    (goto-char
     (point-max))
    (display-buffer
     (current-buffer))))

(defun --el--prepare-llm-buffer
    (prompt provider model &optional template)
  "Prepare the LLM buffer for displaying a response.
PROMPT is the user input.
PROVIDER is the name of the LLM provider.
MODEL is the model name.
Optional TEMPLATE is the name of the template used, if any."
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
      (insert "\n\n")
      (insert
       (format "Provider: %s | Model: %s\n\n" provider model))

      ;; Display user prompt with template info
      (insert
       (propertize "User:" 'face 'bold))
      (when template
        (insert
         (format " [Template: %s]" template)))
      (insert "\n\n")
      (insert prompt)
      (insert "\n\n")

      ;; Display response heading
      (insert
       (propertize "AI:" 'face 'bold))
      (insert "\n\n")

      (display-buffer
       (current-buffer)))
    buffer-name))

(provide 'emacs-llm-display)

(when
    (not load-file-name)
  (message "emacs-llm-display.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))