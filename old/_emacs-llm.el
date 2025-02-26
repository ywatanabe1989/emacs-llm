;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 12:11:53>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm.el

(require 'request)
(require 'json)
(require 'markdown-mode)
(require 'cl-lib)
(require 'emacs-llm-core)
(require 'emacs-llm-api)
(require 'emacs-llm-stream)

;;;###autoload
(defun el-send-region-or-prompt
    ()
  "Send the selected region or prompt to the AI and display the response."
  (interactive)
  (el--load-history)
  (let
      ((prompt
        (if
            (use-region-p)
            (prog1
                (buffer-substring-no-properties
                 (region-beginning)
                 (region-end))
              (deactivate-mark))
          (read-string "Enter prompt: "))))
    (el--append-to-history "user" prompt)
    (el--send-request prompt)))

;;;###autoload
(defun el-send-region-or-prompt-stream
    ()
  "Send the selected region or prompt to the AI and display the response."
  (interactive)
  (el--load-history)
  (let
      ((prompt
        (if
            (use-region-p)
            (prog1
                (buffer-substring-no-properties
                 (region-beginning)
                 (region-end))
              (deactivate-mark))
          (read-string "Enter prompt: "))))
    (el--append-to-history "user" prompt)
    (el--send-request-stream prompt)))

;;;###autoload
(defun el-show-history
    ()
  "Display the conversation history in a buffer."
  (interactive)
  (el--load-history)
  (let
      ((buffer
        (get-buffer-create "*Emacs-LLM History*")))
    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer)
      (dolist
          (entry el-history)
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
      (read-only-mode 1)
      (view-mode 1)
      (display-buffer
       (current-buffer)))))

;;;###autoload
(defun el-clear-history
    ()
  "Clear the conversation history."
  (interactive)
  (when
      (yes-or-no-p "Are you sure you want to clear the conversation history?")
    (el--clear-history)))

;;;###autoload
(defun el-set-api-key
    ()
  "Prompt the user to enter the API key and set `el-api-key`."
  (interactive)
  (setq el-api-key
        (read-string "Enter OpenAI API key: "))
  (message "API key set."))

;;;###autoload
(defun el-set-model
    ()
  "Prompt the user to select the AI model."
  (interactive)
  (let
      ((models
        '("gpt-3.5-turbo" "gpt-4")))
    (setq el-model
          (completing-read "Select model: " models nil t)))
  (message "Model set to %s." el-model))

;;;###autoload
(defun el-set-max-tokens
    ()
  "Prompt the user to set the maximum number of tokens."
  (interactive)
  (setq el-max-tokens
        (read-number "Enter max tokens: "))
  (message "Max tokens set to %d." el-max-tokens))

;;;###autoload
(defun el-set-temperature
    ()
  "Prompt the user to set the sampling temperature."
  (interactive)
  (setq el-temperature
        (read-number "Enter temperature (0.0 - 1.0): "))
  (message "Temperature set to %f." el-temperature))

;;;###autoload
(defun el-insert-template
    ()
  "Insert a prompt template into the current buffer."
  (interactive)
  (let
      ((templates
        (directory-files el-templates-dir nil "\\.md$")))
    (if templates
        (let*
            ((template
              (completing-read "Select template: " templates nil t))
             (template-path
              (expand-file-name template el-templates-dir)))
          (insert-file-contents template-path))
      (message "No templates found in %s" el-templates-dir))))

;;;###autoload
(defun el-copy-last-response
    ()
  "Copy the last AI response to the kill ring."
  (interactive)
  (let
      ((response
        (with-current-buffer el-buffer-name
          (save-excursion
            (goto-char
             (point-max))
            (re-search-backward
             (propertize "AI:" 'face 'bold))
            (forward-line 2)
            (buffer-substring-no-properties
             (point)
             (point-max))))))
    (when response
      (kill-new response)
      (message "Last AI response copied to kill ring."))))

(provide 'emacs-llm)
(when
    (not load-file-name)
  (message "emacs-llm.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))

(provide 'emacs-llm)

(when
    (not load-file-name)
  (message "emacs-llm.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))