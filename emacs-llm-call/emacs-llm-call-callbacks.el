;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-03-01 17:08:11>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-call/emacs-llm-call-callbacks.el

(defun my-simple-callback
    (text &optional status proc)
  "Handle LLM response TEXT.
STATUS may be 'finished or 'error when process completes.
PROC is the process object."
  (cond
   ;; Handle text chunk - only when text is non-nil
   ((and text
         (not
          (string-empty-p text)))
    (message "%s" text))
   ;; Handle process completion
   ((eq status 'finished)
    (message "LLM response complete"))
   ;; Handle errors
   ((eq status 'error)
    (message "Error: %s"
             (or proc "Unknown error")))))

;; they work

;; ;; Example usage:
;; (el-switch "openai" "o3-mini-low")
;; (emacs-llm-call-core "Hi there" "anthropic" "claude-3-7-sonnet-20250219" nil #'my-simple-callback)
;; (emacs-llm-call-core "Hi there" "openai" "o3-mini-low" nil #'my-simple-callback)
;; (emacs-llm-call-core "Hi there" "deepseek" "deepseek-chat" nil #'my-simple-callback)
;; (emacs-llm-call-core "Hi there" "google" "gemini-2.0-flash-thinking-exp-01-21" nil #'my-simple-callback)

(defun insert-to-buffer-callback
    (text &optional status proc)
  "Insert LLM response TEXT into a specific buffer.
STATUS may be 'finished or 'error when process completes.
PROC is the process object."
  (let
      ((buffer-name "*LLM Output*"))
    (cond
     ;; Handle text chunk
     (text
      (with-current-buffer
          (get-buffer-create buffer-name)
        (goto-char
         (point-max))
        (insert text)
        (goto-char
         (point-max))))

     ;; Handle process completion
     ((eq status 'finished)
      (with-current-buffer
          (get-buffer-create buffer-name)
        (insert "\n\n--- Response Complete ---\n\n")))

     ;; Handle errors
     ((eq status 'error)
      (with-current-buffer
          (get-buffer-create buffer-name)
        (insert
         (format "\n\nError: %s\n" proc)))))))

;; ;; Example usage:
;; (emacs-llm-call-core "Tell me about Emacs" "anthropic" nil nil #'insert-to-buffer-callback)

(defun markdown-processing-callback
    (text &optional status proc)
  "Process LLM response TEXT as markdown.
STATUS may be 'finished or 'error when process completes.
PROC is the process object."
  (let
      ((buffer-name "*LLM Markdown*"))
    (cond
     ;; Handle text chunk
     (text
      (with-current-buffer
          (get-buffer-create buffer-name)
        (let
            ((markdown-mode-loaded
              (require 'markdown-mode nil t)))
          ;; Try to enable markdown mode if available
          (when
              (and markdown-mode-loaded
                   (eq major-mode 'fundamental-mode))
            (markdown-mode))
          ;; Insert text
          (goto-char
           (point-max))
          (insert text)
          (goto-char
           (point-max)))))

     ;; Handle process completion
     ((eq status 'finished)
      (with-current-buffer
          (get-buffer-create buffer-name)
        (when
            (fboundp 'markdown-fontify-buffer-wiki-links)
          (markdown-fontify-buffer-wiki-links))
        (display-buffer buffer-name)))

     ;; Handle errors
     ((eq status 'error)
      (message "Error in LLM response: %s" proc)))))

;; ;; Example usage:
;; (emacs-llm-call-core "Explain recursion with code examples" "anthropic" nil nil #'markdown-processing-callback)

(defun org-capture-callback
    (text &optional status proc)
  "Capture LLM response TEXT into an org file.
STATUS may be 'finished or 'error when process completes.
PROC is the process object."
  (cond
   ;; Handle text chunks by storing them
   (text
    ;; Append to accumulated content in process properties
    (when proc
      (let
          ((content
            (or
             (process-get proc 'content)
             "")))
        (process-put proc 'content
                     (concat content text)))))

   ;; On completion, save to org file
   ((eq status 'finished)
    (let*
        ((content
          (process-get proc 'content))
         (prompt
          (process-get proc 'prompt))
         (provider
          (process-get proc 'provider))
         (org-file "~/llm-responses.org"))

      ;; Create the org entry
      (with-temp-buffer
        (insert
         (format "* LLM Response: %s\n"
                 (format-time-string "%Y-%m-%d %H:%M")))
        (insert
         (format "Provider: %s\n\n" provider))
        (insert "** Prompt\n\n")
        (insert prompt)
        (insert "\n\n** Response\n\n")
        (insert content)
        (insert "\n")

        ;; Append to org file
        (append-to-file
         (point-min)
         (point-max)
         org-file))

      (message "Response saved to %s" org-file)))

   ;; Handle errors
   ((eq status 'error)
    (message "Error occurred: %s" proc))))

;; ;; Example usage:
;; (emacs-llm-call-core "Compare functional and OOP paradigms" "anthropic" nil nil #'org-capture-callback)

(defun data-extraction-callback
    (text &optional status proc)
  "Extract structured data from LLM response TEXT.
STATUS may be 'finished or 'error when process completes.
PROC is the process object."
  (cond
   ;; Store text chunks
   (text
    (when proc
      (let
          ((content
            (or
             (process-get proc 'content)
             "")))
        (process-put proc 'content
                     (concat content text)))))

   ;; Process the complete response when finished
   ((eq status 'finished)
    (let*
        ((content
          (process-get proc 'content))
         (json-object-type 'alist)
         (json-array-type 'list)
         (result nil))

      ;; Look for JSON data in the response
      (when
          (string-match "```json\\(.*?\\)```" content)
        (let
            ((json-text
              (match-string 1 content)))
          (condition-case err
              (setq result
                    (json-read-from-string json-text))
            (error
             (message "Failed to parse JSON: %s" err)))))

      ;; Do something with the extracted data
      (when result
        (message "Extracted data: %s" result))))

   ;; Handle errors
   ((eq status 'error)
    (message "Error in LLM response: %s" proc))))

;; ;; Example usage:
;; (emacs-llm-call-core
;;  "Return a JSON object with fields: name, age, occupation for a fictional person"
;;  "anthropic" nil nil #'data-extraction-callback)

(defun cli-style-callback
    (text &optional status proc)
  "Display LLM responses in a command-line style.
STATUS may be 'finished or 'error when process completes.
PROC is the process object."
  (cond
   ;; Print text as it arrives (no newlines)
   (text
    (princ text))

   ;; Print newline at end
   ((eq status 'finished)
    (princ "\n"))

   ;; Show errors
   ((eq status 'error)
    (princ
     (format "\nError: %s\n" proc)))))

;; Example usage from an interactive function:

(defun ask-llm-cli
    ()
  "Ask LLM a question and display response in command-line style."
  (interactive)
  (let*
      ((query
        (read-string "Ask: "))
       (proc
        (emacs-llm-call-core query "anthropic" nil nil #'cli-style-callback)))
    (message "Processing...")))

(provide 'emacs-llm-call-callbacks)

(when
    (not load-file-name)
  (message "emacs-llm-call-callbacks.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))