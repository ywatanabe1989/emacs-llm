;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 12:09:46>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm.el

(require 'request)
(require 'json)
(require 'markdown-mode)
(require 'cl-lib)

(defgroup emacs-llm nil
  "Customization group for emacs-llm.el."
  :group 'applications
  :prefix "el-")

(defcustom el-api-key
  (getenv "OPENAI_API_KEY")
  "API key for accessing the OpenAI API."
  :type 'string
  :group 'emacs-llm)

(defcustom el-model "gpt-3.5-turbo"
  "Default model to use for AI interactions."
  :type 'string
  :group 'emacs-llm)

(defcustom el-max-tokens 2000
  "Maximum number of tokens for the AI response."
  :type 'integer
  :group 'emacs-llm)

(defcustom el-temperature 0.7
  "Sampling temperature for the AI response."
  :type 'float
  :group 'emacs-llm)

(defcustom el-history-file
  (expand-file-name "el-history.json" user-emacs-directory)
  "File path to save conversation history."
  :type 'string
  :group 'emacs-llm)

(defcustom el-templates-dir
  (expand-file-name "templates/"
                    (file-name-nondirectory
                     (or load-file-name buffer-file-name)))
  "Directory containing prompt templates."
  :type 'string
  :group 'emacs-llm)

(defcustom el-buffer-name "*Emacs-LLM*"
  "Name of the buffer for displaying AI interactions."
  :type 'string
  :group 'emacs-llm)

(defvar el-history
  '()
  "List to keep track of conversation history.")

(defun el--load-history
    ()
  "Load conversation history from `el-history-file`."
  (when
      (file-exists-p el-history-file)
    (with-temp-buffer
      (insert-file-contents el-history-file)
      (setq el-history
            (json-read-from-string
             (buffer-string))))))

(defun el--save-history
    ()
  "Save conversation history to `el-history-file`."
  (with-temp-file el-history-file
    (insert
     (json-encode el-history))))

(defun el--append-to-history
    (role content)
  "Append a message with ROLE and CONTENT to the conversation history."
  (let
      ((entry
        `(("role" . ,role)
          ("content" . ,content))))
    (setq el-history
          (append el-history
                  (list entry)))
    (el--save-history)))

(defun el--clear-history
    ()
  "Clear the conversation history."
  (interactive)
  (setq el-history
        '())
  (el--save-history)
  (message "Emacs-LLM conversation history cleared."))

(defun el--display-response
    (response)
  "Display the AI RESPONSE in `el-buffer-name` buffer."
  (with-current-buffer
      (get-buffer-create el-buffer-name)
    (read-only-mode -1)
    (goto-char
     (point-max))
    (insert "\n\n")
    (insert
     (propertize "AI:" 'face 'bold))
    (insert "\n\n")
    (insert response)
    (markdown-view-mode)
    (goto-char
     (point-max))
    (read-only-mode 1)
    (display-buffer
     (current-buffer))))

(defun el--construct-request-data
    (prompt)
  "Construct the JSON data for the API request with PROMPT."
  (let
      ((messages
        (append el-history
                `((("role" . "user")
                   ("content" . ,prompt))))))
    (json-encode
     `(("model" . ,el-model)
       ("messages" . ,messages)
       ("temperature" . ,el-temperature)
       ("max_tokens" . ,el-max-tokens)))))

(defun el--handle-api-response
    (response)
  "Handle the API RESPONSE and display it."
  (let*
      ((choices
        (alist-get 'choices response))
       (first-choice
        (elt choices 0))
       (msg
        (alist-get 'message first-choice))
       (content
        (alist-get 'content msg)))
    (when content
      (el--append-to-history "assistant" content)
      (el--display-response content))))

(defun el--handle-api-error
    (error-thrown)
  "Handle API errors represented by ERROR-THROWN."
  (message "Emacs-LLM API Error: %s" error-thrown))

(defun el--send-request
    (prompt)
  "Send the PROMPT to the AI API."
  (let
      ((url "https://api.openai.com/v1/chat/completions")
       (data
        (el--construct-request-data prompt)))
    (request
      url
      :type "POST"
      :headers
      `(("Content-Type" . "application/json")
        ("Authorization" . ,(concat "Bearer " el-api-key)))
      :data data
      :parser 'json-read
      :success
      (cl-function
       (lambda
         (&key data &allow-other-keys)
         (el--handle-api-response data)))
      :error
      (cl-function
       (lambda
         (&key error-thrown &allow-other-keys)
         (el--handle-api-error error-thrown))))))

(defun el--send-request-stream
    (prompt)
  "Send the PROMPT to the AI API using streaming."
  (interactive)
  (let*
      ((url "https://api.openai.com/v1/chat/completions")
       (buffer-name
        (get-buffer-create el-buffer-name))
       (payload
        (json-encode
         `(("model" . ,el-model)
           ("messages" . ,(append el-history
                                  `((("role" . "user")
                                     ("content" . ,prompt)))))
           ("stream" . t)
           ("temperature" . ,el-temperature)
           ("max_tokens" . ,el-max-tokens))))
       (args
        (list "-N"
              url
              "-H" "Content-Type: application/json"
              "-H"
              (concat "Authorization: Bearer " el-api-key)
              "-d" payload))
       (proc
        (apply #'start-process "el-stream-curl" "*el-stream-curl*" "curl" args))
       (content ""))

    ;; Set up display buffer
    (with-current-buffer buffer-name
      (read-only-mode -1)
      (goto-char
       (point-max))
      (insert "\n\n")
      (insert
       (propertize "AI:" 'face 'bold))
      (insert "\n\n")
      (read-only-mode 1)
      (display-buffer
       (current-buffer)))

    ;; Filter function to process streaming output
    (set-process-filter
     proc
     (lambda
       (proc chunk)
       (let*
           ((partial
             (or
              (process-get proc 'partial-data)
              ""))
            (combined
             (concat partial chunk))
            (lines
             (split-string combined "\n"))
            (incomplete
             (if
                 (string-suffix-p "\n" combined)
                 ""
               (car
                (last lines)))))
         (unless
             (string= incomplete "")
           (setq lines
                 (butlast lines)))
         (process-put proc 'partial-data incomplete)

         (dolist
             (line lines)
           (when
               (string-prefix-p "data:" line)
             (let
                 ((jsonstr
                   (string-trim
                    (substring line
                               (if
                                   (string-prefix-p "data: " line)
                                   6 5)))))
               (unless
                   (string= jsonstr "[DONE]")
                 (let*
                     ((json-object-type 'alist)
                      (json-array-type 'vector)
                      (json-key-type 'symbol)
                      (data
                       (condition-case err
                           (json-read-from-string jsonstr)
                         (error
                          (message "Error parsing JSON: %s"
                                   (error-message-string err))
                          nil))))
                   (when data
                     (let*
                         ((choices
                           (alist-get 'choices data))
                          (delta
                           (when
                               (and choices
                                    (>
                                     (length choices)
                                     0))
                             (alist-get 'delta
                                        (aref choices 0))))
                          (text
                           (alist-get 'content delta)))
                       (when text
                         (setq content
                               (concat content text))
                         (with-current-buffer buffer-name
                           (let
                               ((buffer-read-only nil))
                             (goto-char
                              (point-max))
                             (insert text))))))))))))))

    ;; Process sentinel to handle completion
    (set-process-sentinel
     proc
     (lambda
       (proc event)
       (when
           (string-match "finished" event)
         (el--append-to-history "assistant" content)
         (message "OpenAI streaming completed."))))

    (el--append-to-history "user" prompt)))

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