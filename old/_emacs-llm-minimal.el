;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 12:24:24>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-minimal.el

(require 'markdown-mode)
(require 'cl-lib)
(require 'json)

(defgroup emacs-llm nil
  "Customization group for Minimal LLM."
  :group 'applications
  :prefix "emacs-llm-")

(defcustom emacs-llm-home-dir
  (file-name-directory
   (or load-file-name buffer-file-name))
  "The home directory of emacs-llm.el."
  :type 'string)

(defcustom emacs-llm-buffer "*MinimalLLM*"
  "Name of the buffer for displaying LLM outputs."
  :type 'string)

(defcustom emacs-llm-api-key
  (getenv "OPENAI_API_KEY")
  "API key for accessing the LLM API."
  :type 'string)

(defcustom emacs-llm-provider "openai"
  "Default LLM provider to use."
  :type
  '(choice
    (const :tag "OpenAI" "openai")
    (const :tag "Anthropic" "anthropic")
    (const :tag "Google" "google")
    (const :tag "DeepSeek" "deepseek"))
  :group 'emacs-llm)

(defcustom emacs-llm-model "gpt-3.5-turbo"
  "Default model to use for LLM interactions."
  :type 'string
  :group 'emacs-llm)

(defcustom emacs-llm-temperature 0.7
  "Sampling temperature for the LLM response."
  :type 'float
  :group 'emacs-llm)

(defcustom emacs-llm-max-tokens 2000
  "Maximum number of tokens for the LLM response."
  :type 'integer
  :group 'emacs-llm)

(defvar emacs-llm-history nil
  "List to keep track of conversation history.")

(defvar emacs-llm-process nil
  "Process object for the LLM process.")

(defconst emacs-llm-separator
  "--------------------------------------------------------------------------------"
  "Separator for LLM outputs in buffer.")

(defvar emacs-llm-spinner-frames
  '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")
  "Frames for the spinner animation.")

(defvar emacs-llm-spinner-timer nil
  "Timer for the spinner animation.")

(defvar emacs-llm-spinner-index 0
  "Current index in the spinner animation.")

(defvar emacs-llm-spinner-marker nil
  "Marker for spinner position.")

;; Model maps for each provider
(defvar emacs-llm-openai-models
  '("gpt-4o" "gpt-4-turbo" "gpt-3.5-turbo" "gpt-4" "o1-mini" "o1" "o3-mini")
  "Available OpenAI models.")

(defvar emacs-llm-anthropic-models
  '("claude-3-7-sonnet-20250219" "claude-3-5-sonnet-20241022" "claude-3-5-haiku-20241022")
  "Available Anthropic Claude models.")

(defvar emacs-llm-google-models
  '("gemini-2.0-flash" "gemini-2.0-pro" "gemini-1.5-pro")
  "Available Google Gemini models.")

(defvar emacs-llm-deepseek-models
  '("deepseek-chat" "deepseek-coder" "deepseek-reasoner")
  "Available DeepSeek models.")

;;;###autoload
(defun emacs-llm-switch
    (provider)
  "Switch the LLM provider and select a model."
  (interactive
   (list
    (completing-read "Select LLM provider: "
                     '("anthropic" "google" "deepseek" "openai")
                     nil t)))
  (setq emacs-llm-provider provider)

  (let*
      ((provider-models
        (cond
         ((string= provider "anthropic")
          emacs-llm-anthropic-models)
         ((string= provider "google")
          emacs-llm-google-models)
         ((string= provider "deepseek")
          emacs-llm-deepseek-models)
         ((string= provider "openai")
          emacs-llm-openai-models)))
       (selected-model
        (completing-read "Select model: " provider-models nil t)))

    (setq emacs-llm-api-key
          (getenv
           (format "%s_API_KEY"
                   (upcase provider))))

    (setq emacs-llm-model selected-model)
    (message "Switched to %s using model: %s" provider selected-model)))

(defun emacs-llm-start-spinner
    ()
  "Start the spinner animation in the MinimalLLM buffer."
  (when
      (and
       (get-buffer emacs-llm-buffer)
       (not emacs-llm-spinner-timer))
    (with-current-buffer emacs-llm-buffer
      (goto-char
       (point-max))
      (setq emacs-llm-spinner-marker
            (point-marker))
      (insert " ")
      (setq emacs-llm-spinner-timer
            (run-with-timer 0 0.1
                            (lambda
                              ()
                              (when
                                  (buffer-live-p
                                   (marker-buffer emacs-llm-spinner-marker))
                                (with-current-buffer
                                    (marker-buffer emacs-llm-spinner-marker)
                                  (let
                                      ((inhibit-read-only t))
                                    (save-excursion
                                      (goto-char emacs-llm-spinner-marker)
                                      (delete-char 1)
                                      (insert
                                       (propertize
                                        (nth emacs-llm-spinner-index emacs-llm-spinner-frames)
                                        'face
                                        '(:foreground "blue")))
                                      (setq emacs-llm-spinner-index
                                            (mod
                                             (1+ emacs-llm-spinner-index)
                                             (length emacs-llm-spinner-frames)))))))))))))

(defun emacs-llm-stop-spinner
    ()
  "Stop the spinner animation."
  (when emacs-llm-spinner-timer
    (cancel-timer emacs-llm-spinner-timer)
    (setq emacs-llm-spinner-timer nil)
    (when
        (and emacs-llm-spinner-marker
             (marker-buffer emacs-llm-spinner-marker))
      (with-current-buffer
          (marker-buffer emacs-llm-spinner-marker)
        (let
            ((inhibit-read-only t))
          (save-excursion
            (goto-char emacs-llm-spinner-marker)
            (delete-char 1)
            (goto-char
             (point-max))
            (insert "\n\n")
            (insert emacs-llm-separator)
            (insert "\n\n")))))))

(defun emacs-llm-process-sentinel
    (process event)
  "Handle when PROCESS changes state, as described by EVENT."
  (cond
   ((string-match-p "finished\\|exited" event)
    (emacs-llm-stop-spinner)
    (message "LLM process finished."))
   ((string-match-p "error" event)
    (emacs-llm-stop-spinner)
    (message "LLM process encountered an error: %s" event))
   (t
    (message "LLM process: %s" event))))

(defun emacs-llm-construct-openai-payload
    (prompt)
  "Construct the JSON payload for OpenAI API with PROMPT."
  (json-encode
   `(("model" . ,emacs-llm-model)
     ("messages" .
      [,(append
         `(("role" . "user")
           ("content" . ,prompt)))])
     ("temperature" . ,emacs-llm-temperature)
     ("max_tokens" . ,emacs-llm-max-tokens)
     ("stream" . t))))

(defun emacs-llm-construct-anthropic-payload
    (prompt)
  "Construct the JSON payload for Anthropic API with PROMPT."
  (json-encode
   `(("model" . ,emacs-llm-model)
     ("max_tokens" . ,emacs-llm-max-tokens)
     ("stream" . t)
     ("temperature" . ,emacs-llm-temperature)
     ("messages" .
      [,(append
         `(("role" . "user")
           ("content" . ,prompt)))]))))

(defun emacs-llm-send-request-stream
    (prompt)
  "Send PROMPT to the selected LLM provider via streaming API."
  (let*
      ((buffer-name
        (get-buffer-create emacs-llm-buffer))
       (temp-buffer
        (generate-new-buffer " *emacs-llm-temp*"))
       (url
        (cond
         ((string= emacs-llm-provider "openai")
          "https://api.openai.com/v1/chat/completions")
         ((string= emacs-llm-provider "anthropic")
          "https://api.anthropic.com/v1/messages")
         ((string= emacs-llm-provider "google")
          "https://generativelanguage.googleapis.com/v1beta/models/")
         ((string= emacs-llm-provider "deepseek")
          "https://api.deepseek.com/v1/chat/completions")))
       (payload
        (cond
         ((string= emacs-llm-provider "openai")
          (emacs-llm-construct-openai-payload prompt))
         ((string= emacs-llm-provider "anthropic")
          (emacs-llm-construct-anthropic-payload prompt))))
       (headers
        (cond
         ((string= emacs-llm-provider "openai")
          (list "-H" "Content-Type: application/json"
                "-H"
                (concat "Authorization: Bearer " emacs-llm-api-key)))
         ((string= emacs-llm-provider "anthropic")
          (list "-H" "Content-Type: application/json"
                "-H" "anthropic-version: 2023-06-01"
                "-H"
                (concat "x-api-key: " emacs-llm-api-key)))
         ((string= emacs-llm-provider "google")
          (list "-H" "Content-Type: application/json"))
         ((string= emacs-llm-provider "deepseek")
          (list "-H" "Content-Type: application/json"
                "-H"
                (concat "Authorization: Bearer " emacs-llm-api-key)))))
       (args
        (append
         (list "-N" url)
         headers
         (list "-d" payload)))
       (proc
        (apply #'start-process
               (format "emacs-llm-%s-stream" emacs-llm-provider)
               temp-buffer
               "curl"
               args)))

    ;; Set up display buffer
    (with-current-buffer buffer-name
      (unless
          (derived-mode-p 'markdown-mode)
        (markdown-mode))
      (read-only-mode -1)
      (goto-char
       (point-max))
      (unless
          (=
           (point-min)
           (point-max))
        (insert "\n\n"))
      (insert emacs-llm-separator)
      (insert "\n\n")
      (insert
       (format "Provider: %s | Model: %s\n\n"
               (upcase emacs-llm-provider)
               emacs-llm-model))
      (insert
       (format "Prompt: %s\n\n" prompt))
      (insert "Response: ")
      (read-only-mode 1)
      (display-buffer
       (current-buffer)))

    ;; Set up process filter based on provider
    (cond
     ((string= emacs-llm-provider "openai")
      (set-process-filter proc #'emacs-llm-openai-filter))
     ((string= emacs-llm-provider "anthropic")
      (set-process-filter proc #'emacs-llm-anthropic-filter))
     ((string= emacs-llm-provider "google")
      (set-process-filter proc #'emacs-llm-google-filter))
     ((string= emacs-llm-provider "deepseek")
      (set-process-filter proc #'emacs-llm-deepseek-filter)))

    ;; Set process sentinel and start spinner
    (set-process-sentinel proc #'emacs-llm-process-sentinel)
    (emacs-llm-start-spinner)

    proc))

(defun emacs-llm-parse-openai-chunk
    (chunk)
  "Parse OpenAI JSON CHUNK and return content."
  (when
      (and chunk
           (not
            (string= chunk "[DONE]")))
    (let*
        ((json-object-type 'alist)
         (json-array-type 'vector)
         (json-key-type 'symbol)
         (data
          (condition-case err
              (json-read-from-string chunk)
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
                           (aref choices 0)))))
          (alist-get 'content delta))))))

(defun emacs-llm-parse-anthropic-chunk
    (chunk)
  "Parse Anthropic JSON CHUNK and return text delta."
  (when
      (and chunk
           (not
            (string= chunk "[DONE]")))
    (let*
        ((json-object-type 'alist)
         (json-array-type 'list)
         (data
          (condition-case err
              (json-read-from-string chunk)
            (error
             (message "Error parsing JSON chunk: %s"
                      (error-message-string err))
             nil))))
      (when data
        (let
            ((type
              (alist-get 'type data)))
          (cond
           ((string= type "content_block_delta")
            (let
                ((delta
                  (alist-get 'delta data)))
              (or
               (alist-get 'text delta)
               (alist-get 'thinking delta))))
           (t nil)))))))

(defun emacs-llm-process-chunk
    (proc chunk parse-func)
  "Process CHUNK from PROC using PARSE-FUNC to extract content."
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
            (let
                ((text
                  (funcall parse-func jsonstr)))
              (when text
                (with-current-buffer emacs-llm-buffer
                  (save-excursion
                    (let
                        ((buffer-read-only nil))
                      (goto-char
                       (point-max))
                      (insert text))))))))))))

(defun emacs-llm-openai-filter
    (proc chunk)
  "Filter for OpenAI stream PROC processing CHUNK."
  (emacs-llm-process-chunk proc chunk #'emacs-llm-parse-openai-chunk))

(defun emacs-llm-anthropic-filter
    (proc chunk)
  "Filter for Anthropic stream PROC processing CHUNK."
  (emacs-llm-process-chunk proc chunk #'emacs-llm-parse-anthropic-chunk))

(defun emacs-llm-google-filter
    (proc chunk)
  "Filter for Google stream PROC processing CHUNK."
  (message "Google API not fully implemented yet"))

(defun emacs-llm-deepseek-filter
    (proc chunk)
  "Filter for DeepSeek stream PROC processing CHUNK."
  (message "DeepSeek API not fully implemented yet"))

;;;###autoload
(defun emacs-llm-on-region
    ()
  "Run LLM on selected region or prompt for input.
If a region is selected, use that text as the prompt.
Otherwise, prompt the user to enter text."
  (interactive)
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
    (emacs-llm-send-request-stream prompt)))

;;;###autoload
(defun emacs-llm-copy-last-response
    ()
  "Copy the last response to the kill ring."
  (interactive)
  (with-current-buffer emacs-llm-buffer
    (save-excursion
      (goto-char
       (point-max))
      (let
          ((end
            (point)))
        (re-search-backward "Response: " nil t)
        (forward-line 1)
        (let
            ((start
              (point)))
          (kill-ring-save start end)
          (message "Copied response to kill ring"))))))

(provide 'emacs-llm-minimal)

(when
    (not load-file-name)
  (message "emacs-llm-minimal.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))