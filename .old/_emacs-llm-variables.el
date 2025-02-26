;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 12:16:25>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/_emacs-llm-variables.el

;; OpenAI streaming implementation
(defun el--parse-openai-chunk
    (chunk)
  "Parse a JSON CHUNK from OpenAI API and return the content string."
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

;; Anthropic streaming implementation
(defun el--parse-anthropic-chunk
    (chunk)
  "Parse JSON CHUNK from Anthropic API and return text delta if event is content_block_delta."
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
               (alist-get 'thinking delta)
               (alist-get 'text delta))))
           (t nil)))))))

;; Generic streaming process filter
(defun el--stream-filter
    (proc chunk parse-func)
  "Process filter for LLM streams.
Accumulates CHUNK data and processes complete lines using PARSE-FUNC."
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
                (with-current-buffer
                    (process-get proc 'target-buffer)
                  (let
                      ((buffer-read-only nil))
                    (goto-char
                     (point-max))
                    (insert text)))
                (process-put proc 'content
                             (concat
                              (or
                               (process-get proc 'content)
                               "")
                              text))))))))))

;; OpenAI-specific stream filter
(defun el--openai-stream-filter
    (proc chunk)
  "Process filter for OpenAI stream."
  (el--stream-filter proc chunk #'el--parse-openai-chunk))

;; Anthropic-specific stream filter
(defun el--anthropic-stream-filter
    (proc chunk)
  "Process filter for Anthropic stream."
  (el--stream-filter proc chunk #'el--parse-anthropic-chunk))

;; Generic stream sentinel
(defun el--stream-sentinel
    (proc event)
  "Sentinel for LLM streams."
  (when
      (string-match "finished" event)
    (let*
        ((content
          (process-get proc 'content))
         (temp-buf
          (process-get proc 'temp-buffer)))
      (when content
        (el--append-to-history "assistant" content))
      (when
          (buffer-live-p temp-buf)
        (kill-buffer temp-buf))
      (message "LLM streaming completed."))))

;; Send request via OpenAI streaming API
(defun el--send-openai-stream
    (prompt)
  "Send PROMPT to OpenAI API via streaming."
  (let*
      ((buffer-name
        (get-buffer-create el-buffer-name))
       (temp-buffer
        (generate-new-buffer " *openai-temp-output*"))
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
              "https://api.openai.com/v1/chat/completions"
              "-H" "Content-Type: application/json"
              "-H"
              (concat "Authorization: Bearer " el-api-key)
              "-d" payload))
       (proc
        (apply #'start-process "el-openai-stream" temp-buffer "curl" args)))

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

    ;; Set up process properties and handlers
    (process-put proc 'target-buffer buffer-name)
    (process-put proc 'temp-buffer temp-buffer)
    (process-put proc 'content "")
    (set-process-filter proc #'el--openai-stream-filter)
    (set-process-sentinel proc #'el--stream-sentinel)

    (el--append-to-history "user" prompt)))

;; Send request via Anthropic streaming API
(defun el--send-anthropic-stream
    (prompt)
  "Send PROMPT to Anthropic API via streaming."
  (let*
      ((buffer-name
        (get-buffer-create el-buffer-name))
       (temp-buffer
        (generate-new-buffer " *anthropic-temp-output*"))
       (payload
        (json-encode
         `(("model" . ,el-anthropic-model)
           ("max_tokens" . ,el-max-tokens)
           ("stream" . t)
           ("temperature" . ,el-temperature)
           ("thinking" .
            (("type" . "enabled")
             ("budget_tokens" . 32000)))
           ("messages" .
            [(("role" . "user")
              ("content" . ,prompt))]))))
       (args
        (list "-N"
              "https://api.anthropic.com/v1/messages"
              "-H" "Content-Type: application/json"
              "-H" "anthropic-version: 2023-06-01"
              "-H" "anthropic-beta: output-128k-2025-02-19"
              "-H"
              (concat "x-api-key: " el-anthropic-api-key)
              "-d" payload))
       (proc
        (apply #'start-process "el-anthropic-stream" temp-buffer "curl" args)))

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

    ;; Set up process properties and handlers
    (process-put proc 'target-buffer buffer-name)
    (process-put proc 'temp-buffer temp-buffer)
    (process-put proc 'content "")
    (set-process-filter proc #'el--anthropic-stream-filter)
    (set-process-sentinel proc #'el--stream-sentinel)

    (el--append-to-history "user" prompt)))

;; Multi-provider streaming function
(defun el--send-multistream-request
    (prompt)
  "Send PROMPT to the selected provider using streaming API."
  (interactive)
  (pcase el-provider
    ("openai"
     (el--send-openai-stream prompt))
    ("anthropic"
     (el--send-anthropic-stream prompt))
    (_
     (message "Unsupported provider for streaming: %s" el-provider))))

;;;###autoload
(defun el-set-provider
    ()
  "Set the LLM provider to use."
  (interactive)
  (let
      ((providers
        '("openai" "anthropic")))
    (setq el-provider
          (completing-read "Select provider: " providers nil t nil nil el-provider))
    (message "Provider set to %s" el-provider)))

;;;###autoload
(defun el-toggle-stream
    ()
  "Toggle streaming mode."
  (interactive)
  (setq el-use-stream
        (not el-use-stream))
  (message "Streaming mode %s"
           (if el-use-stream "enabled" "disabled")))

;;;###autoload
(defun el-send-region-or-prompt-multi
    ()
  "Send region or prompt to selected provider."
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
    (if el-use-stream
        (el--send-multistream-request prompt)
      (el--send-request prompt))))

(provide 'emacs-llm-multistream)

(provide '_emacs-llm-variables)

(when
    (not load-file-name)
  (message "_emacs-llm-variables.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))