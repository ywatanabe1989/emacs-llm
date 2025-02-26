;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-27 01:53:33>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-call/emacs-llm-call-openai.el

(defun --el-openai-stream
    (prompt &optional template-name)
  "Send PROMPT to OpenAI API via streaming.
Optional TEMPLATE-NAME is the name of the template used."
  (let*
      ((temp-buffer
        (generate-new-buffer " *openai-temp-output*"))
       ;; Apply template internally to get full prompt
       (full-prompt
        (if template-name
            (--el-template-apply prompt template-name)
          prompt))
       (payload
        (--el-construct-openai-payload full-prompt))
       ;; No need for string replacements when using shell-quote-argument
       (curl-command
        (format "curl -N 'https://api.openai.com/v1/chat/completions' -H 'Content-Type: application/json' -H 'Authorization: Bearer %s' -d %s"
                (or --el-api-key-openai --el-api-key)
                (shell-quote-argument payload)))
       (engine-name
        (or --el-openai-engine --el-default-engine-openai)))
    ;; (message "Using engine: %s" engine-name)
    ;; (message "Curl command length: %d"
    ;;          (length curl-command))
    ;; (message "Payload first 300 chars: %s"
    ;;          (substring payload 0
    ;;                     (min 300
    ;;                          (length payload))))
    (let*
        (;; Pass original prompt for display purposes, not the template-augmented one
         (buffer-name
          (--el-prepare-llm-buffer prompt "openai" engine-name template-name))
         (proc
          (start-process-shell-command "--el-openai-stream" temp-buffer curl-command)))
      (message "Process started with PID: %s"
               (process-id proc))
      (process-put proc 'target-buffer buffer-name)
      (process-put proc 'temp-buffer temp-buffer)
      (process-put proc 'content "")
      (process-put proc 'prompt prompt)
      (process-put proc 'provider "openai")
      (process-put proc 'engine engine-name)
      (process-put proc 'template template-name)
      (set-process-filter proc #'--el-openai-filter)
      (message "Process filter set")
      (set-process-sentinel proc #'--el-process-sentinel)
      (message "Process sentinel set")
      (--el-start-spinner)
      proc)))

(defun --el-parse-openai-chunk
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
             (message "Error parsing JSON: %s | Chunk: %s"
                      (error-message-string err)
                      (substring chunk 0
                                 (min 50
                                      (length chunk))))
             nil))))
      (when data
        ;; (message "Parsed data: %S" data)
        ;; Add this debug line
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

          ;; Check for content or thinking (reasoning_efforts)
          (if
              (alist-get 'content delta)
              (progn
                (alist-get 'content delta))
            (if
                (alist-get 'reasoning_efforts delta)
                (progn
                  (alist-get 'reasoning_efforts delta))
              nil)))))))

(defun --el-openai-filter
    (proc chunk)
  "Filter for OpenAI stream PROC processing CHUNK."

  (when
      (>
       (length chunk)
       0)
    )
  (--el-process-chunk proc chunk #'--el-parse-openai-chunk))

(defun --el-construct-openai-payload
    (prompt)
  "Construct the JSON payload for OpenAI API with PROMPT."
  (let*
      ((engine-name
        (or --el-openai-engine --el-default-engine-openai))
       (max-tokens
        (or
         (alist-get engine-name --el-openai-engine-max-tokens-alist nil nil 'string=)
         8192))
       ;; Parse engine for reasoning effort
       (engine-parts
        (--el-parse-openai-engine engine-name))
       (actual-engine
        (car engine-parts))
       (effort
        (cdr engine-parts))
       ;; Add recent history as string
       (conversation
        (--el-history-get-recent-as-string))
       ;; Combine history with prompt
       (full-conversation
        (if
            (string-empty-p conversation)
            prompt
          (concat conversation "\n\n" prompt)))
       ;; Base payload
       (payload
        `(("model" . ,actual-engine)
          ;; This is correct - OpenAI API uses "model"
          ("messages" . ,(list
                          `(("role" . "user")
                            ("content" . ,full-conversation))))
          ;; ("max_tokens" . ,max-tokens)
          ;; ("temperature" . ,--el-temperature)
          ("stream" . t))))
    ;; Add reasoning_effort if specified
    (when effort
      (push
       (cons "reasoning_effort" effort)
       payload))
    (let
        ((json-result
          (json-encode payload)))
      json-result)))

(defun --el-parse-openai-engine
    (engine-string)
  "Parse OpenAI engine string to get base engine and effort level.
Returns (ACTUAL-ENGINE . EFFORT)."
  (let
      ((base nil)
       (effort nil))
    (cond
     ;; o3-mini variants
     ((string-prefix-p "o3-mini" engine-string)
      (setq base "o3-mini")
      (when
          (string-match "-\\(low\\|medium\\|high\\)$" engine-string)
        (setq effort
              (match-string 1 engine-string))))
     ;; o1 variants
     ((string-prefix-p "o1" engine-string)
      (setq base "o1")
      (when
          (string-match "-\\(low\\|medium\\|high\\)$" engine-string)
        (setq effort
              (match-string 1 engine-string))))
     ;; gpt-4o
     ((string= engine-string "gpt-4o")
      (setq base "gpt-4o"))
     ;; fallback
     (t
      (setq base engine-string)))
    (cons base effort)))

(provide 'emacs-llm-call-openai)

(when
    (not load-file-name)
  (message "emacs-llm-call-openai.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))