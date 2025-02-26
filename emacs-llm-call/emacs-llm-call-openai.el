;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 21:18:21>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-call/emacs-llm-call-openai.el

;; (defun --el-openai-stream
;;     (prompt &optional template-name)
;;   "Send PROMPT to OpenAI API via streaming.
;; Optional TEMPLATE-NAME is the name of the template used."
;;   (let*
;;       ((temp-buffer
;;         (generate-new-buffer " *openai-temp-output*"))
;;        (full-prompt
;;         (--el-apply-template prompt template-name))
;;        (payload
;;         (--el-construct-openai-payload full-prompt))
;;        ;; No need for string replacements when using shell-quote-argument
;;        (curl-command
;;         (format "curl -N 'https://api.openai.com/v1/chat/completions' -H 'Content-Type: application/json' -H 'Authorization: Bearer %s' -d %s"
;;                 (or --el-api-key-openai --el-api-key)
;;                 (shell-quote-argument payload)))
;;        (model-name
;;         (or --el-openai-model --el-default-engine-openai)))
;;     (message "Using model: %s" model-name)
;;     (message "Curl command length: %d"
;;              (length curl-command))
;;     (message "Payload first 100 chars: %s"
;;              (substring payload 0
;;                         (min 100
;;                              (length payload))))
;;     (let*
;;         ((buffer-name
;;           (--el-prepare-llm-buffer prompt "OPENAI" model-name template-name))
;;          (proc
;;           (start-process-shell-command "--el-openai-stream" temp-buffer curl-command)))
;;       (message "Process started with PID: %s"
;;                (process-id proc))
;;       (process-put proc 'target-buffer buffer-name)
;;       (process-put proc 'temp-buffer temp-buffer)
;;       (process-put proc 'content "")
;;       (process-put proc 'prompt prompt)
;;       (process-put proc 'provider "OPENAI")
;;       (process-put proc 'model model-name)
;;       (process-put proc 'template template-name)
;;       (set-process-filter proc #'--el-openai-filter)
;;       (message "Process filter set")
;;       (set-process-sentinel proc #'--el-process-sentinel)
;;       (message "Process sentinel set")
;;       (--el-start-spinner)
;;       ;; Don't append to history here - let the process-sentinel do it
;;       ;; when the response is complete
;;       proc)))

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
            (--el-apply-template prompt template-name)
          prompt))
       (payload
        (--el-construct-openai-payload full-prompt))
       ;; No need for string replacements when using shell-quote-argument
       (curl-command
        (format "curl -N 'https://api.openai.com/v1/chat/completions' -H 'Content-Type: application/json' -H 'Authorization: Bearer %s' -d %s"
                (or --el-api-key-openai --el-api-key)
                (shell-quote-argument payload)))
       (model-name
        (or --el-openai-model --el-default-engine-openai)))
    (message "Using model: %s" model-name)
    (message "Curl command length: %d"
             (length curl-command))
    (message "Payload first 100 chars: %s"
             (substring payload 0
                        (min 100
                             (length payload))))
    (let*
        (;; Pass original prompt for display purposes, not the template-augmented one
         (buffer-name
          (--el-prepare-llm-buffer prompt "OPENAI" model-name template-name))
         (proc
          (start-process-shell-command "--el-openai-stream" temp-buffer curl-command)))
      (message "Process started with PID: %s"
               (process-id proc))
      (process-put proc 'target-buffer buffer-name)
      (process-put proc 'temp-buffer temp-buffer)
      (process-put proc 'content "")
      (process-put proc 'prompt prompt)
      (process-put proc 'provider "OPENAI")
      (process-put proc 'model model-name)
      ;; Don't store template name in process properties
      ;; (process-put proc 'template template-name)
      (set-process-filter proc #'--el-openai-filter)
      (message "Process filter set")
      (set-process-sentinel proc #'--el-process-sentinel)
      (message "Process sentinel set")
      (--el-start-spinner)
      ;; Don't append to history here - let the process-sentinel do it
      ;; when the response is complete
      proc)))

;; Helper
;; ----------------------------------------

(defun --el-parse-openai-chunk
    (chunk)
  "Parse OpenAI JSON CHUNK and return content."
  (message "DEBUG: Parsing OpenAI chunk: %s"
           (substring chunk 0
                      (min 30
                           (length chunk))))
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
          (message "DEBUG: Delta: %S" delta)
          ;; Check for content or thinking (reasoning_efforts)
          (if
              (alist-get 'content delta)
              (alist-get 'content delta)
            (if
                (alist-get 'reasoning_efforts delta)
                (progn
                  (message "DEBUG: Found reasoning_efforts: %s"
                           (alist-get 'reasoning_efforts delta))
                  (alist-get 'reasoning_efforts delta))
              nil)))))))

(defun --el-openai-filter
    (proc chunk)
  "Filter for OpenAI stream PROC processing CHUNK."
  (message "DEBUG: OpenAI filter received chunk of length %d"
           (length chunk))
  (when
      (>
       (length chunk)
       0)
    (message "DEBUG: Full chunk: %s" chunk))
  (--el-process-chunk proc chunk #'--el-parse-openai-chunk))

(defun --el-construct-openai-payload
    (prompt)
  "Construct the JSON payload for OpenAI API with PROMPT."
  (let*
      ((model-name
        (or --el-openai-model --el-default-engine-openai))
       (max-tokens
        (or
         (alist-get model-name --el-openai-engine-max-tokens-alist nil nil 'string=)
         8192))
       ;; Parse engine for reasoning effort
       (engine-parts
        (--el-parse-openai-engine model-name))
       (actual-engine
        (car engine-parts))
       (effort
        (cdr engine-parts))
       ;; Get recent history
       (recent-history
        (--el-get-recent-history))
       ;; Create messages array - include history and current message
       (messages
        (append recent-history
                (list
                 `(("role" . "user")
                   ("content" . ,prompt)))))
       ;; Base payload
       (payload
        `(("model" . ,actual-engine)
          ("messages" . ,messages)
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
      (message "DEBUG: Final JSON payload: %s"
               (substring json-result 0
                          (min 300
                               (length json-result))))
      json-result)))

(defun --el-parse-openai-engine
    (engine-string)
  "Parse OpenAI engine string to get base model and effort level.
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
    (message "DEBUG: Parsed engine %s to %s with effort %s"
             engine-string base effort)
    (cons base effort)))

(provide 'emacs-llm-call-openai)

(when
    (not load-file-name)
  (message "emacs-llm-call-openai.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))