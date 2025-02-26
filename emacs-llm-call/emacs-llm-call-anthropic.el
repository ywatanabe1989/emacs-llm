;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-27 01:35:50>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-call/emacs-llm-call-anthropic.el

;; Main
;; ----------------------------------------

(defun --el-anthropic-stream
    (prompt &optional template-name)
  "Send PROMPT to Anthropic API via streaming.
Optional TEMPLATE-NAME is the name of the template-name used."
  (let*
      ((temp-buffer
        (generate-new-buffer " *anthropic-temp-output*"))
       (full-prompt
        (--el-template-apply prompt template-name))
       (payload
        (--el-construct-anthropic-payload full-prompt))
       (payload-oneline
        (replace-regexp-in-string "\n" " " payload))
       (escaped-payload
        (replace-regexp-in-string "'" "\\\\'" payload-oneline))
       (curl-command
        (format "curl -N 'https://api.anthropic.com/v1/messages' -H 'Content-Type: application/json' -H 'anthropic-version: 2023-06-01' -H 'anthropic-beta: output-128k-2025-02-19' -H 'x-api-key: %s' -d '%s'"
                (or --el-api-key-anthropic --el-anthropic-api-key)
                escaped-payload))
       (actual-engine
        (or --el-anthropic-engine --el-default-engine-anthropic))
       (buffer-name
        (--el-prepare-llm-buffer prompt "anthropic" actual-engine template-name))
       (proc
        (start-process-shell-command "--el-anthropic-stream" temp-buffer curl-command)))
    (process-put proc 'target-buffer buffer-name)
    (process-put proc 'temp-buffer temp-buffer)
    (process-put proc 'content "")
    (process-put proc 'prompt prompt)
    (process-put proc 'provider "anthropic")
    (process-put proc 'template template-name)
    (process-put proc 'engine actual-engine)
    (set-process-filter proc #'--el-anthropic-filter)
    (set-process-sentinel proc #'--el-process-sentinel)
    (--el-start-spinner)
    (--el-history-append "user" prompt template-name)
    proc))

;; Helper
;; ----------------------------------------

(defun --el-parse-anthropic-chunk
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

(defun --el-anthropic-filter
    (proc chunk)
  "Filter for Anthropic stream PROC processing CHUNK."
  (--el-process-chunk proc chunk #'--el-parse-anthropic-chunk))

(defun --el-construct-anthropic-payload
    (prompt)
  "Construct the JSON payload for Anthropic API with PROMPT."
  (let*
      ((actual-engine
        (or --el-anthropic-engine --el-default-engine-anthropic))
       (max-tokens
        (or
         (alist-get actual-engine --el-anthropic-engine-max-tokens-alist nil nil 'string=)
         128000))
       ;; Add recent history as string
       (conversation
        (--el-history-get-recent-as-string))
       ;; Combine history with prompt
       (full-conversation
        (if
            (string-empty-p conversation)
            prompt
          (concat conversation "\n\n" prompt))))
    (json-encode
     `(("model" . ,actual-engine)
       ;; This is correct - Anthropic API uses "model"
       ("max_tokens" . ,max-tokens)
       ("stream" . t)
       ;; ("temperature" . ,--el-temperature)
       ("thinking" .
        (("type" . "enabled")
         ("budget_tokens" . 32000)))
       ("messages" .
        ,(list
          `(("role" . "user")
            ("content" . ,full-conversation))))))))

(provide 'emacs-llm-call-anthropic)

(when
    (not load-file-name)
  (message "emacs-llm-call-anthropic.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))