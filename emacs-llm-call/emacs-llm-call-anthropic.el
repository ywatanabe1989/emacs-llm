;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 21:18:19>
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
        (--el-apply-template prompt template-name))
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
       (engine-name
        (or --el-anthropic-engine --el-default-engine-anthropic))
       (buffer-name
        (--el-prepare-llm-buffer prompt "ANTHROPIC" engine-name template-name))
       (proc
        (start-process-shell-command "--el-anthropic-stream" temp-buffer curl-command)))

    (process-put proc 'target-buffer buffer-name)
    (process-put proc 'temp-buffer temp-buffer)
    (process-put proc 'content "")
    (set-process-filter proc #'--el-anthropic-filter)
    (set-process-sentinel proc #'--el-process-sentinel)
    (--el-start-spinner)
    (--el-append-to-history "user" prompt template-name)
    proc))

;; Helper
;; ----------------------------------------

(defun --el-parse-anthropic-chunk
    (chunk)
  "Parse Anthropic JSON CHUNK and return text delta."
  ;; (message "Parsing Anthropic chunk: %s"
  ;;          (substring chunk 0
  ;;                     (min 30
  ;;                          (length chunk))))
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
        ;; (message "Data type: %s"
        ;;          (alist-get 'type data))
        (let
            ((type
              (alist-get 'type data)))
          (cond
           ((string= type "content_block_delta")
            (let
                ((delta
                  (alist-get 'delta data)))
              ;; (message "Content delta: %s" delta)
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
      ((engine-name
        (or --el-anthropic-engine --el-default-engine-anthropic))
       (max-tokens
        (or
         (alist-get engine-name --el-anthropic-engine-max-tokens-alist nil nil 'string=)
         128000)))
    (json-encode
     `(("engine" . ,engine-name)
       ("max_tokens" . ,max-tokens)
       ("stream" . t)
       ;; ("temperature" . ,--el-temperature)
       ("thinking" .
        (("type" . "enabled")
         ("budget_tokens" . 32000)))
       ("messages" .
        ,(list
          `(("role" . "user")
            ("content" . ,prompt))))))))

(provide 'emacs-llm-call-anthropic)

(when
    (not load-file-name)
  (message "emacs-llm-call-anthropic.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))