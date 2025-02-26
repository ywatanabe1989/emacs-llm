;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 13:17:18>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-anthropic.el

;; Main
;; ----------------------------------------

(defun el--send-anthropic-stream
    (prompt)
  "Send PROMPT to Anthropic API via streaming."
  (let*
      ((buffer-name
        (get-buffer-create el-buffer-name))
       (temp-buffer
        (generate-new-buffer " *anthropic-temp-output*"))
       (payload
        (el--construct-anthropic-payload prompt))
       (payload-oneline
        (replace-regexp-in-string "\n" " " payload))
       (escaped-payload
        (replace-regexp-in-string "'" "\\\\'" payload-oneline))
       (curl-command
        (format "curl -N 'https://api.anthropic.com/v1/messages' -H 'Content-Type: application/json' -H 'anthropic-version: 2023-06-01' -H 'anthropic-beta: output-128k-2025-02-19' -H 'x-api-key: %s' -d '%s'"
                el-anthropic-api-key
                escaped-payload))
       (proc
        (start-process-shell-command "el-anthropic-stream" temp-buffer curl-command)))
    ;; (message "DEBUG: Curl command: %s" curl-command)
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
      (insert el-separator)
      (insert "\n\n")
      (insert
       (format "Provider: %s | Model: %s\n\n" "ANTHROPIC" el-anthropic-model))
      (insert
       (format "Prompt: %s\n\n" prompt))
      (insert "Response: ")
      (display-buffer
       (current-buffer)))
    (process-put proc 'target-buffer buffer-name)
    (process-put proc 'temp-buffer temp-buffer)
    (process-put proc 'content "")
    (set-process-filter proc #'el--anthropic-filter)
    (set-process-sentinel proc #'el--process-sentinel)
    (el--append-to-history "user" prompt)
    proc))

;; Helper
;; ----------------------------------------

(defun el--parse-anthropic-chunk
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

(defun el--anthropic-filter
    (proc chunk)
  "Filter for Anthropic stream PROC processing CHUNK."
  (el--process-chunk proc chunk #'el--parse-anthropic-chunk))

(defun el--construct-anthropic-payload
    (prompt)
  "Construct the JSON payload for Anthropic API with PROMPT."
  (json-encode
   `(("model" . ,el-anthropic-model)
     ("max_tokens" . 128000)
     ("stream" . t)
     ;; ("temperature" . ,el-temperature)
     ("thinking" .
      (("type" . "enabled")
       ("budget_tokens" . 32000)))
     ("messages" .
      [(("role" . "user")
        ("content" . ,prompt))]))))

(provide 'emacs-llm-anthropic)

(when
    (not load-file-name)
  (message "emacs-llm-anthropic.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))