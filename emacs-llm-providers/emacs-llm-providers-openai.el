;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 16:19:43>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-providers/emacs-llm-providers-openai.el

;; (defun --el--send-openai-stream
;;     (prompt)
;;   "Send PROMPT to OpenAI API via streaming."
;;   (let*
;;       ((buffer-name
;;         (get-buffer-create --el-buffer-name))
;;        (temp-buffer
;;         (generate-new-buffer " *openai-temp-output*"))
;;        (payload
;;         (--el--construct-openai-payload prompt))
;;        (payload-oneline
;;         (replace-regexp-in-string "\n" " " payload))
;;        (escaped-payload
;;         (replace-regexp-in-string "'" "\\\\'" payload-oneline))
;;        (curl-command
;;         (format "curl -N 'https://api.openai.com/v1/chat/completions' -H 'Content-Type: application/json' -H 'Authorization: Bearer %s' -d '%s'"
;;                 (or --el-api-key-openai --el-api-key)
;;                 escaped-payload))
;;        (proc
;;         (start-process-shell-command "--el-openai-stream" temp-buffer curl-command)))

;;     ;; (message "DEBUG: Original payload: %s" payload)
;;     ;; (message "DEBUG: Payload on one line: %s" payload-oneline)
;;     ;; (message "DEBUG: Escaped payload: %s" escaped-payload)
;;     ;; (message "DEBUG: Curl command: %s" curl-command)
;;     (with-current-buffer buffer-name
;;       (unless
;;           (derived-mode-p 'markdown-mode)
;;         (markdown-mode))
;;       (goto-char
;;        (point-max))
;;       (unless
;;           (=
;;            (point-min)
;;            (point-max))
;;         (insert "\n\n"))
;;       (insert --el-separator)
;;       (insert "\n\n")
;;       (insert
;;        (format "Provider: %s | Model: %s\n\n" "OPENAI"
;;                (or --el-default-engine-openai --el-openai-model)))
;;       (insert
;;        (format "Prompt: %s\n\n" prompt))
;;       (insert "Response: ")
;;       (display-buffer
;;        (current-buffer)))

;;     (process-put proc 'target-buffer buffer-name)
;;     (process-put proc 'temp-buffer temp-buffer)
;;     (process-put proc 'content "")
;;     (set-process-filter proc #'--el--openai-filter)
;;     (set-process-sentinel proc #'--el--process-sentinel)
;;     (--el--start-spinner)
;;     (--el--append-to-history "user" prompt)
;;     proc))

(defun --el--send-openai-stream
    (prompt &optional template)
  "Send PROMPT to OpenAI API via streaming.
Optional TEMPLATE is the name of the template used."
  (let*
      ((temp-buffer
        (generate-new-buffer " *openai-temp-output*"))
       (payload
        (--el--construct-openai-payload prompt))
       (payload-oneline
        (replace-regexp-in-string "\n" " " payload))
       (escaped-payload
        (replace-regexp-in-string "'" "\\\\'" payload-oneline))
       (curl-command
        (format "curl -N 'https://api.openai.com/v1/chat/completions' -H 'Content-Type: application/json' -H 'Authorization: Bearer %s' -d '%s'"
                (or --el-api-key-openai --el-api-key)
                escaped-payload))
       (model-name
        (or --el-default-engine-openai --el-openai-model))
       (buffer-name
        (--el--prepare-llm-buffer prompt "OPENAI" model-name template))
       (proc
        (start-process-shell-command "--el-openai-stream" temp-buffer curl-command)))

    (process-put proc 'target-buffer buffer-name)
    (process-put proc 'temp-buffer temp-buffer)
    (process-put proc 'content "")
    (set-process-filter proc #'--el--openai-filter)
    (set-process-sentinel proc #'--el--process-sentinel)
    (--el--start-spinner)
    (--el--append-to-history "user" prompt template)
    proc))

;; Helper
;; ----------------------------------------
(defun --el--parse-openai-chunk
    (chunk)
  "Parse OpenAI JSON CHUNK and return content."
  ;; (message "DEBUG: Parsing OpenAI chunk: %s" (substring chunk 0 (min 30 (length chunk))))
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
          ;; (message "DEBUG: Delta: %s" delta)
          (alist-get 'content delta))))))

(defun --el--openai-filter
    (proc chunk)
  "Filter for OpenAI stream PROC processing CHUNK."
  ;; (message "DEBUG: OpenAI filter received chunk of length %d" (length chunk))
  (--el--process-chunk proc chunk #'--el--parse-openai-chunk))

;; (defun --el--construct-openai-payload
;;     (prompt)
;;   "Construct the JSON payload for OpenAI API with PROMPT."
;;   (let*
;;       ((mod--el-name
;;         (or --el-default-engine-openai --el-openai-model))
;;        (max-tokens
;;         (or
;;          (alist-get mod--el-name --el-openai-engine-max-tokens-alist nil nil 'string=)
;;          8192))
;;        (recent-history
;;         (--el--get-recent-history))
;;        (payload
;;         (json-encode
;;          `(("model" . ,mod--el-name)
;;            ("messages" . ,(append recent-history
;;                                   `((("role" . "user")
;;                                      ("content" . ,prompt)))))
;;            ("max_tokens" . ,max-tokens)
;;            ("temperature" . ,--el-temperature)
;;            ("stream" . t)))))
;;     ;; (message "DEBUG: Constructed payload: %s" payload)
;;     payload))

(defun --el--construct-openai-payload
    (prompt)
  "Construct the JSON payload for OpenAI API with PROMPT."
  (let*
      ((mod--el-name
        (or --el-default-engine-openai --el-openai-model))
       (max-tokens
        (or
         (alist-get mod--el-name --el-openai-engine-max-tokens-alist nil nil 'string=)
         8192))
       (recent-history
        (--el--get-recent-history))
       (payload
        (json-encode
         `(("model" . ,mod--el-name)
           ("messages" . ,(append recent-history
                                  (list
                                   `(("role" . "user")
                                     ("content" . ,prompt)))))
           ("max_tokens" . ,max-tokens)
           ("temperature" . ,--el-temperature)
           ("stream" . t)))))
    payload))

(provide 'emacs-llm-providers-openai)

(when
    (not load-file-name)
  (message "emacs-llm-providers-openai.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))