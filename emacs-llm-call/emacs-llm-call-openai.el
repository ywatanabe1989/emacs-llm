;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-03-01 16:48:05>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-call/emacs-llm-call-openai.el

(defun --el-construct-openai-curl-command
    (prompt)
  "Construct curl command for OpenAI API with PROMPT."
  (let*
      ((url "https://api.openai.com/v1/chat/completions")
       (auth-header
        (format "Bearer %s"
                (or --el-api-key-openai --el-api-key)))
       (engine-name
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
       ;; Get history in correct format
       (history
        (--el-history-load-recent))
       ;; Prepare messages array with proper format for OpenAI
       (messages
        (append
         (when history
           (mapcar
            (lambda
              (msg)
              `(("role" . ,(cdr
                            (assoc "role" msg)))
                ("content" . ,(cdr
                               (assoc "content" msg)))))
            history))
         `((("role" . "user")
            ("content" . ,prompt)))))
       ;; Base payload
       (payload
        `(("model" . ,actual-engine)
          ("messages" . ,messages)
          ("stream" . t)))
       ;; Add reasoning_effort if specified
       (payload-with-effort
        (if effort
            (cons
             (cons "reasoning_effort" effort)
             payload)
          payload))
       (json-payload
        (json-encode payload-with-effort))
       (escaped-payload
        (shell-quote-argument json-payload)))
    (format "curl -N %s -H \"Content-Type: application/json\" -H \"Authorization: %s\" -d %s"
            url auth-header escaped-payload)))

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
                              6
                            5)))))
          (unless
              (string= jsonstr "[DONE]")
            (let
                ((text
                  (--el-parse-openai-chunk jsonstr)))
              (when
                  (and text
                       (not
                        (string-empty-p text)))
                ;; Call callback if set
                (when-let
                    ((callback
                      (process-get proc 'callback)))
                  (funcall callback text nil proc))
                ;; Update buffer if target buffer is set
                (when-let
                    ((target-buffer
                      (process-get proc 'target-buffer)))
                  (when
                      (buffer-live-p
                       (get-buffer target-buffer))
                    (with-current-buffer target-buffer
                      (save-excursion
                        (goto-char
                         (point-max))
                        (insert text)))))
                ;; Store content
                (process-put proc 'content
                             (concat
                              (or
                               (process-get proc 'content)
                               "")
                              text))))))))))

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