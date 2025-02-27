;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-28 10:07:43>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-call/emacs-llm-call-anthropic.el

;; Main
;; ----------------------------------------

;; Helper
;; ----------------------------------------

(defun --el-construct-anthropic-curl-command
    (prompt)
  "Construct curl command for Anthropic API with PAYLOAD."
  (let*
      ((payload
        (--el-construct-anthropic-payload prompt))
       (url "https://api.anthropic.com/v1/messages")
       (api-key
        (or --el-api-key-anthropic --el-anthropic-api-key))
       (escaped-payload
        (shell-quote-argument payload)))
    (format "curl -v -N %s -H \"Content-Type: application/json\" -H \"anthropic-version: 2023-06-01\" -H \"anthropic-beta: output-128k-2025-02-19\" -H \"x-api-key: %s\" -d %s 2>&1"
            url api-key escaped-payload)))

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
                  (--el-parse-anthropic-chunk jsonstr)))
              (when text
                (with-current-buffer
                    (process-get proc 'target-buffer)
                  (goto-char
                   (point-max))
                  (insert text))
                (process-put proc 'content
                             (concat
                              (or
                               (process-get proc 'content)
                               "")
                              text))))))))))

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
       ;; Get history in correct format
       (history
        (--el-history-load-recent))
       ;; Prepare properly formatted messages for Anthropic
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
            ("content" . ,prompt))))))
    (json-encode
     `(("model" . ,actual-engine)
       ("max_tokens" . ,max-tokens)
       ("stream" . t)
       ("thinking" .
        (("type" . "enabled")
         ("budget_tokens" . 32000)))
       ("messages" . ,messages)))))

(provide 'emacs-llm-call-anthropic)

(when
    (not load-file-name)
  (message "emacs-llm-call-anthropic.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))