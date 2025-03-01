;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-03-01 16:09:54>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-call/emacs-llm-call-anthropic.el

;; (defun --el-construct-anthropic-curl-command
;;     (prompt)
;;   "Construct curl command for Anthropic API with PROMPT."
;;   (let*
;;       ((url "https://api.anthropic.com/v1/messages")
;;        (auth-header
;;         (format "x-api-key: %s"
;;                 (or --el-api-key-anthropic
;;                     (getenv "ANTHROPIC_API_KEY"))))
;;        (version-header "anthropic-version: 2023-06-01")
;;        (content-header "content-type: application/json")
;;        (messages
;;         (vector
;;          `(("role" . "user")
;;            ("content" . ,prompt))))
;;        (payload
;;         `(("model" . "claude-3-7-sonnet-20250219")
;;           ("max_tokens" . 64000)
;;           ("stream" . t)
;;           ("thinking" .
;;            (("type" . "enabled")
;;             ("budget_tokens" . 32000)))
;;           ("messages" . ,messages)))
;;        (json-payload
;;         (json-encode payload))
;;        (escaped-payload
;;         (shell-quote-argument json-payload)))
;;     (format "curl %s --header \"%s\" --header \"%s\" --header \"%s\" --data %s"
;;             url auth-header version-header content-header escaped-payload)))

(defun --el-construct-anthropic-curl-command
    (prompt)
  "Construct curl command for Anthropic API with PROMPT."
  (let*
      ((url "https://api.anthropic.com/v1/messages")
       (auth-header
        (format "x-api-key: %s"
                (or --el-api-key-anthropic
                    (getenv "ANTHROPIC_API_KEY"))))
       (version-header "anthropic-version: 2023-06-01")
       (content-header "content-type: application/json")
       ;; Get history in correct format
       (history
        (--el-history-load-recent))
       ;; Format messages array properly - collect all previous messages and current prompt
       (messages-vec
        (vconcat
         (when history
           (vconcat
            (mapcar
             (lambda
               (msg)
               `(("role" . ,(cdr
                             (assoc "role" msg)))
                 ("content" . ,(cdr
                                (assoc "content" msg)))))
             history)))
         (vector
          `(("role" . "user")
            ("content" . ,prompt)))))
       (payload
        `(("model" . "claude-3-7-sonnet-20250219")
          ("max_tokens" . 64000)
          ("stream" . t)
          ("thinking" .
           (("type" . "enabled")
            ("budget_tokens" . 32000)))
          ("messages" . ,messages-vec)))
       (json-payload
        (json-encode payload))
       (escaped-payload
        (shell-quote-argument json-payload)))
    (format "curl %s --header \"%s\" --header \"%s\" --header \"%s\" --data %s"
            url auth-header version-header content-header escaped-payload)))

(defun --el-parse-anthropic-chunk
    (chunk)
  "Parse Anthropic JSON CHUNK and return content."
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
            (error nil))))
      (when data
        (let
            ((delta
              (alist-get 'delta data)))
          (or
           (alist-get 'text delta)
           (alist-get 'thinking delta)))))))

(defun --el-anthropic-filter
    (proc chunk)
  "Filter for Anthropic stream PROC processing CHUNK."
  ;; (message "Received chunk: %s"
  ;;          (substring chunk 0
  ;;                     (min 100
  ;;                          (length chunk))))
  (let*
      ((partial
        (or
         (process-get proc 'partial-data)
         ""))
       (combined
        (concat partial chunk))
       (lines
        (split-string combined "\n")))

    (message "Processing %d lines"
             (length lines))

    ;; Handle incomplete lines
    (let
        ((incomplete
          (if
              (string-suffix-p "\n" combined)
              ""
            (car
             (last lines)))))
      (message "Incomplete line length: %d"
               (length incomplete))
      (unless
          (string= incomplete "")
        (setq lines
              (butlast lines)))
      (process-put proc 'partial-data incomplete)

      ;; Process each line individually, look for data lines with content_block_delta
      (dolist
          (line lines)
        (when
            (string-match "^data: \\(.*\\)" line)
          (let*
              ((json-data
                (match-string 1 line))
               (json-object-type 'alist)
               (json-array-type 'list)
               (json-key-type 'string)
               (data
                (condition-case err
                    (json-read-from-string json-data)
                  (error
                   (message "JSON parse error: %s"
                            (error-message-string err))
                   nil))))

            (when data
              (let
                  ((type
                    (alist-get 'type data nil nil 'string=)))
                (message "Event type: %s" type)

                (when
                    (string= type "content_block_delta")
                  (let*
                      ((delta
                        (alist-get 'delta data nil nil 'string=))
                       (delta-type
                        (and delta
                             (alist-get 'type delta nil nil 'string=))))

                    ;; (message "Delta type: %s" delta-type)

                    (when
                        (and delta-type
                             (string= delta-type "text_delta"))
                      (let
                          ((text
                            (alist-get 'text delta nil nil 'string=)))
                        ;; (message "Found text: %s" text)
                        (when text
                          (with-current-buffer
                              (process-get proc 'target-buffer)
                            (save-excursion
                              (goto-char
                               (point-max))
                              (insert text)))
                          (process-put proc 'content
                                       (concat
                                        (or
                                         (process-get proc 'content)
                                         "")
                                        text)))))))))))))))

(provide 'emacs-llm-call-anthropic)

(when
    (not load-file-name)
  (message "emacs-llm-call-anthropic.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))