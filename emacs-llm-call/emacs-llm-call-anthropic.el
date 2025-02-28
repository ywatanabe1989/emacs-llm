;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-03-01 08:53:48>
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
  (message "Received chunk: %s"
           (substring chunk 0
                      (min 100
                           (length chunk))))
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

                    (message "Delta type: %s" delta-type)

                    (when
                        (and delta-type
                             (string= delta-type "text_delta"))
                      (let
                          ((text
                            (alist-get 'text delta nil nil 'string=)))
                        (message "Found text: %s" text)
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

;; (lle-shell-run-cmd (--el-construct-anthropic-curl-command "hi"))
;; ("% Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
;;                                  Dload  Upload   Total   Spent    Left  Speed
;;   0     0    0     0    0     0      0      0 --:--:-- --:--:-- --:--:--     0100   289    0     0  100   289      0    236  0:00:01  0:00:01 --:--:--   236event: message_start
;; data: {\"type\":\"message_start\",\"message\":{\"id\":\"msg_01MX76pKx3wVYvYH1GKcXsGa\",\"type\":\"message\",\"role\":\"assistant\",\"model\":\"claude-3-7-sonnet-20250219\",\"content\":[],\"stop_reason\":null,\"stop_sequence\":null,\"usage\":{\"input_tokens\":38,\"cache_creation_input_tokens\":0,\"cache_read_input_tokens\":0,\"output_tokens\":2}}    }

;; 100   626    0   337  100   289    220    188  0:00:01  0:00:01 --:--:--   408event: content_block_start
;; data: {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"thinking\",\"thinking\":\"\",\"signature\":\"\"}      }

;; event: ping
;; data: {\"type\": \"ping\"}

;; event: content_block_delta
;; data: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"thinking_delta\",\"thinking\":\"This\"}  }

;; event: content_block_delta
;; data: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"thinking_delta\",\"thinking\":\" is a very brief greeting from the user. I should respond in\"}               }

;; event: content_block_delta
;; data: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"thinking_delta\",\"thinking\":\" a friendly and welcoming manner, matching their casual tone. I\"}}

;; event: content_block_delta
;; data: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"thinking_delta\",\"thinking\":\" can offer to help with any questions or tasks they\"}             }

;; 100  1508    0  1219  100   289    518    123  0:00:02  0:00:02 --:--:--   641event: content_block_delta
;; data: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"thinking_delta\",\"thinking\":\" might have.\"}   }

;; event: content_block_delta
;; data: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"signature_delta\",\"signature\":\"ErUBCkYIARgCIkA9J5EnttUy5ihxhY2Zu/awdlwv6Ml/cNUHx+G9qVOEYAQAmwxAQV3cwPcXQrScYtxYwC68W4H/LeOpetExwXYgEgw8F+/4K58y30SrFhsaDASFhu4dR3gbd8eKfSIwEuwg9wft77tdozkiJOxScxJeAlqxwTta5fblGEfvrJSg+JwPi4QEJCsOlPo6eJapKh1W4h/9cQO4yrQjW8whhFt08CPCYJnemED4aJ5qlQ==\"}     }

;; event: content_block_stop
;; data: {\"type\":\"content_block_stop\",\"index\":0   }

;; event: content_block_start
;; data: {\"type\":\"content_block_start\",\"index\":1,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}

;; event: content_block_delta
;; data: {\"type\":\"content_block_delta\",\"index\":1,\"delta\":{\"type\":\"text_delta\",\"text\":\"Hello! How are you today? I'm here to help if you\"}           }

;; event: content_block_delta
;; data: {\"type\":\"content_block_delta\",\"index\":1,\"delta\":{\"type\":\"text_delta\",\"text\":\" have any questions or if there's anything you\"} }

;; event: content_block_delta
;; data: {\"type\":\"content_block_delta\",\"index\":1,\"delta\":{\"type\":\"text_delta\",\"text\":\"'d like to discuss. How can I assist you?\"}         }

;; event: content_block_stop
;; data: {\"type\":\"content_block_stop\",\"index\":1               }

;; event: message_delta
;; data: {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\",\"stop_sequence\":null},\"usage\":{\"output_tokens\":86}           }

;; event: message_stop
;; data: {\"type\":\"message_stop\"           }

;; 100  3018    0  2729  100   289    930     98  0:00:02  0:00:02 --:--:--  1028" . 0)

;; (el-run "hi" "hello")

(provide 'emacs-llm-call-anthropic)

(when
    (not load-file-name)
  (message "emacs-llm-call-anthropic.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))