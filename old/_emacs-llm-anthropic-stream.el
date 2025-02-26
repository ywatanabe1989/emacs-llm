;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 12:13:31>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/lle-llm-anthropic-stream.el

;; Main
;; ----------------------------------------

;; (el-anthropic-stream "hello")

(defun el-anthropic-stream
    (prompt)
  "Stream response from Anthropics API using curl and insert result at point.
Uses a temporary buffer to accumulate the text output."
  (interactive "sPrompt: ")
  (let*
      ((target-buffer
        (get-buffer-create "*LLM-STREAMING-OUTPUT*"))
       (_
        (display-buffer target-buffer))
       (insertion-marker
        (point-marker))
       (temp-buffer
        (generate-new-buffer " *anthropic-temp-output*"))
       (api-key
        (getenv "ANTHROPIC_API_KEY"))
       (payload
        (json-encode
         `(("model" . "claude-3-7-sonnet-20250219")
           ("max_tokens" . 128000)
           ("stream" . t)
           ("thinking" .
            (("type" . "enabled")
             ("budget_tokens" . 32000)))
           ("messages" .
            [
             (("role" . "user")
              ("content" . ,prompt))]))))
       (args
        (list "-N"
              "https://api.anthropic.com/v1/messages"
              "-H" "Content-Type: application/json"
              "-H" "anthropic-version: 2023-06-01"
              "-H" "anthropic-beta: output-128k-2025-02-19"
              "-H"
              (concat "x-api-key: " api-key)
              "-d" payload))
       (proc
        (apply #'start-process "anthropic-stream-curl" temp-buffer "curl" args)))
    (set-process-filter proc '--el-anthropic-stream-filter)
    (set-process-sentinel proc '--el-anthropic-stream-sentinel)
    (process-put proc 'target-buffer target-buffer)
    (process-put proc 'insertion-marker insertion-marker)
    (process-put proc 'temp-buffer temp-buffer)
    (display-buffer target-buffer)))

;; Helper
;; ----------------------------------------

(defun --el-anthropic-parse-message-from-chunk
    (chunk)
  "Parse JSON CHUNK and return the text delta if event is content_block_delta.
Return nil if not valid or not a delta event."
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
               (alist-get 'thinking delta)
               (alist-get 'text delta)))
            )
           (t nil)))))))

(defun --el-anthropic-stream-filter
    (proc chunk)
  "Filter for Anthropics stream.
Accumulates partial data, splits complete lines, and inserts returned text into buffers.
Processes lines starting with \"data:\"."
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
                              6 5)))))
          (unless
              (string= jsonstr "[DONE]")
            (let
                ((text
                  (--el-anthropic-parse-message-from-chunk jsonstr)))
              (when text
                (with-current-buffer
                    (process-get proc 'target-buffer)
                  (goto-char
                   (point-max))
                  (insert text))
                (with-current-buffer
                    (process-get proc 'temp-buffer)
                  (goto-char
                   (point-max))
                  (insert text))))))))))

(defun --el-anthropic-stream-sentinel
    (proc event)
  "Sentinel for Anthropics stream.
On finish, flush any remaining partial data and kill the temporary buffer."
  (when
      (string-match "finished" event)
    (let*
        ((target
          (process-get proc 'target-buffer))
         (partial
          (process-get proc 'partial-data))
         (temp-buf
          (process-get proc 'temp-buffer)))
      (when
          (and partial
               (not
                (string= partial "")))
        (let
            ((text
              (--el-anthropic-parse-message-from-chunk partial)))
          (when text
            (with-current-buffer target
              (goto-char
               (point-max))
              (insert text)))))
      (when
          (buffer-live-p temp-buf)
        (kill-buffer temp-buf))
      (message "Anthropic streaming completed."))))

(provide 'lle-llm-anthropic-stream)

(when
    (not load-file-name)
  (message "lle-llm-anthropic-stream.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))