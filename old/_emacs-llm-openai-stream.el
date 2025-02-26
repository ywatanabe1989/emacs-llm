;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 10:16:13>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/03-01-llm-llm/el-openai-stream.el

;;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'el-variables)
(require 'json)
(require 'url)

(defun parse-model-content-from-chunk
    (chunk)
  "Parse a JSON CHUNK and return the content string.
Returns nil if no valid content is present or if CHUNK equals \"[DONE]\"."
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
          (alist-get 'content delta))))))

(defun lle-openai-stream-filter
    (proc chunk)
  "Process filter for openai stream.
Accumulates CHUNK data in a process property and processes complete lines.
Parses lines that start with \"data:\" and inserts returned text into the output buffer."
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
                  (parse-model-content-from-chunk jsonstr)))
              (when text
                (with-current-buffer
                    (get-buffer-create "*LLM-STREAMING-OUTPUT*")
                  (goto-char
                   (point-max))
                  (insert text))))))))))

(defun lle-openai-stream
    (text)
  "Stream response from OpenAI API using curl.
Displays only the text content in the *openai-text-output* buffer."
  (interactive)
  (let*
      ((api-key
        (getenv "OPENAI_API_KEY"))
       (payload
        (json-encode
         `(("model" . "o3-mini")
           ("messages" .
            [
             (("role" . "system")
              ("content" . "You are a helpful assistant."))
             (("role" . "user")
              ("content" . ,text))
             ])
           ("stream" . t))))
       (args
        (list "-N"
              "https://api.openai.com/v1/chat/completions"
              "-H" "Content-Type: application/json"
              "-H"
              (concat "Authorization: Bearer " api-key)
              "-d" payload))
       (proc
        (apply #'start-process "openai-stream-curl" "*openai-stream-curl*" "curl" args)))
    (set-process-filter proc 'lle-openai-stream-filter)
    (display-buffer
     (get-buffer-create "*LLM-STREAMING-OUTPUT*"))))

;; This works
;; (lle-openai-stream "Hello.")

(provide 'el-openai-stream)

(when
    (not load-file-name)
  (message "el-openai-stream.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))