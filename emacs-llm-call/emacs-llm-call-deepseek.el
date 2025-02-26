;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-27 01:20:53>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-llm/emacs-llm-call/emacs-llm-call-deepseek.el

(defun --el-deepseek-stream
    (prompt &optional template-name)
  "Send PROMPT to DeepSeek API via streaming.
Optional TEMPLATE-NAME is the name of the template used."
  (let*
      ((temp-buffer
        (generate-new-buffer " *deepseek-temp-output*"))
       (full-prompt
        (--el-apply-template prompt template-name))
       (payload
        (--el-construct-deepseek-payload full-prompt))
       (payload-oneline
        (replace-regexp-in-string "\n" " " payload))
       (escaped-payload
        (replace-regexp-in-string "'" "\\\\'" payload-oneline))
       (curl-command
        (format "curl -N 'https://api.deepseek.com/v1/chat/completions' -H 'Content-Type: application/json' -H 'Authorization: Bearer %s' -d '%s'"
                --el-deepseek-api-key
                escaped-payload))
       (actual-engine
        (or --el-deepseek-engine --el-default-engine-deepseek))
       (buffer-name
        (--el-prepare-llm-buffer prompt "deepseek" actual-engine template-name))
       (proc
        (start-process-shell-command "--el-deepseek-stream" temp-buffer curl-command)))
    (process-put proc 'target-buffer buffer-name)
    (process-put proc 'temp-buffer temp-buffer)
    (process-put proc 'content "")
    (process-put proc 'partial-data "")
    (process-put proc 'prompt prompt)
    (process-put proc 'provider "deepseek")
    (process-put proc 'engine actual-engine)
    (process-put proc 'template template-name)
    (set-process-filter proc #'--el-deepseek-filter)
    (set-process-sentinel proc #'--el-process-sentinel)
    (--el-start-spinner)
    (--el-history-append "user" prompt template-name)
    proc))

;; Helper
;; ----------------------------------------

(defun --el-parse-deepseek-chunk
    (chunk)
  "Parse DeepSeek JSON CHUNK and return content."

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

(defun --el-deepseek-filter
    (proc chunk)
  "Filter for DeepSeek stream PROC processing CHUNK."

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
                  (--el-parse-deepseek-chunk jsonstr)))
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

(defun --el-construct-deepseek-payload
    (prompt)
  "Construct the JSON payload for DeepSeek API with PROMPT."
  (let*
      ((actual-engine
        (or --el-deepseek-engine --el-default-engine-deepseek))
       (max-tokens
        (or
         (alist-get actual-engine --el-deepseek-engine-max-tokens-alist nil nil 'string=)
         8192))
       ;; Add recent history as string
       (conversation
        (--el-history-get-recent-as-string))
       ;; Combine history with prompt
       (full-conversation
        (if
            (string-empty-p conversation)
            prompt
          (concat conversation "\n\n" prompt)))
       (payload
        (json-encode
         `(("model" . ,actual-engine)
           ("messages" . ,(list
                           `(("role" . "user")
                             ("content" . ,full-conversation))))
           ("temperature" . ,--el-temperature)
           ("max_tokens" . ,max-tokens)
           ("stream" . t)))))
    payload))

(provide 'emacs-llm-call-deepseek)

(when
    (not load-file-name)
  (message "emacs-llm-call-deepseek.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))