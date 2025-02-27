;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-28 10:08:28>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-call/emacs-llm-call-deepseek.el

;; Helper
;; ----------------------------------------

(defun --el-construct-deepseek-curl-command
    (prompt)
  "Construct curl command for DeepSeek API with PAYLOAD."
  (let*
      ((payload
        (--el-construct-deepseek-payload prompt))
       (url "https://api.deepseek.com/v1/chat/completions")
       (auth-header
        (format "Bearer %s" --el-api-key-deepseek))
       (escaped-payload
        (shell-quote-argument payload)))
    (format "curl -v -N %s -H \"Content-Type: application/json\" -H \"Authorization: %s\" -d %s 2>&1"
            url auth-header escaped-payload)))

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
       ;; Get history in correct format
       (history
        (--el-history-load-recent))
       ;; Format messages properly for DeepSeek
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
       ("messages" . ,messages)
       ("temperature" . ,--el-temperature)
       ("max_tokens" . ,max-tokens)
       ("stream" . t)))))

(provide 'emacs-llm-call-deepseek)

(when
    (not load-file-name)
  (message "emacs-llm-call-deepseek.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))