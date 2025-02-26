;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 16:49:15>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-providers/emacs-llm-providers-deepseek.el

(defun --el-deepseek-stream
    (prompt &optional template)
  "Send PROMPT to DeepSeek API via streaming.
Optional TEMPLATE is the name of the template used."
  (let*
      ((temp-buffer
        (generate-new-buffer " *deepseek-temp-output*"))
       (payload
        (--el-construct-deepseek-payload prompt))
       (payload-oneline
        (replace-regexp-in-string "\n" " " payload))
       (escaped-payload
        (replace-regexp-in-string "'" "\\\\'" payload-oneline))
       (curl-command
        (format "curl -N 'https://api.deepseek.com/v1/chat/completions' -H 'Content-Type: application/json' -H 'Authorization: Bearer %s' -d '%s'"
                --el-deepseek-api-key
                escaped-payload))
       (model-name
        (or --el-deepseek-model --el-default-engine-deepseek))
       (buffer-name
        (--el-prepare-llm-buffer prompt "DEEPSEEK" model-name template))
       (proc
        (start-process-shell-command "--el-deepseek-stream" temp-buffer curl-command)))

    (process-put proc 'target-buffer buffer-name)
    (process-put proc 'temp-buffer temp-buffer)
    (process-put proc 'content "")
    (process-put proc 'partial-data "")
    (set-process-filter proc #'--el-deepseek-filter)
    (set-process-sentinel proc #'--el-process-sentinel)
    (--el-start-spinner)
    (--el-append-to-history "user" prompt template)
    proc))

;; Helper
;; ----------------------------------------
(defun --el-parse-deepseek-chunk
    (chunk)
  "Parse DeepSeek JSON CHUNK and return content."
  ;; (message "DEBUG: Parsing DeepSeek chunk: %s"
  ;;          (substring chunk 0
  ;;                     (min 30
  ;;                          (length chunk))))
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
          ;; (message "DEBUG: DeepSeek delta: %s" delta)
          (alist-get 'content delta))))))

(defun --el-deepseek-filter
    (proc chunk)
  "Filter for DeepSeek stream PROC processing CHUNK."
  ;; (message "DEBUG: DeepSeek filter received chunk of length %d"
  ;;          (length chunk))
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

;; (defun --el-construct-deepseek-payload
;;     (prompt)
;;   "Construct the JSON payload for DeepSeek API with PROMPT."
;;   (let*
;;       ((mod--el-name
;;         (or --el-deepseek-model --el-default-engine-deepseek))
;;        (max-tokens
;;         (or
;;          (alist-get mod--el-name --el-deepseek-engine-max-tokens-alist nil nil 'string=)
;;          8192))
;;        (recent-history
;;         (--el-get-recent-history))
;;        (payload
;;         (json-encode
;;          `(("model" . ,mod--el-name)
;;            ("messages" . ,(append recent-history
;;                                   (vector
;;                                    `(("role" . "user")
;;                                      ("content" . ,prompt)))))
;;            ("temperature" . ,--el-temperature)
;;            ("max_tokens" . ,max-tokens)
;;            ("stream" . t)))))
;;     ;; (message "DEBUG: Constructed DeepSeek payload: %s" payload)
;;     payload))

(defun --el-construct-deepseek-payload
    (prompt)
  "Construct the JSON payload for DeepSeek API with PROMPT."
  (let*
      ((mod--el-name
        (or --el-deepseek-model --el-default-engine-deepseek))
       (max-tokens
        (or
         (alist-get mod--el-name --el-deepseek-engine-max-tokens-alist nil nil 'string=)
         8192))
       (recent-history
        (--el-get-recent-history))
       (payload
        (json-encode
         `(("model" . ,mod--el-name)
           ("messages" . ,(append recent-history
                                  (list
                                   `(("role" . "user")
                                     ("content" . ,prompt)))))
           ("temperature" . ,--el-temperature)
           ("max_tokens" . ,max-tokens)
           ("stream" . t)))))
    payload))

(provide 'emacs-llm-providers-deepseek)

(when
    (not load-file-name)
  (message "emacs-llm-providers-deepseek.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))