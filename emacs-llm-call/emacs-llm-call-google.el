;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-27 01:18:09>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-llm/emacs-llm-call/emacs-llm-call-google.el

;; Main
;; ----------------------------------------

(defun --el-google-stream
    (prompt &optional template-name)
  "Send PROMPT to Google API via streaming.
Optional TEMPLATE-NAME is the name of the template used."
  (let*
      ((temp-buffer
        (generate-new-buffer " *google-temp-output*"))
       (actual-engine
        (or --el-google-engine --el-default-engine-google))
       (url
        (format "https://generativelanguage.googleapis.com/v1beta/models/%s:streamGenerateContent?alt=sse&key=%s"
                actual-engine
                (or --el-api-key-google --el-google-api-key)))
       (full-prompt
        (--el-template-apply prompt template-name))
       (payload
        (--el-construct-google-payload full-prompt))
       (args
        (list "--no-buffer"
              url
              "-H" "Content-Type: application/json"
              "-d" payload))
       (buffer-name
        (--el-prepare-llm-buffer prompt "google" actual-engine template-name))
       (proc
        (apply #'start-process "--el-google-stream" temp-buffer "curl" args)))
    (process-put proc 'target-buffer buffer-name)
    (process-put proc 'temp-buffer temp-buffer)
    (process-put proc 'content "")
    (process-put proc 'partial-data "")
    (process-put proc 'provider "GOOGLE")
    (process-put proc 'engine actual-engine)
    (set-process-filter proc #'--el-google-filter)
    (set-process-sentinel proc #'--el-process-sentinel)
    (--el-start-spinner)
    (--el-history-append "user" prompt template-name)
    proc))

;; Helper
;; ----------------------------------------
(defun --el-parse-google-chunk
    (chunk)
  "Parse Google JSON CHUNK and return content."
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
             (message "Error parsing JSON: %s"
                      (error-message-string err))
             nil))))
      (when data
        (let*
            ((candidates
              (alist-get 'candidates data))
             (first-candidate
              (if
                  (and candidates
                       (>
                        (length candidates)
                        0))
                  (car candidates)
                nil))
             (content
              (and first-candidate
                   (alist-get 'content first-candidate)))
             (parts
              (and content
                   (alist-get 'parts content))))
          (when parts
            (mapconcat
             (lambda
               (part)
               (alist-get 'text part))
             parts
             "")))))))

(defun --el-google-filter
    (proc chunk)
  "Filter for Google stream PROC processing CHUNK."
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
                  (--el-parse-google-chunk jsonstr)))
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
(defun --el-construct-google-payload
    (prompt)
  "Construct the JSON payload for Google Gemini API with PROMPT."
  (let*
      ((actual-engine
        (or --el-google-engine --el-default-engine-google))
       (max-tokens
        (or
         (alist-get actual-engine --el-google-engine-max-tokens-alist nil nil 'string=)
         100000))
       ;; Add recent history as string
       (conversation
        (--el-history-get-recent-as-string))
       ;; Combine history with prompt
       (full-conversation
        (if
            (string-empty-p conversation)
            prompt
          (concat conversation "\n\n" prompt))))
    (json-encode
     `(("contents" .
        ,(list
          `(("role" . "user")
            ("parts" .
             ((("text" . ,full-conversation)))))))
       ("generationConfig" .
        (("maxOutputTokens" . ,max-tokens)))))))

(provide 'emacs-llm-call-google)

(when
    (not load-file-name)
  (message "emacs-llm-call-google.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))