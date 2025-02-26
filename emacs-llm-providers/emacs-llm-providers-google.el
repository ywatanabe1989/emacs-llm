;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 14:19:24>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-providers/emacs-llm-providers-google.el

;; Main
;; ----------------------------------------
(defun --el--send-google-stream
    (prompt)
  "Send PROMPT to Google API via streaming."
  (let*
      ((buffer-name
        (get-buffer-create --el-buffer-name))
       (temp-buffer
        (generate-new-buffer " *google-temp-output*"))
       (mod--el-name
        (or --el-default-engine-google --el-google-model))
       (url
        (format "https://generativelanguage.googleapis.com/v1beta/models/%s:streamGenerateContent?alt=sse&key=%s"
                mod--el-name
                (or --el-api-key-google --el-google-api-key)))
       (max-tokens
        (or
         (alist-get mod--el-name --el-google-engine-max-tokens-alist nil nil 'string=)
         100000))
       (payload
        (json-encode
         `(("contents" .
            ((("parts" .
               (("text" . ,prompt))))))
           ("generationConfig" .
            (("maxOutputTokens" . ,max-tokens))))))
       (args
        (list "--no-buffer"
              url
              "-H" "Content-Type: application/json"
              "-d" payload))
       (proc
        (apply #'start-process "--el-google-stream" temp-buffer "curl" args)))

    (with-current-buffer buffer-name
      (unless
          (derived-mode-p 'markdown-mode)
        (markdown-mode))
      (goto-char
       (point-max))
      (unless
          (=
           (point-min)
           (point-max))
        (insert "\n\n"))
      (insert --el-separator)
      (insert "\n\n")
      (insert
       (format "Provider: %s | Model: %s\n\n" "GOOGLE" mod--el-name))
      (insert
       (format "Prompt: %s\n\n" prompt))
      (insert "Response: \n")
      (display-buffer
       (current-buffer)))

    (process-put proc 'target-buffer buffer-name)
    (process-put proc 'temp-buffer temp-buffer)
    (process-put proc 'content "")
    (process-put proc 'partial-data "")
    (set-process-filter proc #'--el--google-filter)
    (set-process-sentinel proc #'--el--process-sentinel)
    (--el--start-spinner)
    (--el--append-to-history "user" prompt)
    proc))

;; Helper
;; ----------------------------------------
(defun --el--parse-google-chunk
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

(defun --el--google-filter
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
                  (--el--parse-google-chunk jsonstr)))
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

(provide 'emacs-llm-providers-google)

(when
    (not load-file-name)
  (message "emacs-llm-providers-google.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))