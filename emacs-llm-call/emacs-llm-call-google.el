;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-03-01 16:47:54>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-call/emacs-llm-call-google.el

(defun --el-construct-google-curl-command
    (prompt)
  "Construct curl command for Google API with PROMPT."
  (let*
      ((actual-engine
        (or --el-google-engine --el-default-engine-google))
       (api-key
        (or --el-api-key-google --el-google-api-key))
       (url
        (format "https://generativelanguage.googleapis.com/v1beta/models/%s:streamGenerateContent?alt=sse&key=%s"
                actual-engine api-key))
       ;; Get history in correct format
       (history
        (--el-history-load-recent))
       ;; Build Google-specific payload structure
       (parts
        (apply #'vector
               (append
                ;; Format previous messages as text
                (when history
                  (mapcar
                   (lambda
                     (msg)
                     `(("text" . ,(format "%s: %s"
                                          (cdr
                                           (assoc "role" msg))
                                          (cdr
                                           (assoc "content" msg))))))
                   history))
                ;; Add current prompt
                `((("text" . ,prompt))))))
       (content
        `(("parts" . ,parts)))
       (payload
        (json-encode
         `(("contents" .
            [,content])
           ("generationConfig" .
            (("maxOutputTokens" . 2048))))))
       (escaped-payload
        (shell-quote-argument payload)))
    (format "curl \"%s\" -H \"Content-Type: application/json\" --no-buffer -d %s 2>&1"
            url escaped-payload)))

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
              (when
                  (and text
                       (not
                        (string-empty-p text)))
                ;; Call callback if set
                (when-let
                    ((callback
                      (process-get proc 'callback)))
                  (funcall callback text nil proc))
                ;; Update buffer if target buffer is set
                (when-let
                    ((target-buffer
                      (process-get proc 'target-buffer)))
                  (when
                      (buffer-live-p
                       (get-buffer target-buffer))
                    (with-current-buffer target-buffer
                      (save-excursion
                        (goto-char
                         (point-max))
                        (insert text)))))
                ;; Store content
                (process-put proc 'content
                             (concat
                              (or
                               (process-get proc 'content)
                               "")
                              text))))))))))

(provide 'emacs-llm-call-google)

(when
    (not load-file-name)
  (message "emacs-llm-call-google.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))