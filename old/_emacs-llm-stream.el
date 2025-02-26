;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 12:12:27>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-stream.el

(require 'emacs-llm-core)

(defun el--send-request-stream
    (prompt)
  "Send the PROMPT to the AI API using streaming."
  (interactive)
  (let*
      ((url "https://api.openai.com/v1/chat/completions")
       (buffer-name
        (get-buffer-create el-buffer-name))
       (payload
        (json-encode
         `(("model" . ,el-model)
           ("messages" . ,(append el-history
                                  `((("role" . "user")
                                     ("content" . ,prompt)))))
           ("stream" . t)
           ("temperature" . ,el-temperature)
           ("max_tokens" . ,el-max-tokens))))
       (args
        (list "-N"
              url
              "-H" "Content-Type: application/json"
              "-H"
              (concat "Authorization: Bearer " el-api-key)
              "-d" payload))
       (proc
        (apply #'start-process "el-stream-curl" "*el-stream-curl*" "curl" args))
       (content ""))

    ;; Set up display buffer
    (with-current-buffer buffer-name
      (read-only-mode -1)
      (goto-char
       (point-max))
      (insert "\n\n")
      (insert
       (propertize "AI:" 'face 'bold))
      (insert "\n\n")
      (read-only-mode 1)
      (display-buffer
       (current-buffer)))

    ;; Filter function to process streaming output
    (set-process-filter
     proc
     (lambda
       (proc chunk)
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
                 (let*
                     ((json-object-type 'alist)
                      (json-array-type 'vector)
                      (json-key-type 'symbol)
                      (data
                       (condition-case err
                           (json-read-from-string jsonstr)
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
                                        (aref choices 0))))
                          (text
                           (alist-get 'content delta)))
                       (when text
                         (setq content
                               (concat content text))
                         (with-current-buffer buffer-name
                           (let
                               ((buffer-read-only nil))
                             (goto-char
                              (point-max))
                             (insert text))))))))))))))

    ;; Process sentinel to handle completion
    (set-process-sentinel
     proc
     (lambda
       (proc event)
       (when
           (string-match "finished" event)
         (el--append-to-history "assistant" content)
         (message "OpenAI streaming completed."))))

    (el--append-to-history "user" prompt)))

(provide 'emacs-llm-stream)

(when
    (not load-file-name)
  (message "emacs-llm-stream.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))