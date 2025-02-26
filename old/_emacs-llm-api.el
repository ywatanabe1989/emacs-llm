;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 12:12:05>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-api.el

;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 12:09:46>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-api.el
(require 'emacs-llm-core)

(defun el--construct-request-data
    (prompt)
  "Construct the JSON data for the API request with PROMPT."
  (let
      ((messages
        (append el-history
                `((("role" . "user")
                   ("content" . ,prompt))))))
    (json-encode
     `(("model" . ,el-model)
       ("messages" . ,messages)
       ("temperature" . ,el-temperature)
       ("max_tokens" . ,el-max-tokens)))))

(defun el--handle-api-response
    (response)
  "Handle the API RESPONSE and display it."
  (let*
      ((choices
        (alist-get 'choices response))
       (first-choice
        (elt choices 0))
       (msg
        (alist-get 'message first-choice))
       (content
        (alist-get 'content msg)))
    (when content
      (el--append-to-history "assistant" content)
      (el--display-response content))))

(defun el--handle-api-error
    (error-thrown)
  "Handle API errors represented by ERROR-THROWN."
  (message "Emacs-LLM API Error: %s" error-thrown))

(defun el--send-request
    (prompt)
  "Send the PROMPT to the AI API."
  (let
      ((url "https://api.openai.com/v1/chat/completions")
       (data
        (el--construct-request-data prompt)))
    (request
      url
      :type "POST"
      :headers
      `(("Content-Type" . "application/json")
        ("Authorization" . ,(concat "Bearer " el-api-key)))
      :data data
      :parser 'json-read
      :success
      (cl-function
       (lambda
         (&key data &allow-other-keys)
         (el--handle-api-response data)))
      :error
      (cl-function
       (lambda
         (&key error-thrown &allow-other-keys)
         (el--handle-api-error error-thrown))))))

(provide 'emacs-llm-api)

(when
    (not load-file-name)
  (message "emacs-llm-api.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))