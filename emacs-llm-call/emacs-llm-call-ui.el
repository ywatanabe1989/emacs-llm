;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-03-01 17:27:16>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-call/emacs-llm-call-ui.el

(require 'emacs-llm-process)
(require 'emacs-llm-call-variables)
(require 'emacs-llm-call-openai)
(require 'emacs-llm-call-google)
(require 'emacs-llm-call-anthropic)
(require 'emacs-llm-call-deepseek)

(defun el-llm-call
    (prompt &optional template-name provider)
  "Send PROMPT to specified PROVIDER API and display results in a buffer.
OPTIONAL TEMPLATE-NAME is the name of the template used."
  (interactive "sPrompt: ")
  (let*
      ((--el-scroll-to-last-separator)
       (actual-provider
        (or provider --el-default-provider))
       (actual-engine
        (eval
         (intern
          (format "--el-default-engine-%s" actual-provider)))))

    ;; Create display buffer
    (let
        ((buffer-name
          (--el-display-buffer
           prompt actual-provider actual-engine template-name)))

      ;; Define UI callback
      (let
          ((ui-callback
            (lambda
              (text status &rest args)
              (when text
                (with-current-buffer
                    (get-buffer buffer-name)
                  (goto-char
                   (point-max))
                  (insert text)
                  (when --el-scroll-to-last-separator
                    (goto-char
                     (point-max)))))

              ;; Handle completion
              (when
                  (eq status 'finished)
                (--el-stop-spinner)
                (--el-history-append
                 actual-provider
                 (process-get
                  (car args)
                  'content)
                 template-name))

              ;; Handle errors
              (when
                  (eq status 'error)
                (--el-stop-spinner)
                (message "LLM process error: %s"
                         (cadr args))))))

        ;; Start spinner and append to history
        (--el-start-spinner)
        (--el-history-append "user" prompt template-name)

        ;; Call the core function with our UI callback
        (emacs-llm-call-core prompt actual-provider nil template-name ui-callback)))))

;; (defun ui-callback
;;     (text status &optional proc)
;;   "UI callback for LLM responses.
;; TEXT is the response text chunk.
;; STATUS may be 'finished or 'error when process completes.
;; PROC is the process object."
;;   (let
;;       ((buffer-name
;;         (if proc
;;             (when-let
;;                 ((buf
;;                   (process-get proc 'target-buffer)))
;;               (get-buffer buf))
;;           (current-buffer))))
;;     (cond
;;      ;; Handle text chunks
;;      ((and text
;;            (not
;;             (string-empty-p text)))
;;       (when buffer-name
;;         (with-current-buffer buffer-name
;;           (goto-char
;;            (point-max))
;;           (insert text)
;;           (when
;;               (boundp '--el-scroll-to-last-separator)
;;             (when --el-scroll-to-last-separator
;;               (goto-char
;;                (point-max)))))))

;;      ;; Handle completion
;;      ((eq status 'finished)
;;       (when
;;           (fboundp '--el-stop-spinner)
;;         (--el-stop-spinner))
;;       (when proc
;;         (let
;;             ((provider
;;               (process-get proc 'provider))
;;              (content
;;               (process-get proc 'content))
;;              (template-name
;;               (process-get proc 'template)))
;;           (when
;;               (fboundp '--el-history-append)
;;             (--el-history-append provider content template-name)))))

;;      ;; Handle errors
;;      ((eq status 'error)
;;       (when
;;           (fboundp '--el-stop-spinner)
;;         (--el-stop-spinner))
;;       (message "LLM process error: %s"
;;                (or proc "Unknown error"))))))

;; ;; ;; Example usage:
;; ;; (el-switch "openai" "o3-mini-low")
;; ;; (emacs-llm-call-core "Hi there" "anthropic" "claude-3-7-sonnet-20250219" nil #'ui-callback)
;; ;; (emacs-llm-call-core "Hi there" "openai" "o3-mini-low" nil #'ui-callback)
;; ;; (emacs-llm-call-core "Hi there" "deepseek" "deepseek-chat" nil #'ui-callback) ;; does not work
;; ;; (emacs-llm-call-core "Hi there" "google" "gemini-2.0-flash-thinking-exp-01-21" nil #'ui-callback)

(provide 'emacs-llm-call-ui)

(when
    (not load-file-name)
  (message "emacs-llm-call-ui.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))