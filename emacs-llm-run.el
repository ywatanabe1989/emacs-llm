;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 19:00:24>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/_emacs-llm-run-with-dired.el

;;;###autoload
(defun el-run
    (&optional prompt)
  "Run El command on selected region, dired files or prompt.
If a region is selected, use that text as the prompt.
If in dired-mode with marked files, concatenate their contents.
Otherwise, prompt the user to enter a prompt.
The response will be displayed in the *El* buffer."
  (interactive)
  (--el-load-history)
  (let*
      ((prompt
        (or prompt
            (cond
             ((use-region-p)
              (buffer-substring-no-properties
               (region-beginning)
               (region-end)))
             ((eq major-mode 'dired-mode)
              (--el-dired-get-contents))
             (t
              (read-string "Enter prompt: " ""))))))
    ;; Sanitization isn't needed here - it should be done in the provider functions
    ;; where the prompt is actually passed to shell commands
    (message "DEBUG: Prompt is: %s" prompt)
    (message "DEBUG: --el-provider is: %s" --el-provider)
    (pcase --el-provider
      ("openai"
       (--el-openai-stream prompt))
      ("anthropic"
       (--el-anthropic-stream prompt))
      ("google"
       (--el-google-stream prompt))
      ("deepseek"
       (--el-deepseek-stream prompt))
      (_
       (message "DEBUG: Provider %s not fully supported yet" --el-provider)))))

;; (defun el-run
;;     ()
;;   "Run El command on selected region, dired files or prompt.
;; If a region is selected, use that text as the prompt.
;; If in dired-mode with marked files, concatenate their contents.
;; Otherwise, prompt the user to enter a prompt.
;; The response will be displayed in the *El* buffer."
;;   (interactive)
;;   (--el-load-history)
;;   (let*
;;       ((prompt
;;         (cond
;;          ((use-region-p)
;;           (buffer-substring-no-properties
;;            (region-beginning)
;;            (region-end)))
;;          ((eq major-mode 'dired-mode)
;;           (--el-dired-get-contents))
;;          (t
;;           (read-string "Enter prompt: " ""))))
;;        (prompt
;;         (prog1 prompt))
;;        )
;;     (message "DEBUG: Prompt is: %s" prompt)
;;     (message "DEBUG: --el-provider is: %s" --el-provider)
;;     (pcase --el-provider
;;       ("openai"
;;        (--el-openai-stream prompt))
;;       ("anthropic"
;;        (--el-anthropic-stream prompt))
;;       ("google"
;;        (--el-google-stream prompt))
;;       ("deepseek"
;;        (--el-deepseek-stream prompt))
;;       (_
;;        (message "DEBUG: Provider %s not fully supported yet" --el-provider)))))

;; DEBUG: OpenAI filter received chunk of length 4095
;; DEBUG: Full chunk: /bin/bash: -c: line 1: syntax error near unexpected token `;;'

(defun --el-dired-get-contents
    ()
  "Get contents of marked files recursively, handling only safe files. If no files specified, just call ordinal el-on-region"
  (interactive)
  (let*
      ((marked-files
        (dired-get-marked-files nil nil
                                (lambda
                                  (f)
                                  (dired-file-marker f))))
       (safe-extensions
        '(".el" ".py" ".sh" ".src" ".txt" ".md" ".org" ".yml" ".yaml" ".json"))
       (size-limit
        (* 1024 1024))
       (contents ""))
    (if
        (null marked-files)
        (read-string "Enter prompt: " "")
      (cl-labels
          ((process-file
             (file)
             (cond
              ((file-directory-p file)
               (dolist
                   (f
                    (directory-files file t "^[^.]"))
                 (process-file f)))
              ((and
                (file-regular-p file)
                (member
                 (file-name-extension file t)
                 safe-extensions)
                (<
                 (file-attribute-size
                  (file-attributes file))
                 size-limit))
               (setq contents
                     (concat contents
                             (format "\n\n;;; ----- %s -----\n\n" file)
                             (with-temp-buffer
                               (insert-file-contents file)
                               (buffer-string))))))))
        (dolist
            (file marked-files)
          (process-file file)))
      contents)))

;; ;;;###autoload
;; (defun el-run
;;     ()
;;   "Run LLM on selected region or prompt for input.
;; If a region is selected, use that text as the prompt.
;; Otherwise, prompt the user to enter text."
;;   (interactive)
;;   ;; (message "DEBUG: Starting el-run")
;;   (--el-load-history)
;;   (let
;;       ((prompt
;;         (if
;;             (use-region-p)
;;             (prog1
;;                 (buffer-substring-no-properties
;;                  (region-beginning)
;;                  (region-end))
;;               (message "DEBUG: Region detected; using selected text as prompt")
;;               (deactivate-mark))
;;           (read-string "Enter prompt: "))))
;;     (message "DEBUG: Prompt is: %s" prompt)
;;     (message "DEBUG: --el-provider is: %s" --el-provider)
;;     (pcase --el-provider
;;       ("openai"
;;        (--el-openai-stream prompt))
;;       ("anthropic"
;;        (--el-anthropic-stream prompt))
;;       ("google"
;;        (--el-google-stream prompt))
;;       ("deepseek"
;;        (--el-deepseek-stream prompt))
;;       (_
;;        (message "DEBUG: Provider %s not fully supported yet" --el-provider)))))

(defun --el-cancel-timer
    ()
  "Cancel the LLM spinner timer if active."
  (when --el-spinner-timer
    (cancel-timer --el-spinner-timer)
    (setq --el-spinner-timer nil)))

(defun --el-process-chunk
    (proc chunk parse-func)
  "Process CHUNK from PROC using PARSE-FUNC to extract content."
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

    ;; Process each complete line
    (dolist
        (line lines)
      (when
          (string-prefix-p "data: " line)
        (let*
            ((json-data
              (substring line 6))
             (text
              (when
                  (not
                   (string= json-data "[DONE]"))
                (funcall parse-func json-data))))

          (when text
            (with-current-buffer
                (process-get proc 'target-buffer)
              (goto-char
               (point-max))
              (insert text))

            ;; Accumulate content
            (process-put proc 'content
                         (concat
                          (or
                           (process-get proc 'content)
                           "")
                          text))))))))

(defun --el-process-sentinel
    (proc event)
  "Process sentinel for PROC handling EVENT."
  (message "Process %s received event: %s"
           (process-name proc)
           event)
  (when
      (string-match-p "\\(finished\\|exited\\|failed\\)" event)
    (--el-stop-spinner)
    (let
        ((prompt
          (process-get proc 'prompt))
         (provider
          (process-get proc 'provider))
         (model
          (process-get proc 'model))
         (template
          (process-get proc 'template))
         (response
          (process-get proc 'content)))

      (when
          (and prompt provider)
        ;; Only append to history after we have a complete response
        (--el-append-to-history "user" prompt template)
        (when
            (not
             (string-empty-p response))
          (--el-append-to-history "assistant" response template)))

      ;; Clean up temp buffer
      (when-let
          ((tb
            (process-get proc 'temp-buffer)))
        (when
            (buffer-live-p tb)
          (kill-buffer tb))))))

(provide 'emacs-llm-run)

(when
    (not load-file-name)
  (message "_emacs-llm-run-with-dired.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
