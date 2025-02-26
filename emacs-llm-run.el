;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 17:15:33>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-run.el

;;;###autoload
(defun --el-on-region
    ()
  "Run LLM on selected region or prompt for input.
If a region is selected, use that text as the prompt.
Otherwise, prompt the user to enter text."
  (interactive)
  ;; (message "DEBUG: Starting --el-on-region")
  (--el-load-history)
  (let
      ((prompt
        (if
            (use-region-p)
            (prog1
                (buffer-substring-no-properties
                 (region-beginning)
                 (region-end))
              (message "DEBUG: Region detected; using selected text as prompt")
              (deactivate-mark))
          (read-string "Enter prompt: "))))
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

(defun --el-canc--el-timer
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

;; (defun --el-process-sentinel
;;     (proc event)
;;   "Monitor PROC status and handle EVENT completion."
;;   ;; (message "Process %s had event: %s" proc event)
;;   (when
;;       (string-match-p "\\(finished\\|exited\\|failed\\)" event)
;;     (--el-canc--el-timer)
;;     (when
;;         (process-live-p proc)
;;       (kill-process proc))
;;     (let
;;         ((temp-buffer
;;           (process-get proc 'temp-buffer))
;;          (content
;;           (or
;;            (process-get proc 'content)
;;            ""))
;;          (prompt
;;           (process-get proc 'prompt))
;;          (provider
;;           (process-get proc 'provider))
;;          (model
;;           (process-get proc 'model))
;;          (template
;;           (process-get proc 'template)))
;;       (when
;;           (buffer-live-p temp-buffer)
;;         (kill-buffer temp-buffer))
;;       ;; Display final response with context
;;       (when
;;           (and content prompt provider model)
;;         (--el-display-with-context prompt template content provider model))
;;       (--el-append-to-history "assistant" content)
;;       (run-hooks '--el-completion-hook))))

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
  (message "emacs-llm-run.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))