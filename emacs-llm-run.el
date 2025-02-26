;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 15:14:16>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-run.el

(defun --el--canc--el-timer
    ()
  "Cancel the LLM spinner timer if active."
  (when --el-spinner-timer
    (cancel-timer --el-spinner-timer)
    (setq --el-spinner-timer nil)))

(defun --el--process-sentinel
    (proc event)
  "Monitor PROC status and handle EVENT completion."
  ;; (message "Process %s had event: %s" proc event)
  (when
      (string-match-p "\\(finished\\|exited\\|failed\\)" event)
    (--el--canc--el-timer)
    (when
        (process-live-p proc)
      (kill-process proc))
    (let
        ((temp-buffer
          (process-get proc 'temp-buffer)))
      (when
          (buffer-live-p temp-buffer)
        (kill-buffer temp-buffer)))
    (--el--append-to-history "assistant"
                             (or
                              (process-get proc 'content)
                              ""))
    (run-hooks '--el-completion-hook)))

;;;###autoload
(defun --el-on-region
    ()
  "Run LLM on selected region or prompt for input.
If a region is selected, use that text as the prompt.
Otherwise, prompt the user to enter text."
  (interactive)
  ;; (message "DEBUG: Starting --el-on-region")
  (--el--load-history)
  (let
      ((prompt
        (if
            (use-region-p)
            (prog1
                (buffer-substring-no-properties
                 (region-beginning)
                 (region-end))
              ;; (message "DEBUG: Region detected; using selected text as prompt")
              (deactivate-mark))
          (read-string "Enter prompt: "))))
    ;; (message "DEBUG: Prompt is: %s" prompt)
    ;; (message "DEBUG: --el-provider is: %s" --el-provider)
    (pcase --el-provider
      ("openai"
       (--el--send-openai-stream prompt))
      ("anthropic"
       (--el--send-anthropic-stream prompt))
      ("google"
       (--el--send-google-stream prompt))
      ("deepseek"
       (--el--send-deepseek-stream prompt))
      (_
       (message "DEBUG: Provider %s not fully supported yet" --el-provider)))))

;; (defun --el--process-sentinel
;;     (proc event)
;;   "Sentinel function for LLM streaming processes.
;; It stops the spinner when the process finishes."
;;   ;; (message "DEBUG: Process %s received event: %s" proc event)
;;   (when
;;       (or
;;        (string-match-p "finished" event)
;;        (string-match-p "exited" event)
;;        (string-match-p "signal" event))
;;     (--el--stop-spinner)
;;     (with-current-buffer
;;         (process-get proc 'target-buffer)
;;       (goto-char
;;        (point-max))
;;       ;; (insert "\n\nLLM process finished.\n")
;;       )))

(defun --el--process-chunk
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

(provide 'emacs-llm-run)

(when
    (not load-file-name)
  (message "emacs-llm-run.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))