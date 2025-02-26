;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 13:19:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-run.el

;;;###autoload
(defun el-on-region
    ()
  "Run LLM on selected region or prompt for input.
If a region is selected, use that text as the prompt.
Otherwise, prompt the user to enter text."
  (interactive)
  ;; (message "DEBUG: Starting el-on-region")
  (el--load-history)
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
    ;; (message "DEBUG: el-provider is: %s" el-provider)
    (pcase el-provider
      ("openai"
       ;; (message "DEBUG: Routing to OpenAI stream")
       (el--send-openai-stream prompt))
      ("anthropic"
       ;; (message "DEBUG: Routing to Anthropic stream")
       (el--send-anthropic-stream prompt))
      (_
       (message "DEBUG: Provider %s not fully supported yet" el-provider)))))

(provide 'emacs-llm-run)

(when
    (not load-file-name)
  (message "emacs-llm-run.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))