;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-03-01 05:45:17>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-llm/emacs-llm-switch.el

;;;###autoload

(defun el-switch
    (&optional provider engine)
  "Switch the LLM provider and select a engine.
If ENGINE is provided, use it directly without prompting."
  (interactive)
  (let
      ((selected-provider
        (or provider
            (completing-read "Select LLM provider: "
                             '("openai" "anthropic" "google" "deepseek")
                             nil t))))
    (setq --el-default-provider selected-provider)
    (let*
        ((provider-engines
          (cond
           ((string= selected-provider "openai")
            --el-openai-engines)
           ((string= selected-provider "anthropic")
            --el-anthropic-engines)
           ((string= selected-provider "google")
            --el-google-engines)
           ((string= selected-provider "deepseek")
            --el-deepseek-engines)))
         (selected-engine
          (cond
           ;; If engine is provided and is valid for this provider, use it
           ((and engine
                 (member engine provider-engines))
            engine)
           ;; Otherwise prompt for engine selection
           (t
            (completing-read "Select engine: " provider-engines nil t)))))
      (cond
       ((string= selected-provider "openai")
        (setq --el-openai-engine selected-engine))
       ((string= selected-provider "anthropic")
        (setq --el-anthropic-engine selected-engine))
       ((string= selected-provider "google")
        (setq --el-google-engine selected-engine))
       ((string= selected-provider "deepseek")
        (setq --el-deepseek-engine selected-engine))
       )
      (message "Switched to: %s (by %s)" selected-engine selected-provider))))

;;     (message "Switched to %s using engine: %s" provider selected-engine)))

(provide 'emacs-llm-switch)

(when
    (not load-file-name)
  (message "emacs-llm-switch.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
