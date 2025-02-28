;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-03-01 05:51:22>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/playground.el

(setq prompt "Now, I am switching LLM models. Without thinking about the past conversation, what is your name: gemini, claude, gpt, or deepseek?")

(el-switch "google" "gemini-2.0-flash-thinking-exp-01-21")
(el-run prompt nil)

(el-switch "anthropic" "claude-3-7-sonnet-20250219")
(el-run prompt nil)

(el-switch "openai" "o3-mini-high")
(el-run prompt nil)

(el-switch "deepseek" "deepseek-reasoner")
(el-run prompt nil)

(provide 'playground)

(when
    (not load-file-name)
  (message "playground.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))