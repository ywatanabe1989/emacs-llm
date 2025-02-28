;;; ----- /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm.el -----
;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-03-01 04:11:24>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm.el
;; Copyright (C) 2024 Yusuke Watanabe
;; Author: Yusuke Watanabe <ywatanabe@alumni.u-tokyo.ac.jp>
;; Maintainer: Yusuke Watanabe <ywatanabe@alumni.u-tokyo.ac.jp>
;; Created: 7 Jul 2024
;; URL: https://github.com/ywatanabe1989/emacs-llm
;; Version: 1.3
;; License: GNU General Public License (GPL) version 3
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "25.1") (markdown-mode "2.6") (pulse "1.0"))
;; Keywords: tools, LLM, template
;;; Commentary:
;;
;; `emacs-llm.el` provides an interface to interact with large language models
;; (LLMs) such as ChatGPT, Gemini, Claude, Llama.
;; Features:
;;
;; - LLM queries are constructed from the region or manual typing
;;
;; - Templates for prompts are customizable
;;
;; - LLM outputs are displayed in a streaming manner
;;
;; - Markup is enabled using markdown mode
;;
;; - Code blocks provided by LLM can be navigated and automatically
;;   copied to the kill ring
;;
;; - Conversation history as both human- and AI-readable files is stored,
;;   independently from chat history applied to LLMs
;; Usage:
;;
;; - M-x el-run:
;;       Run LLM on selected text. If no region is selected,
;;       a manual prompt will be requested.
;;
;; - M-x el-history-show:
;;       Display conversation history
;;
;; - M-n (on "Emacs-LLM" buffer):
;;       Navigate to the "next" code block and copy the content to the kill ring
;;
;; - M-p (on "Emacs-LLM" buffer):
;;       Navigate to the "previous" code block and copy the content to the kill ring
;;; Sample configuration:
;;
;; (require 'emacs-llm)
;;
;; ;; Model (OpenAI, Gemini, Claude, Deepseek, or other models)
;; (setq --el-api-key-openai (getenv "OPENAI_API_KEY"))
;; (setq --el-openai-engine "gpt-4o")
;;
;; ;; Model Parameters
;; (setq --el-n-histories "5")
;; (setq --el-max-tokens "2000")
;; (setq --el-temperature "0")
;;
;; ;; Templates:
;;
;; Templates are markdown files (*.md) stored in './templates' directory
;; (default: load-path/emacs-llm/templates/).
;;
;; Template Selection:
;; 1. Default: First uppercase letter of filename is used as shortcut
;; 2. Custom: Define your own shortcuts in el-template-mapping:
;;    (setq el-template-mapping
;;          '(("p" . "Program")                ; p -> Program.md
;;            ("e" . "Email")                  ; s -> SciWrite.md
;;            ("c" . "Correct")                ; c -> Correct.md
;;            ("my" . "MyAwesomeTemplate")))   ; my -> MyAwesometemplate.md
;;
;; Note: Templates should contain "PLACEHOLDER" where your input will be inserted.
;;
;; ;; Key Bindings
;; (--el-setup-key-bindings)  ; Sets up C-M-g for el-run
;;; Code:

;; (add-to-list 'load-path "/home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/")

;; Emacs LLM package
;; ----------------------------------------

;; Preparation
(require 'emacs-llm-path-and-load)

;; LLM
(require 'emacs-llm-variables)
(require 'emacs-llm-display)
(require 'emacs-llm-templates)
(require 'emacs-llm-call)
(require 'emacs-llm-switch)

;; Utils
(require 'emacs-llm-spinner)
(require 'emacs-llm-scroll)
(require 'emacs-llm-code-navigation)
(require 'emacs-llm-history)

;; Dired
(require 'emacs-llm-filter)
(require 'emacs-llm-dired)

;; Main
(require 'emacs-llm-run)
(require 'emacs-llm-keybindings)

(provide 'emacs-llm)

(when
    (not load-file-name)
  (message "emacs-llm.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
