;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 14:19:23>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-variables.el

(require 'json)
(require 'markdown-mode)
(require 'cl-lib)

;;;; Customization

(defgroup emacs-llm nil
  "Interface to interact with LLMs from Emacs."
  :group 'applications
  :prefix "--el-")

(defcustom --el-buffer-name "*Emacs-LLM*"
  "Name of the buffer for displaying LLM outputs."
  :type 'string
  :group 'emacs-llm)

(defcustom --el-default-provider "google"
  "Default provider"
  :type 'string
  :group 'el)

(defcustom --el-default-engine "gemini-2.0-flash-thinking-exp-01-21"
  "Default engine"
  :type 'string
  :group 'el)

;; (defcustom --el-api-key
;;   (getenv "OPENAI_API_KEY")
;;   "API key for OpenAI services."
;;   :type 'string
;;   :group 'emacs-llm)

;; (defcustom --el-model "gpt-4o"
;;   "Default model to use for OpenAI."
;;   :type 'string
;;   :group 'emacs-llm)

(defcustom --el-temperature 0.7
  "Temperature setting for LLM responses."
  :type 'float
  :group 'emacs-llm)

(defcustom --el-max-tokens 4096
  "Maximum number of tokens for LLM responses."
  :type 'integer
  :group 'emacs-llm)

(defcustom --el-provider "openai"
  "Default LLM provider to use."
  :type
  '(choice
    (const :tag "OpenAI" "openai")
    (const :tag "Anthropic" "anthropic")
    (const :tag "Google" "google")
    (const :tag "DeepSeek" "deepseek"))
  :group 'emacs-llm)

(defcustom --el-use-stream t
  "Whether to use streaming APIs by default."
  :type 'boolean
  :group 'emacs-llm)

(defcustom --el-history-file
  (expand-file-name "--el-history.json" "/home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/")
  "File path to save conversation history."
  :type 'string
  :group 'emacs-llm)

(defvar --el-history
  '()
  "List to keep track of conversation history.")

(defconst --el-separator "------------------------------------------------------------------------"
  "Separator for LLM outputs in buffer.")

(provide 'emacs-llm-variables)

(when
    (not load-file-name)
  (message "emacs-llm-variables.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))