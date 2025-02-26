;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 14:19:24>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-providers/emacs-llm-providers-variables.el

;; Used
;; ----------------------------------------

;; (defcustom --el-google-api-key
;;   (getenv "GOOGLE_API_KEY")
;;   "API key for Google Gemini services."
;;   :type 'string
;;   :group 'el)

;; (defcustom --el-deepseek-api-key
;;   (getenv "DEEPSEEK_API_KEY")
;;   "API key for Deepseek Gemini services."
;;   :type 'string
;;   :group 'el)

;; (defcustom --el-anthropic-model "claude-3-7-sonnet-20250219"
;;   "Default model to use for Anthropic Claude."
;;   :type 'string
;;   :group 'el)

;; (defvar --el-openai-models
;;   '("gpt-4o" "gpt-4-turbo" "gpt-3.5-turbo" "gpt-4" "o1-mini" "o1" "o3-mini")
;;   "Available OpenAI models.")

;; (defvar --el-anthropic-models
;;   '("claude-3-7-sonnet-20250219" "claude-3-5-sonnet-20241022" "claude-3-5-haiku-20241022")
;;   "Available Anthropic Claude models.")

;; (defvar --el-google-models
;;   '("gemini-2.0-flash" "gemini-2.0-pro" "gemini-1.5-pro")
;;   "Available Google Gemini models.")

;; (defvar --el-deepseek-models
;;   '("deepseek-chat" "deepseek-coder" "deepseek-reasoner")
;;   "Available DeepSeek models.")

;; Timeout
;; ----------------------------------------

(defvar --el-request-timeout 300
  "Timeout in seconds for LLM API requests")

;; API keys
;; ----------------------------------------

(defcustom --el-api-key-openai
  (getenv "OPENAI_API_KEY")
  "API key for OpenAI."
  :type 'string
  :risky t
  :group 'el)

(defcustom --el-api-key-anthropic
  (getenv "ANTHROPIC_API_KEY")
  "API key for Anthropic Claude."
  :type 'string
  :risky t
  :group 'el)

(defcustom --el-api-key-google
  (getenv "GOOGLE_API_KEY")
  "API key for Google Claude."
  :type 'string
  :risky t
  :group 'el)

(defcustom --el-api-key-deepseek
  (getenv "DEEPSEEK_API_KEY")
  "API key for DeepSeek."
  :type 'string
  :risky t
  :group 'el)

(defcustom --el-api-key-groq
  (getenv "GROQ_API_KEY")
  "API key for Groq."
  :type 'string
  :risky t
  :group 'el)

;; Default Engines
;; ----------------------------------------

(defcustom --el-default-engine-openai
  (getenv "OPENAI_ENGINE")
  "Deafult model for OpenAI."
  :type 'string
  :group 'el)

(defcustom --el-default-engine-anthropic
  (getenv "ANTHROPIC_ENGINE")
  "Deafult model for Anthropic Claude."
  :type 'string
  :group 'el)

(defcustom --el-default-engine-google
  (getenv "GOOGLE_ENGINE")
  "Deafult model for Google Claude."
  :type 'string
  :group 'el)

(defcustom --el-default-engine-deepseek
  (getenv "DEEPSEEK_ENGINE")
  "Deafult model for DeepSeek."
  :type 'string
  :group 'el)

(defcustom --el-default-engine-groq
  (getenv "GROQ_ENGINE")
  "Deafult model for Groq."
  :type 'string
  :group 'el)

;; Available Engines and Max Tokens
;; ----------------------------------------

(defvar --el-openai-engine-max-tokens-alist
  '(("o3-mini" . 100000)
    ("o3-mini-low" . 100000)
    ("o3-mini-medium" . 100000)
    ("o3-mini-high" . 100000)
    ("gpt-4o" . 8192))
  "Alist mapping OpenAI engines to their max token limits.")

(defvar --el-anthropic-engine-max-tokens-alist
  '(("claude-3-5-sonnet-20241022" . 8192)
    ("claude-3-5-haiku-20241022" . 4096))
  "Alist mapping Anthropic engines to their max token limits.")

(defvar --el-google-engine-max-tokens-alist
  '(("gemini-2.0-flash-exp" . 100000)
    ("gemini-2.0-flash" . 100000)
    ("gemini-2.0-flash-lite-preview-02-05" . 100000)
    ("gemini-2.0-pro-exp-02-05" . 100000)
    ("gemini-2.0-flash-thinking-exp-01-21" . 100000))
  "Alist mapping Google engines to their max token limits.")

(defvar --el-deepseek-engine-max-tokens-alist
  '(("deepseek-chat" . 8192)
    ("deepseek-coder" . 8192)
    ("deepseek-reasoner" . 8192))
  "Alist mapping Deepseek engines to their max token limits.")

(defvar --el-groq-engine-max-tokens-alist
  '(("qwen-2.5-32b" . 8192)
    ("deepseek-r1-distill-qwen-32b" . 16384)
    ("deepseek-r1-distill-llama-70b-specdec" . 16384)
    ("deepseek-r1-distill-llama-70b" . nil))
  "Alist mapping Groq engines to their max token limits.")

;; Available Engines
;; ----------------------------------------

(defvar --el-openai-available-engines
  (mapcar #'car --el-openai-engine-max-tokens-alist)
  "List of available models for the openai provider.")

(defvar --el-anthropic-available-engines
  (mapcar #'car --el-anthropic-engine-max-tokens-alist)
  "List of available models for the anthropic provider.")

(defvar --el-google-available-engines
  (mapcar #'car --el-google-engine-max-tokens-alist)
  "List of available models for the google provider.")

(defvar --el-deepseek-available-engines
  (mapcar #'car --el-deepseek-engine-max-tokens-alist)
  "List of available models for the deepseek provider.")

(defvar --el-groq-available-engines
  (mapcar #'car --el-groq-engine-max-tokens-alist)
  "List of available models for the groq provider.")

(defcustom --el-anthropic-api-key
  (getenv "ANTHROPIC_API_KEY")
  "API key for Anthropic services."
  :type 'string
  :group 'el)

(provide 'emacs-llm-providers-variables)

(when
    (not load-file-name)
  (message "emacs-llm-providers-variables.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))