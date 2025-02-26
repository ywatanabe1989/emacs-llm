;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-27 09:41:49>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-call/emacs-llm-call-variables.el

;; General
;; ----------------------------------------

(defcustom el-general-instruction "
########################################
## General Instruction
########################################
- Avoid unnecessary messages and keep your output minimal (= quiet mode)
- For coding:
  - NEVER SKIP ANY LINES
  - When the amount of code is substantial, split into rounds and add this continuation tag: `[>> CONTINUED]`
  - Avoid trailing comments
  - Wrap code with triple backticks with language indicator, like ```python
AWESOME-CODE-HERE
```
########################################
"
  "General instruction preceding every input"
  :type 'string
  :group 'emacs-llm)

(defcustom --el-request-timeout 300
  "Timeout in seconds for LLM API requests"
  :type 'int
  :group 'emacs-llm)

(defcustom --el-temperature 0.7
  "Temperature setting for LLM responses."
  :type 'float
  :group 'emacs-llm)

(defcustom --el-max-tokens 4096
  "Maximum number of tokens for LLM responses."
  :type 'integer
  :group 'emacs-llm)

(provide 'emacs-llm-call-variables)

(when
    (not load-file-name)
  (message "emacs-llm-call-variables.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))