;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 21:16:48>
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

(defcustom --el-use-stream t
  "Whether to use streaming APIs by default."
  :type 'boolean
  :group 'emacs-llm)

(defconst --el-separator "------------------------------------------------------------------------"
  "Separator for LLM outputs in buffer.")

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

(provide 'emacs-llm-variables)

(when
    (not load-file-name)
  (message "emacs-llm-variables.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))