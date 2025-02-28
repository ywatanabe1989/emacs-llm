;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-03-01 04:40:16>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-call/emacs-llm-call-variables.el

;; General
;; ----------------------------------------

;; GENERAL_INSTRUCTION = f"""
;; ########################################
;; ## General Instruction
;; ########################################
;; I am busy. So,

;; - Please avoid unnecessary messages.
;; - Keep your output minimal.
;; - When programming code is provided, please concentrate on differences between my input and your output; always be concise and stick to the point.
;; - However, do not skip any lines of code as I will use your output as they are, even when your code is long, do not care about it. In such a case I will request you to continue afterwards.
;; - Trailing comment is removed. So, when adding comments, please write in dedicated lines instead of placing at the end of line.
;; - When you return code, please wrap them with triple backquotations with language indicator, like ```python\nCODE\n```
;; ########################################

;; ########################################
;; ## General Instruction
;; ########################################
;; - Avoid unnecessary messages and keep your output minimal (= quiet mode)
;; - For coding:
;; - NEVER SKIP ANY LINES
;; - When the amount of code is substantial, split into rounds and add this continuation tag: `[>> CONTINUED]`
;; - Avoid trailing comments
;; - Wrap code with triple backticks with language indicator, like ```python
;; AWESOME-CODE-HERE
;; ```
;; ########################################

(defcustom el-general-instruction "########################################
## General Instruction
########################################
I am busy. So,

- NEVER INCLUDE UNNECESSARY COMMENTS OTHER THAN DIRECT RESPONSE TO THE REQUEST
  - PLEASE BE QUIET
  - DO NOT DESTRUCT ME
- Please avoid unnecessary messages.
- Keep your output minimal.
- When programming code is provided, please concentrate on differences between my input and your output; always be concise and stick to the point.
- However, do not skip any lines of code as I will use your output as they are, even when your code is long, do not care about it. In such a case I will request you to continue afterwards.
- Trailing comment is removed. So, when adding comments, please write in dedicated lines instead of placing at the end of line.
- When you return code, please wrap them with triple backquotations with language indicator, like ```python\nCODE\n```
- Wrap your thought with this think tag ```think\nYOUR THINKING HERE\n```, as I will remove the section as they arejust destructive to me.
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