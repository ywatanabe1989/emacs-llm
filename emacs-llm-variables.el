;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 14:39:55>
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

(provide 'emacs-llm-variables)

(when
    (not load-file-name)
  (message "emacs-llm-variables.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))