;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-28 08:41:34>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-call/emacs-llm-call.el

(require 'emacs-llm-call-variables)
(require 'emacs-llm-call-variables-api)
(require 'emacs-llm-call-openai)
(require 'emacs-llm-call-google)
(require 'emacs-llm-call-anthropic)
(require 'emacs-llm-call-deepseek)
(require 'emacs-llm-call-any)

(provide 'emacs-llm-call)

(when
    (not load-file-name)
  (message "emacs-llm-call.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))