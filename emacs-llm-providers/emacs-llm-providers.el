;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 14:01:09>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-providers/emacs-llm-providers.el

(require 'emacs-llm-providers-variables)
(require 'emacs-llm-providers-openai)
(require 'emacs-llm-providers-google)
(require 'emacs-llm-providers-anthropic)
(require 'emacs-llm-providers-deepseek)

(provide 'emacs-llm-providers)

(when
    (not load-file-name)
  (message "emacs-llm-providers.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))