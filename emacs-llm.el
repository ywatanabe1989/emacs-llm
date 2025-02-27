;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-27 09:37:17>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm.el

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
