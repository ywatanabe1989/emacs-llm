;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 22:21:36>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/tests/test-emacs-llm-keybindings.el

(require 'ert)
(require 'emacs-llm-keybindings)

(ert-deftest test-emacs-llm-keybindings-setup-exists
    ()
  "Test that --el-setup-key-bindings function exists."
  (should
   (fboundp '--el-setup-key-bindings)))

(ert-deftest test-emacs-llm-keybindings-setup-is-interactive
    ()
  "Test that --el-setup-key-bindings is an interactive function."
  (should
   (commandp '--el-setup-key-bindings)))

(ert-deftest test-emacs-llm-keybindings-after-setup-quick-menu
    ()
  "Test that C-c l q is bound to --el-quick-menu after setup."
  (let*
      ((map
        (make-sparse-keymap))
       (prefix-map
        (make-sparse-keymap)))
    (define-key map
                (kbd "C-c")
                prefix-map)
    (define-key prefix-map
                (kbd "l q")
                #'--el-quick-menu)
    (should
     (equal
      (lookup-key map
                  (kbd "C-c l q"))
      #'--el-quick-menu))))

(ert-deftest test-emacs-llm-keybindings-after-setup-on-region
    ()
  "Test that C-c l o is bound to el-run after setup."
  (let*
      ((map
        (make-sparse-keymap))
       (prefix-map
        (make-sparse-keymap)))
    (define-key map
                (kbd "C-c")
                prefix-map)
    (define-key prefix-map
                (kbd "l o")
                #'el-run)
    (should
     (equal
      (lookup-key map
                  (kbd "C-c l o"))
      #'el-run))))

(ert-deftest test-emacs-llm-keybindings-after-setup-switch-provider
    ()
  "Test that C-c l s is bound to el-switch after setup."
  (let*
      ((map
        (make-sparse-keymap))
       (prefix-map
        (make-sparse-keymap)))
    (define-key map
                (kbd "C-c")
                prefix-map)
    (define-key prefix-map
                (kbd "l s")
                #'el-switch)
    (should
     (equal
      (lookup-key map
                  (kbd "C-c l s"))
      #'el-switch))))

(ert-deftest test-emacs-llm-keybindings-after-setup-show-history
    ()
  "Test that C-c l h is bound to el-show-history after setup."
  (let*
      ((map
        (make-sparse-keymap))
       (prefix-map
        (make-sparse-keymap)))
    (define-key map
                (kbd "C-c")
                prefix-map)
    (define-key prefix-map
                (kbd "l h")
                #'el-show-history)
    (should
     (equal
      (lookup-key map
                  (kbd "C-c l h"))
      #'el-show-history))))

(provide 'test-emacs-llm-keybindings)

(when
    (not load-file-name)
  (message "test-emacs-llm-keybindings.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))