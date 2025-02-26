;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 16:42:10>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/tests/test-emacs-llm-bindings.el

(require 'ert)
(require 'emacs-llm-bindings)

(ert-deftest test-emacs-llm-bindings-setup-exists
    ()
  "Test that --el-setup-key-bindings function exists."
  (should
   (fboundp '--el-setup-key-bindings)))

(ert-deftest test-emacs-llm-bindings-setup-is-interactive
    ()
  "Test that --el-setup-key-bindings is an interactive function."
  (should
   (commandp '--el-setup-key-bindings)))

(ert-deftest test-emacs-llm-bindings-after-setup-quick-menu
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

(ert-deftest test-emacs-llm-bindings-after-setup-on-region
    ()
  "Test that C-c l o is bound to --el-on-region after setup."
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
                #'--el-on-region)
    (should
     (equal
      (lookup-key map
                  (kbd "C-c l o"))
      #'--el-on-region))))

(ert-deftest test-emacs-llm-bindings-after-setup-switch-provider
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

(ert-deftest test-emacs-llm-bindings-after-setup-show-history
    ()
  "Test that C-c l h is bound to --el-show-history after setup."
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
                #'--el-show-history)
    (should
     (equal
      (lookup-key map
                  (kbd "C-c l h"))
      #'--el-show-history))))

(ert-deftest test-emacs-llm-bindings-after-setup-copy-last-response
    ()
  "Test that C-c l c is bound to --el-copy-last-response after setup."
  (let*
      ((map
        (make-sparse-keymap))
       (prefix-map
        (make-sparse-keymap)))
    (define-key map
                (kbd "C-c")
                prefix-map)
    (define-key prefix-map
                (kbd "l c")
                #'--el-copy-last-response)
    (should
     (equal
      (lookup-key map
                  (kbd "C-c l c"))
      #'--el-copy-last-response))))

(ert-deftest test-emacs-llm-bindings-global-C-M-S-g
    ()
  "Test that C-M-S-g is bound to --el-on-region."
  (should
   (eq
    (global-key-binding
     (kbd "C-M-S-g"))
    '--el-on-region)))

(provide 'test-emacs-llm-bindings)

;;; test-emacs-llm-bindings.el ends here

(provide 'test-emacs-llm-bindings)

(when
    (not load-file-name)
  (message "test-emacs-llm-bindings.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))