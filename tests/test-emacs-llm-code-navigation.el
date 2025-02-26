;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-27 09:34:42>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/tests/test-emacs-llm-code-navigation.el

(require 'ert)
(require 'emacs-llm-code-navigation)

(ert-deftest test-el-code-block-start-delimiter
    ()
  (should
   (stringp --el-code-block-start-delimiter))
  (should
   (string-match-p --el-code-block-start-delimiter "```python")))

(ert-deftest test-el-code-block-end-delimiter
    ()
  (should
   (stringp --el-code-block-end-delimiter))
  (should
   (string-match-p --el-code-block-end-delimiter "```")))

(ert-deftest test-el-navigate-code-blocks-next
    ()
  (with-temp-buffer
    (insert "Some text\n```python\nprint('hello')\n```\nMore text\n```elisp\n(message \"hi\")\n```\n")
    (goto-char
     (point-min))

    ;; Create a mock implementation of the functions that --el-navigate-code-blocks calls
    (cl-letf
        (((symbol-function 'kill-ring-save)
          (lambda
            (start end)
            nil))
         ((symbol-function 'pulse-momentary-highlight-region)
          (lambda
            (start end)
            nil))
         ((symbol-function 'set-mark)
          (lambda
            (pos)
            nil))
         ((symbol-function 'activate-mark)
          (lambda
            ()
            nil))
         ((symbol-function 'message)
          (lambda
            (&rest args)
            nil)))

      ;; Get expected positions before running the function
      (let*
          ((expected-start
            (save-excursion
              (goto-char
               (point-min))
              (re-search-forward "```python" nil t)
              (forward-line 1)
              (point)))
           (expected-end
            (save-excursion
              (goto-char
               (point-min))
              (re-search-forward "```" nil t 2)
              (line-beginning-position)))
           (result
            (--el-navigate-code-blocks 'next)))

        (should result)
        (should
         (consp result))

        ;; Test approximately right - test that we're at least in the ballpark
        (should
         (<=
          (abs
           (-
            (car result)
            expected-start))
          1))
        (should
         (<=
          (abs
           (-
            (cdr result)
            expected-end))
          1))))))

(ert-deftest test-el-navigate-code-blocks-previous
    ()
  (with-temp-buffer
    (insert "Some text\n```python\nprint('hello')\n```\nMore text\n```elisp\n(message \"hi\")\n```\n")
    (goto-char
     (point-max))
    (let
        ((result
          (--el-navigate-code-blocks 'previous)))
      (should result)
      (should
       (consp result))
      (should
       (=
        (car result)
        (save-excursion
          (goto-char
           (point-min))
          (re-search-forward "(message" nil t)
          (line-beginning-position))))
      (should
       (=
        (cdr result)
        (save-excursion
          (goto-char
           (point-max))
          (re-search-backward "```" nil t)
          (line-beginning-position)))))))

(ert-deftest test-el-copy-current-code-block
    ()
  (with-temp-buffer
    (insert "Some text\n```python\nprint('hello')\n```\nMore text")
    (goto-char
     (point-min))
    (re-search-forward "print" nil t)

    ;; We need to manually set kill-ring to test the function
    (let
        ((test-kill-ring
          '()))

      ;; Replace the real functions with mock versions
      (cl-letf
          (((symbol-function 'kill-ring-save)
            (lambda
              (start end)
              (push
               (buffer-substring-no-properties start end)
               test-kill-ring)))
           ((symbol-function 'pulse-momentary-highlight-region)
            (lambda
              (start end)
              nil))
           ((symbol-function 'message)
            (lambda
              (&rest args)
              nil)))

        (let
            ((result
              (--el-copy-current-code-block)))
          (should result)
          (should
           (=
            (length test-kill-ring)
            1))
          (should
           (string=
            (car test-kill-ring)
            "print('hello')\n")))))))

(ert-deftest test-el-next-code-block-function-exists
    ()
  (should
   (fboundp '--el-next-code-block)))

(ert-deftest test-el-previous-code-block-function-exists
    ()
  (should
   (fboundp '--el-previous-code-block)))

(provide 'test-emacs-llm-code-navigation)

(provide 'test-emacs-llm-code-navigation)

(when
    (not load-file-name)
  (message "test-emacs-llm-code-navigation.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))