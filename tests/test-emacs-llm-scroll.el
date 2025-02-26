;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 22:20:13>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/tests/test-emacs-llm-scroll.el

;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 21:31:21>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/tests/test-emacs-llm-scroll.el
(require 'ert)
(require 'emacs-llm-scroll)

(ert-deftest test-el-scroll-to-bottom-loadable
    ()
  (should
   (fboundp '--el-scroll-to-bottom)))

(ert-deftest test-el-scroll-to-point-loadable
    ()
  (should
   (fboundp '--el-scroll-to-point)))

(ert-deftest test-el-scroll-to-last-separator-loadable
    ()
  (should
   (fboundp '--el-scroll-to-last-separator)))

(ert-deftest test-el-scroll-to-point-function
    ()
  (let*
      ((test-buffer-name "*test-scroll-point*")
       (--el-buffer-name test-buffer-name))
    (unwind-protect
        (progn
          (with-current-buffer
              (get-buffer-create test-buffer-name)
            (erase-buffer)
            (insert "Line 1\nLine 2\nLine 3")
            (goto-char
             (point-min))

            ;; Mock the window functions
            (cl-letf
                (((symbol-function 'get-buffer-window)
                  (lambda
                    (buffer-or-name &optional minibuf)
                    (selected-window)))
                 ((symbol-function 'recenter)
                  (lambda
                    (&optional arg)
                    nil)))

              ;; Test scrolling to a specific point (position of "Line 2")
              (--el-scroll-to-point
               (+
                (point-min)
                7))

              ;; Verify cursor moved to the expected position
              (should
               (=
                (point)
                (+
                 (point-min)
                 7))))))
      (kill-buffer test-buffer-name))))

(ert-deftest test-el-scroll-to-last-separator-function
    ()
  (let*
      ((test-buffer-name "*test-scroll-separator*")
       (--el-buffer-name test-buffer-name)
       (--el-separator "---SEPARATOR---"))
    (unwind-protect
        (progn
          (with-current-buffer
              (get-buffer-create test-buffer-name)
            (erase-buffer)
            (insert "Line 1\n---SEPARATOR---\nLine 2\n---SEPARATOR---\nLine 3")
            (goto-char
             (point-min))

            ;; Mock the window functions
            (cl-letf
                (((symbol-function 'get-buffer-window)
                  (lambda
                    (buffer-or-name &optional minibuf)
                    (selected-window)))
                 ((symbol-function 'recenter)
                  (lambda
                    (&optional arg)
                    nil))
                 ((symbol-function 'set-window-point)
                  (lambda
                    (window pos)
                    (goto-char pos))))

              ;; Test scrolling to the last separator
              (--el-scroll-to-last-separator)

              ;; Verify cursor moved to the last separator position
              (should
               (looking-at "---SEPARATOR---")))))
      (kill-buffer test-buffer-name))))

(provide 'test-emacs-llm-scroll)

(provide 'test-emacs-llm-scroll)

(when
    (not load-file-name)
  (message "test-emacs-llm-scroll.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))