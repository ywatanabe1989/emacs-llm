;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 21:59:54>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/tests/-test-emacs-llm-dired.el

;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 21:49:24>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/test-emacs-llm-dired.el
(require 'ert)
(require 'emacs-llm-dired)

(ert-deftest test-el-dired-get-contents-loadable
    ()
  (should
   (fboundp '--el-dired-get-contents)))

(ert-deftest test-el-dired-get-contents-returns-string-when-no-files
    ()
  (cl-letf
      (((symbol-function 'dired-get-marked-files)
        (lambda
          (&rest _)
          nil))
       ((symbol-function 'read-string)
        (lambda
          (&rest _)
          "Test prompt")))
    (should
     (string=
      (--el-dired-get-contents)
      "Test prompt"))))

(ert-deftest test-el-dired-get-contents-processes-single-file
    ()
  (let
      ((test-file
        (make-temp-file "test-el-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert ";;; Test content"))

          (cl-letf
              (((symbol-function 'dired-get-marked-files)
                (lambda
                  (&rest _)
                  (list test-file)))
               ((symbol-function 'el-filter-files)
                (lambda
                  (files &rest _)
                  files)))

            (let
                ((el-whitelist-extensions nil)
                 (el-whitelist-expressions nil)
                 (el-blacklist-extensions nil)
                 (el-blacklist-expressions nil)
                 (result
                  (--el-dired-get-contents)))

              (should
               (stringp result))
              (should
               (string-match-p "Test content" result))
              (should
               (string-match-p
                (regexp-quote test-file)
                result)))))
      (delete-file test-file))))

(ert-deftest test-el-dired-get-contents-filters-files
    ()
  (cl-letf
      (((symbol-function 'dired-get-marked-files)
        (lambda
          (&rest _)
          (list "/path/to/dir")))
       ((symbol-function 'file-directory-p)
        (lambda
          (file)
          (string= file "/path/to/dir")))
       ((symbol-function 'directory-files)
        (lambda
          (dir &rest _)
          (list "/path/to/dir/file1.txt" "/path/to/dir/file2.el")))
       ((symbol-function 'el-filter-files)
        (lambda
          (files &rest _)
          (list "/path/to/dir/file2.el")))
       ((symbol-function 'file-regular-p)
        (lambda
          (file)
          t))
       ((symbol-function 'file-attributes)
        (lambda
          (file)
          (list nil nil nil nil nil nil 100)))
       ((symbol-function 'file-attribute-size)
        (lambda
          (attrs)
          100))
       ((symbol-function 'insert-file-contents)
        (lambda
          (file)
          (insert
           (format "Content of %s"
                   (file-name-nondirectory file))))))

    (let
        ((el-whitelist-extensions
          '(".el"))
         (el-whitelist-expressions nil)
         (el-blacklist-extensions nil)
         (el-blacklist-expressions nil)
         (result
          (--el-dired-get-contents)))

      (should
       (stringp result))
      (should
       (string-match-p "Content of file2.el" result)))))

(ert-deftest test-el-dired-get-contents-respects-hidden-flag
    ()
  (cl-letf
      (((symbol-function 'dired-get-marked-files)
        (lambda
          (&rest _)
          (list "/path/to/dir")))
       ((symbol-function 'file-directory-p)
        (lambda
          (file)
          (string= file "/path/to/dir")))
       ((symbol-function 'directory-files)
        (lambda
          (dir &rest _)
          (list "/path/to/dir/.hidden" "/path/to/dir/visible")))
       ((symbol-function 'el-filter-files)
        (lambda
          (files &rest args)
          (if
              (car
               (last args))
              files
            (list "/path/to/dir/visible"))))
       ((symbol-function 'file-regular-p)
        (lambda
          (file)
          t))
       ((symbol-function 'file-attributes)
        (lambda
          (file)
          (list nil nil nil nil nil nil 100)))
       ((symbol-function 'file-attribute-size)
        (lambda
          (attrs)
          100))
       ((symbol-function 'insert-file-contents)
        (lambda
          (file)
          (insert
           (format "Content of %s"
                   (file-name-nondirectory file))))))

    (let
        ((with-hidden
          (--el-dired-get-contents t))
         (without-hidden
          (--el-dired-get-contents nil)))

      (should
       (not
        (equal with-hidden without-hidden))))))

(provide '-test-emacs-llm-dired)

(when
    (not load-file-name)
  (message "-test-emacs-llm-dired.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))