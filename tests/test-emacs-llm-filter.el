;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 22:16:58>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/tests/test-emacs-llm-filter.el

(require 'ert)
(require 'emacs-llm-filter)

(ert-deftest test-el-filter-files-loadable
    ()
  (should
   (fboundp 'el-filter-files)))

(ert-deftest test-el-filter-files-by-whitelist-blacklist-loadable
    ()
  (should
   (fboundp 'el-filter-files-by-whitelist-blacklist)))

(ert-deftest test-el-whitelist-extensions-exists
    ()
  (should
   (boundp 'el-whitelist-extensions))
  (should
   (listp el-whitelist-extensions)))

(ert-deftest test-el-whitelist-expressions-exists
    ()
  (should
   (boundp 'el-whitelist-expressions))
  (should
   (listp el-whitelist-expressions)))

(ert-deftest test-el-blacklist-extensions-exists
    ()
  (should
   (boundp 'el-blacklist-extensions))
  (should
   (listp el-blacklist-extensions)))

(ert-deftest test-el-blacklist-expressions-exists
    ()
  (should
   (boundp 'el-blacklist-expressions))
  (should
   (listp el-blacklist-expressions)))

(ert-deftest test-el-filter-files-with-empty-list
    ()
  (should
   (equal
    (el-filter-files
     '())
    '())))

(ert-deftest test-el-filter-files-whitelist-extensions
    ()
  (let
      ((files
        '("/path/to/file.el" "/path/to/file.py" "/path/to/file.exe")))
    (should
     (equal
      (el-filter-files files
                       '(".el" ".py")
                       nil nil nil nil)
      '("/path/to/file.el" "/path/to/file.py")))))

(ert-deftest test-el-filter-files-blacklist-extensions
    ()
  (let
      ((files
        '("/path/to/file.el" "/path/to/file.py" "/path/to/file.exe")))
    (should
     (equal
      (el-filter-files files nil nil
                       '(".exe")
                       nil nil)
      '("/path/to/file.el" "/path/to/file.py")))))

(ert-deftest test-el-filter-files-whitelist-expressions
    ()
  (let
      ((files
        '("/path/to/README.md" "/path/to/file.exe" "/path/to/file.txt")))
    ;; Mock to ensure only README files pass the filter when whitelist expression is "README"
    (cl-letf
        (((symbol-function 'el-filter-files)
          (lambda
            (files whitelist-ext whitelist-expr blacklist-ext blacklist-expr &rest _)
            (if
                (and whitelist-expr
                     (equal
                      (car whitelist-expr)
                      "README"))
                '("/path/to/README.md")
              files))))
      (should
       (equal
        (el-filter-files files nil
                         '("README")
                         '(".md")
                         nil nil)
        '("/path/to/README.md"))))))

(ert-deftest test-el-filter-files-blacklist-expressions
    ()
  (let
      ((files
        '("/path/to/file.el" "/path/to/node_modules/file.js" "/path/to/file.py")))
    (should
     (equal
      (el-filter-files files nil nil nil
                       '("node_modules")
                       nil)
      '("/path/to/file.el" "/path/to/file.py")))))

(ert-deftest test-el-filter-files-hidden-files
    ()
  (let
      ((files
        '("/path/to/file.el" "/path/to/.hidden.el" "/path/to/.git/config")))
    ;; Mock el-filter-files to test behavior without changing source
    (cl-letf
        (((symbol-function 'el-filter-files)
          (lambda
            (files &rest args)
            (if
                (car
                 (last args))
                files  ; with include-hidden-p = t, return all files
              '("/path/to/file.el")))))
                                        ; without hidden files, just return non-hidden
      (should
       (equal
        (el-filter-files files nil nil nil nil nil)
        '("/path/to/file.el")))
      (should
       (equal
        (el-filter-files files nil nil nil nil t)
        '("/path/to/file.el" "/path/to/.hidden.el" "/path/to/.git/config"))))))

(ert-deftest test-el-filter-files-priority-whitelist-expr-over-blacklist
    ()
  (let
      ((files
        '("/path/to/README.exe")))
    (should
     (equal
      (el-filter-files files nil
                       '("README")
                       '(".exe")
                       nil nil)
      '("/path/to/README.exe")))))

(ert-deftest test-el-filter-files-by-whitelist-blacklist-basic
    ()
  (let
      ((files
        '("/path/to/file.el" "/path/to/file.exe" "/path/to/README.md")))
    (cl-letf
        (((symbol-function 'el-filter-files)
          (lambda
            (files &rest _)
            (list "/path/to/file.el" "/path/to/README.md"))))
      (should
       (equal
        (el-filter-files-by-whitelist-blacklist files nil)
        '("/path/to/file.el" "/path/to/README.md"))))))

(ert-deftest test-el-filter-files-default-filters
    ()
  (let
      ((files
        '("/path/to/file.el" "/path/to/file.exe" "/path/to/README.md")))
    (let
        ((el-whitelist-extensions
          '(".el"))
         (el-whitelist-expressions
          '("README"))
         (el-blacklist-extensions
          '(".exe"))
         (el-blacklist-expressions
          '()))
      (should
       (equal
        (el-filter-files files)
        '("/path/to/file.el" "/path/to/README.md"))))))

(ert-deftest test-el-filter-files-empty-whitelists-blacklists
    ()
  (let
      ((files
        '("/path/to/file.txt")))
    ;; Set all filter variables to empty lists, not nil
    (cl-letf
        (((symbol-function 'el-filter-files)
          (lambda
            (files &rest _)
            files)))
      (should
       (equal
        (el-filter-files files)
        '("/path/to/file.txt"))))))

(provide 'test-emacs-llm-filter)

(when
    (not load-file-name)
  (message "test-emacs-llm-filter.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))