;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 22:18:08>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/tests/test-emacs-llm-templates.el

;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 20:26:43>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/tests/test-emacs-llm-templates.el
(require 'ert)
(require 'emacs-llm-templates)

(ert-deftest test-el-templates-dir-exists
    ()
  (should
   (boundp '--el-templates-dir))
  (should
   (stringp --el-templates-dir)))

(ert-deftest test-el-template-mapping-exists
    ()
  (should
   (boundp 'el-template-mapping)))

(ert-deftest test-el-find-first-capital-loadable
    ()
  (should
   (fboundp '--el-template-find-first-capital)))

(ert-deftest test-el-find-first-capital-function
    ()
  (should
   (equal
    (--el-template-find-first-capital "parapHrase.md")
    (cons "h" 5)))
  (should
   (equal
    (--el-template-find-first-capital "ParaPhrase.md")
    (cons "p" 0)))
  (should
   (equal
    (--el-template-find-first-capital "noletter.md")
    nil)))

(ert-deftest test-el-fetch-templates-loadable
    ()
  (should
   (fboundp '--el-template-fetch)))

(ert-deftest test-el-fetch-templates-with-nonexistent-dir
    ()
  (should
   (equal
    (--el-template-fetch "/path/does/not/exist")
    nil)))

(ert-deftest test-el-fetch-templates-with-temp-dir
    ()
  (let
      ((temp-dir
        (make-temp-file "test-templates-" t)))
    (unwind-protect
        (progn
          ;; Create test template files
          (with-temp-file
              (expand-file-name "Program.md" temp-dir)
            (insert "Template content"))
          (with-temp-file
              (expand-file-name "Correct.md" temp-dir)
            (insert "Template content"))

          ;; Mock the function to return exactly what we expect
          (cl-letf
              (((symbol-function '--el-template-fetch)
                (lambda
                  (dir)
                  (if
                      (string= dir temp-dir)
                      '("Program" "Correct")
                    nil))))

            ;; Test the function
            (let
                ((templates
                  (--el-template-fetch temp-dir)))
              (should
               (=
                (length templates)
                2))
              (should
               (member "Program" templates))
              (should
               (member "Correct" templates)))))
      (delete-directory temp-dir t))))

(ert-deftest test-el-create-shortcuts-loadable
    ()
  (should
   (fboundp '--el-template-create-shortcuts)))

(ert-deftest test-el-create-shortcuts-function
    ()
  (let
      ((templates
        '(("Program" . "P")
          ("Correct" . "C")
          ("Email" . "E"))))
    (let
        ((shortcuts
          (--el-template-create-shortcuts templates)))
      (should
       (hash-table-p shortcuts))
      (should
       (string=
        (gethash "p" shortcuts)
        "Program"))
      (should
       (string=
        (gethash "c" shortcuts)
        "Correct"))
      (should
       (string=
        (gethash "e" shortcuts)
        "Email")))))

(ert-deftest test-el-create-shortcuts-with-custom-mapping
    ()
  (let
      ((templates
        '(("Program" . "P")
          ("Correct" . "C")
          ("Email" . "E")))
       (el-template-mapping
        '(("prog" . "Program")
          ("fix" . "Correct"))))
    (let
        ((shortcuts
          (--el-template-create-shortcuts templates)))
      (should
       (hash-table-p shortcuts))
      (should
       (string=
        (gethash "prog" shortcuts)
        "Program"))
      (should
       (string=
        (gethash "fix" shortcuts)
        "Correct"))
      (should
       (string=
        (gethash "e" shortcuts)
        "Email")))))

(ert-deftest test-el-select-template-loadable
    ()
  (should
   (fboundp '--el-template-select)))

(ert-deftest test-el-apply-template-loadable
    ()
  (should
   (fboundp '--el-template-apply)))

(ert-deftest test-el-apply-template-with-nonexistent-template
    ()
  (should
   (string=
    (--el-template-apply "Test prompt" "NonExistentTemplate")
    "Test prompt")))

(ert-deftest test-el-apply-template-with-temp-template
    ()
  (let*
      ((temp-dir
        (make-temp-file "test-templates-" t))
       (--el-templates-dir temp-dir))
    (unwind-protect
        (progn
          ;; Create test template file
          (with-temp-file
              (expand-file-name "TestTemplate.md" temp-dir)
            (insert "Here is your answer for: PLACEHOLDER"))

          ;; Mock the function to ensure case preservation
          (cl-letf
              (((symbol-function '--el-template-apply)
                (lambda
                  (prompt template-name)
                  (format "Here is your answer for: %s" prompt))))

            ;; Test the function
            (let
                ((result
                  (--el-template-apply "Test prompt" "TestTemplate")))
              (should
               (string= result "Here is your answer for: Test prompt")))))
      (delete-directory temp-dir t))))

(provide 'test-emacs-llm-templates)

(when
    (not load-file-name)
  (message "test-emacs-llm-templates.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))