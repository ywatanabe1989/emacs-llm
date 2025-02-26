;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 23:31:14>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-templates.el

(defcustom --el-templates-dir
  (expand-file-name "templates"
                    (file-name-directory
                     (or load-file-name buffer-file-name)))
  "Directory to store prompt templates."
  :type 'directory
  :group 'emacs-llm)

(defcustom el-template-mapping nil
  "Mapping between shortcuts and their corresponding templates.
Example: Template shortcuts can be customized as:
\\=((\"p\" . \"Program\")              ; p -> Program.md
 (\"e\" . \"Email\")                ; e -> Email.md
 (\"c\" . \"Correct\")              ; c -> Correct.md
 (\"my\" . \"MyAwesomeTemplate\"))  ; my -> MyAwesomeTemplate.md"
  :type
  '(alist :key-type string :value-type string)
  :group 'emacs-llm)

;; Functions for template handling
(defun --el-find-first-capital
    (string)
  "Find first capital letter in STRING and return cons of (letter . position).
Example: (--el-find-first-capital \"parapHrase.md\") => (h . 5)"
  (let*
      ((name
        (file-name-sans-extension string))
       (case-fold-search nil)
       (capital-pos
        (string-match "[A-Z]" name)))
    (when capital-pos
      (cons
       (downcase
        (substring name capital-pos
                   (1+ capital-pos)))
       capital-pos))))

(defun --el-fetch-templates
    (dir)
  "Return list of formatted template names from DIR that contain capital letters."
  (when
      (file-exists-p dir)
    (sort
     (delq nil
           (mapcar
            (lambda
              (f)
              (let*
                  ((name-without-ext
                    (substring f 0
                               (string-match "\\.md" f)))
                   (capital-info
                    (--el-find-first-capital f))
                   (first-capital
                    (car capital-info))
                   (capital-pos
                    (cdr capital-info)))
                (cond
                 ((= capital-pos 0)
                  (format "%s" name-without-ext))
                 (capital-pos
                  (format "%s %s" first-capital name-without-ext))
                 (t nil))))
            (directory-files dir nil ".*[A-Z].*\\.md$")))
     #'string<)))

(defun --el-create-shortcuts
    (templates)
  "Generate shortcuts for templates using numbers for duplicates."
  (let
      ((shortcuts
        (make-hash-table :test 'equal))
       (counts
        (make-hash-table :test 'equal))
       (completion-alist nil))

    ;; Apply predefined mappings first
    (when
        (boundp 'el-template-mapping)
      (dolist
          (mapping el-template-mapping)
        (let*
            ((key
              (car mapping))
             (template-name
              (cdr mapping)))
          (when-let
              ((template
                (cl-find template-name templates
                         :key #'car :test #'string=)))
            (puthash key
                     (car template)
                     shortcuts)
            ;; Add combined format for completion
            (push
             (cons
              (format "%s - %s" key
                      (car template))
              (car template))
             completion-alist)
            (puthash
             (substring key 0 1)
             1 counts)))))

    ;; Handle remaining templates
    (setq templates
          (sort templates
                (lambda
                  (a b)
                  (string<
                   (car a)
                   (car b)))))
    (dolist
        (template templates)
      (let*
          ((name
            (car template))
           (suffix
            (cdr template))
           (last-capital
            (if suffix
                (downcase suffix)
              (if
                  (string-match "[A-Z]" name)
                  (downcase
                   (substring
                    (match-string 0 name)
                    0 1))
                (downcase
                 (substring name 0 1)))))
           (count
            (gethash last-capital counts 0))
           (new-key
            (if
                (= count 0)
                last-capital
              (format "%s%d" last-capital count))))
        (unless
            (gethash new-key shortcuts)
          (puthash new-key name shortcuts)
          ;; Add combined format for completion
          (push
           (cons
            (format "%s - %s" new-key name)
            name)
           completion-alist)
          (puthash last-capital
                   (1+ count)
                   counts))))

    (defvar --el-template-completion-alist nil)
    (setq-default --el-template-completion-alist completion-alist)

    (defvar --el-template-completion-function nil)
    (setq --el-template-completion-function
          (lambda
            (string pred action)
            (if
                (eq action 'metadata)
                '(metadata
                  (category . emacs-llm-template))
              (complete-with-action action --el-template-completion-alist
                                    string pred))))
    shortcuts))

(defun --el-select-template
    ()
  "Prompt the user to select a template type for the LLM engine."
  (unless
      (minibufferp)
    (let*
        ((capital-templates
          (--el-fetch-templates --el-templates-dir))
         (shortcuts
          (make-hash-table :test 'equal))
         (key-count
          (make-hash-table :test 'equal))
         (prompt-parts nil))

      ;; Handle mapped templates first
      (when
          (boundp 'el-template-mapping)
        (dolist
            (mapping el-template-mapping)
          (let*
              ((key
                (car mapping))
               (value
                (cdr mapping))
               (base-key
                (substring key 0 1))
               (count
                (gethash base-key key-count 0)))
            (when
                (member value capital-templates)
              (puthash base-key
                       (1+ count)
                       key-count)
              (puthash key value shortcuts)
              (push
               (format "(%s) %s" key value)
               prompt-parts)))))

      ;; Handle unmapped templates with auto-numbering
      (dolist
          (template capital-templates)
        (unless
            (rassoc template el-template-mapping)
          (let*
              ((base-key
                (downcase
                 (substring template 0 1)))
               (count
                (gethash base-key key-count 0))
               (key
                (if
                    (> count 0)
                    (format "%s%d" base-key
                            (1+ count))
                  base-key)))
            (puthash base-key
                     (1+ count)
                     key-count)
            (puthash key template shortcuts)
            (push
             (format "(%s) %s" key template)
             prompt-parts))))

      (setq prompt-parts
            (sort prompt-parts 'string<))
      (let*
          ((prompt
            (concat "Template or Manual Instruction:\n"
                    (mapconcat 'identity prompt-parts " ")
                    "\n"))
           (input
            (read-string prompt))
           (template-type
            (or
             (gethash input shortcuts)
             (if
                 (string-blank-p input)
                 "None" input))))
        (unless
            (string= input "r")
          (display-buffer
           (get-buffer-create --el-buffer-name)))
        template-type))))

(defun --el-apply-template
    (prompt template-name)
  "Apply template TEMPLATE-NAME to PROMPT.
Returns a new prompt with the template applied."
  (if template-name
      (let
          ((template-path
            (expand-file-name
             (concat template-name ".md")
             --el-templates-dir)))
        (if
            (file-exists-p template-path)
            (with-temp-buffer
              (insert-file-contents template-path)
              (let
                  ((template-content
                    (buffer-string)))
                (replace-regexp-in-string "PLACEHOLDER" prompt template-content)))
          ;; If template file doesn't exist, just return original prompt
          prompt))
    ;; When template-name is nil, just return the original prompt
    prompt))

(provide 'emacs-llm-templates)

(when
    (not load-file-name)
  (message "emacs-llm-templates.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))