;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-03-01 07:00:54>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-templates.el

(require 'cl-lib)

;; Variables
;; ----------------------------------------

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

;; Main
;; ----------------------------------------

(defun --el-template-embed
    (prompt &optional template-name-or-manual-instruction)
  "Apply template TEMPLATE-NAME-OR-MANUAL-INSTRUCTION to PROMPT.
Returns a new prompt with the template applied."
  (let*
      ((template-name
        (--el-template-select template-name-or-manual-instruction))
       (template-contents
        (--el-template-get template-name)))
    (if template-contents
        (replace-regexp-in-string "PLACEHOLDER" prompt template-contents)
      prompt)))

;; Core
;; ----------------------------------------

(defun --el-template-get
    (template-name-or-manual-instruction)
  "Get template contents for TEMPLATE-NAME-OR-MANUAL-INSTRUCTION.
If it's a valid template name, return the contents of the template file.
If it's a manual instruction, format it with PLACEHOLDER."
  (if
      (--el-manual-instruction-p template-name-or-manual-instruction)
      ;; It's a manual instruction, so format it with PLACEHOLDER
      (format "%s\n\nPLACEHOLDER" template-name-or-manual-instruction)
    ;; It's a template name, so load the template file
    (let
        ((template-path
          (expand-file-name
           (concat template-name-or-manual-instruction ".md")
           --el-templates-dir)))
      (condition-case err
          (with-temp-buffer
            (insert-file-contents template-path)
            (buffer-string))
        (error
         ;; If template file doesn't exist, treat it as manual instruction
         (format "%s\n\nPLACEHOLDER" template-name-or-manual-instruction))))))

(defun --el-template-select
    (&optional template-name-or-manual-instruction)
  "Prompt the user to select a template type for the LLM engine.
If TEMPLATE-NAME-OR-MANUAL-INSTRUCTION is provided, verify and use it if valid."
  (cl-block --el-template-select
    (unless
        (minibufferp)
      (let*
          ((capital-templates
            (--el-template-fetch --el-templates-dir)))
        ;; If template was provided, return it immediately
        (when template-name-or-manual-instruction
          (cl-return-from --el-template-select template-name-or-manual-instruction))
        ;; Otherwise, prompt user to select
        (let*
            ((shortcuts-and-parts
              (--el-template-build-shortcuts capital-templates))
             (shortcuts
              (car shortcuts-and-parts))
             (prompt-parts
              (cdr shortcuts-and-parts))
             (prompt
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
                   "default" input))))
          template-type)))))

;; Helper
;; ----------------------------------------

(defun --el-template-find-first-capital
    (string)
  "Find first capital letter in STRING and return cons of (letter . position).
Example: (--el-template-find-first-capital \"parapHrase.md\") => (h . 5)"
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

(defun --el-template-fetch
    (&optional dir)
  "Return list of formatted template names from DIR that contain capital letters."
  (let
      ((dir
        (or dir --el-templates-dir)))
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
                      (--el-template-find-first-capital f))
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
       #'string<))))

(defun --el-template-create-shortcuts
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

(defun --el-manual-instruction-p
    (template-name-or-manual-instruction)
  "Verify if TEMPLATE-NAME-OR-MANUAL-INSTRUCTION is valid.
Return template-name-or-manual-instruction if valid, otherwise signal an error."
  (when template-name-or-manual-instruction
    (let
        ((template-file
          (expand-file-name
           (concat template-name-or-manual-instruction ".md")
           --el-templates-dir)))
      (if
          (file-exists-p template-file)
          nil
        t))))

(defun --el-template-build-shortcuts
    (capital-templates)
  "Build shortcuts hash tables for CAPITAL-TEMPLATES.
Returns a cons cell (shortcuts . prompt-parts)."
  (let
      ((shortcuts
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
    (cons shortcuts
          (sort prompt-parts 'string<))))

(provide 'emacs-llm-templates)

(when
    (not load-file-name)
  (message "emacs-llm-templates.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))