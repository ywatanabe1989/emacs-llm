;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 22:13:50>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-filter.el

(defcustom el-whitelist-extensions
  '(".el" ".py" ".sh" ".src" ".txt" ".md" ".org" ".yml" ".yaml" ".json")
  "List of file extensions that are allowed for processing."
  :type
  '(repeat string)
  :group 'emacs-llm)

(defcustom el-whitelist-expressions
  '("\\(^\\|/\\)README")
  "List of regex patterns for files to explicitly include, even if they would be excluded otherwise."
  :type
  '(repeat string)
  :group 'emacs-llm)

(defcustom el-blacklist-extensions
  '(".exe" ".dll" ".so" ".o" ".a" ".bin" ".zip" ".tar" ".gz" ".pdf" ".doc" ".docx"
    ".sif" ".img" ".image" ".png" ".jpg" ".jpeg" ".gif" ".tif" ".tiff" ".bmp" ".svg"
    ".ico" ".mp3" ".mp4" ".wav" ".ogg" ".flac" ".avi" ".mov" ".mkv" ".webm" ".wmv"
    ".iso" ".dmg" ".7z" ".rar" ".jar" ".class" ".pyc" ".pyo" ".obj" ".lib" ".pdb"
    ".cache" ".db" ".sqlite" ".dat" ".log" ".tmp" ".temp" ".swp" ".swo" ".bak"
    ".old" ".orig" ".DS_Store" ".pkl" ".npy" ".pth" ".csv")
  "List of file extensions that are always excluded from processing."
  :type
  '(repeat string)
  :group 'emacs-llm)

(defcustom el-blacklist-expressions
  '("/node_modules/" "/\\.git/" "/\\.svn/" "/\\.hg/" "/__pycache__/")
  "List of regex patterns for files to explicitly exclude from processing."
  :type
  '(repeat string)
  :group 'emacs-llm)

(defun el-filter-files
    (file-list &optional whitelist-ext whitelist-expr blacklist-ext blacklist-expr include-hidden-p)
  "Filter FILE-LIST based on whitelist and blacklist configurations.
Filtering priority order:
1. Whitelist expressions (highest priority - will always be included)
2. Blacklist extensions and expressions (will exclude unless whitelisted)
3. Whitelist extensions
4. Hidden files (excluded unless INCLUDE-HIDDEN-P is non-nil)

Optional arguments:
WHITELIST-EXT: List of allowed file extensions (defaults to el-whitelist-extensions)
WHITELIST-EXPR: List of regex patterns to always include (defaults to el-whitelist-expressions)
BLACKLIST-EXT: List of blocked file extensions (defaults to el-blacklist-extensions)
BLACKLIST-EXPR: List of regex patterns to exclude (defaults to el-blacklist-expressions)
INCLUDE-HIDDEN-P: When non-nil, include hidden files and directories."
  (let*
      ((result-files
        '())
       (root-path
        (or default-directory "/"))
       (w-ext
        (or whitelist-ext el-whitelist-extensions))
       (w-expr
        (or whitelist-expr el-whitelist-expressions))
       (b-ext
        (or blacklist-ext el-blacklist-extensions))
       (b-expr
        (or blacklist-expr el-blacklist-expressions)))

    (dolist
        (file file-list)
      (let*
          ((file-ext
            (file-name-extension file t))
           (rel-path
            (file-relative-name file root-path))
           ;; Is the file in any whitelist?
           (in-whitelist-ext
            (member file-ext w-ext))
           (in-whitelist-expr
            (cl-some
             (lambda
               (pattern)
               (string-match-p pattern rel-path))
             w-expr))
           ;; Is the file in any blacklist?
           (in-blacklist-ext
            (member file-ext b-ext))
           (in-blacklist-expr
            (cl-some
             (lambda
               (pattern)
               (string-match-p pattern rel-path))
             b-expr))
           ;; Is the file hidden?
           (is-hidden-file
            (and
             (not include-hidden-p)
             (let
                 ((components
                   (split-string rel-path "/" t))
                  (has-hidden-component nil))
               (dolist
                   (component components has-hidden-component)
                 (when
                     (string-match-p "^\\." component)
                   (setq has-hidden-component t))))))

           ;; Decision logic based on priority
           (include-file
            (cond
             ;; 1. Whitelist expressions (highest priority)
             (in-whitelist-expr t)

             ;; 2. Blacklist check
             ((or in-blacklist-ext in-blacklist-expr)
              nil)

             ;; 3. Whitelist extensions
             (in-whitelist-ext t)

             ;; 4. Hidden file check
             (is-hidden-file nil)

             ;; 5. Default: include if no blacklist/hidden issues
             (t t))))

        (when include-file
          (push file result-files))))

    (nreverse result-files)))

(defun --el-filter-files
    (file-list root-path &optional exclude-patterns include-patterns include-hidden-p)
  "Filter FILE-LIST using exclusion and inclusion patterns relative to ROOT-PATH.
EXCLUDE-PATTERNS: List of regex patterns to exclude files.
INCLUDE-PATTERNS: List of regex patterns to include files (overrides exclusions).
INCLUDE-HIDDEN-P: When non-nil, include hidden files and directories.
Files are first matched against EXCLUDE-PATTERNS, then against INCLUDE-PATTERNS.
A file matches an exclusion if its path relative to ROOT-PATH matches any pattern.
Hidden files are those with path components starting with '.' after ROOT-PATH."
  (let
      ((filtered-list file-list))
    ;; Step 1: Apply exclusion patterns
    (when exclude-patterns
      (setq filtered-list
            (seq-remove
             (lambda
               (file)
               (let*
                   ((rel-path
                     (file-relative-name file root-path)))
                 (cl-some
                  (lambda
                    (pattern)
                    (string-match-p pattern rel-path))
                  exclude-patterns)))
             filtered-list)))
    ;; Step 2: Apply inclusion patterns (these override exclusions)
    (when include-patterns
      (setq filtered-list
            (seq-filter
             (lambda
               (file)
               (let*
                   ((rel-path
                     (file-relative-name file root-path)))
                 (or
                  ;; Already in filtered list AND matches an include pattern
                  (cl-some
                   (lambda
                     (pattern)
                     (string-match-p pattern rel-path))
                   include-patterns)
                  ;; Or was in the original filtered list
                  (member file filtered-list))))
             file-list)))
    ;; Step 3: Filter hidden files unless include-hidden-p is set
    (unless include-hidden-p
      (setq filtered-list
            (seq-filter
             (lambda
               (file)
               (let*
                   ((rel-path
                     (file-relative-name file root-path))
                    (components
                     (split-string rel-path "/" t))
                    (has-hidden-component nil))
                 ;; Skip checking if rel-path is just "." (current directory)
                 (if
                     (string= rel-path ".")
                     t  ;; Always include current directory
                   ;; Otherwise check if any component starts with "."
                   (progn
                     (dolist
                         (component components)
                       (when
                           (string-match-p "^\\." component)
                         (setq has-hidden-component t)))
                     (not has-hidden-component)))))
             filtered-list)))
    filtered-list))

(defun el-filter-files-by-whitelist-blacklist
    (file-list &optional include-hidden-p)
  "Filter FILE-LIST based on whitelist and blacklist configurations.
Files matching whitelist extensions or expressions are included.
Files matching blacklist extensions or expressions are excluded, unless they match the whitelist.
When INCLUDE-HIDDEN-P is non-nil, hidden files are included."
  (el-filter-files file-list nil nil nil nil include-hidden-p))

(provide 'emacs-llm-filter)

(when
    (not load-file-name)
  (message "emacs-llm-filter.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))