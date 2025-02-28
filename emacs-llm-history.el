;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-03-01 04:05:20>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-history.el

;; Variables
;; ----------------------------------------

(defcustom --el-history-dir
  (expand-file-name "history"
                    (file-name-directory
                     (or load-file-name buffer-file-name)))
  "Directory to store history files."
  :type 'directory
  :group 'emacs-llm)

(defcustom --el-history-file
  (expand-file-name "emacs-llm-history.json" --el-history-dir)
  "File path to save conversation history."
  :type 'string
  :group 'emacs-llm)

(defcustom --el-history-max-size
  (* 10 1024 1024)
  ;; 10MB
  "Maximum size for history files in bytes."
  :type 'number
  :group 'emacs-llm)

(defcustom --el-n-histories 10
  "Maximum number of interactions to include in API calls."
  :type 'integer
  :group 'emacs-llm)

(defvar --el-history
  '()
  "List to keep track of conversation history.")

(define-derived-mode --el-history-mode markdown-mode "Emacs-LLM History"
  "Major mode for viewing Emacs-LLM conversation history.")

(defun el-history-clear
    ()
  "Clear the conversation history."
  (interactive)
  (setq --el-history
        '())
  (--el-history-save)
  (message "Emacs-LLM conversation history cleared."))

;; Saving
;; ----------------------------------------

(defun --el-history-save
    ()
  "Save conversation history to `--el-history-file`."
  (--el-history-ensure)
  (with-temp-file --el-history-file
    (insert
     (json-encode --el-history))))

(defun --el-history-append
    (role content &optional template)
  "Append a new message with ROLE and CONTENT to history JSON.
Optional TEMPLATE is the template name used for user messages."
  (let*
      ((history
        (or
         (--el-history-load-recent-as-json)
         '()))
       (entry
        (cond
         ((string= role "user")
          (if template
              `(("role" . "user")
                ("content" . ,content)
                ("template" . ,template))
            `(("role" . "user")
              ("content" . ,content))))
         (t
          `(("role" . ,role)
            ("content" . ,content)))))
       (updated-history
        (append history
                (list entry))))

    ;; Write back to file
    (when --el-history-file
      (with-temp-file --el-history-file
        (insert
         (json-encode updated-history))))))

;; Loading
;; ----------------------------------------

(defun --el-history-load
    ()
  "Load conversation history from `--el-history-file`."
  (--el-history-ensure)
  (with-temp-buffer
    (insert-file-contents --el-history-file)
    (setq --el-history
          (json-read-from-string
           (buffer-string)))))

;; (defun --el-history-load-recent
;;     ()
;;   "Load recent conversation history limited to `--el-n-histories` entries.
;; Returns the most recent entries from the conversation history."
;;   (let
;;       ((history
;;         (--el-history-load-recent-as-json)))
;;     (when history
;;       ;; Take only the most recent entries based on --el-n-histories
;;       (if
;;           (>
;;            (length history)
;;            --el-n-histories)
;;           (nthcdr
;;            (-
;;             (length history)
;;             --el-n-histories)
;;            history)
;;         history))))

(defun --el-history-load-recent
    ()
  "Load recent conversation history limited to `--el-n-histories` entries.
Returns the most recent entries from the conversation history."
  (let
      ((history
        (--el-history-load-recent-as-json)))
    (when history
      ;; Replace engine names with proper role
      (mapcar
       (lambda
         (msg)
         (let
             ((role
               (cdr
                (assoc "role" msg))))
           (when
               (not
                (or
                 (equal role "user")
                 (equal role "assistant")))
             ;; Replace any engine name with "assistant"
             (setf
              (cdr
               (assoc "role" msg))
              "assistant"))
           msg))
       history)
      ;; Take only the most recent entries based on --el-n-histories
      (if
          (>
           (length history)
           --el-n-histories)
          (nthcdr
           (-
            (length history)
            --el-n-histories)
           history)
        history))))

(defun --el-history-load-recent-as-json
    ()
  "Load recent conversation history from the history file as JSON.
Returns nil if the file doesn't exist or is empty."
  (when
      (and --el-history-file
           (file-exists-p --el-history-file)
           (>
            (file-attribute-size
             (file-attributes --el-history-file))
            0))
    (with-temp-buffer
      (insert-file-contents --el-history-file)
      (goto-char
       (point-min))
      (condition-case err
          (let
              ((json-array-type 'list)
               (json-object-type 'alist)
               (json-key-type 'string))
            (json-read))
        (error
         (message "Error reading JSON history: %s"
                  (error-message-string err))
         nil)))))

;; Displaying
;; ----------------------------------------

;;;###autoload
(defun el-history-show
    (&optional num-interactions)
  "Display the conversation history in a buffer.
Show NUM-INTERACTIONS most recent interactions (default: 20)."
  (interactive
   (list
    (read-number "Number of interactions to show: " 20)))
  (--el-history-load)
  (let*
      ((num
        (or num-interactions 20))
       (buffer
        (get-buffer-create "*Emacs-LLM History*"))
       (entries
        (if
            (>
             (length --el-history)
             num)
            (nthcdr
             (-
              (length --el-history)
              num)
             --el-history)
          --el-history)))
    (with-current-buffer buffer
      (let
          ((inhibit-read-only t))
        (erase-buffer)
        (dolist
            (entry entries)
          (let
              ((role
                (alist-get 'role entry))
               (content
                (alist-get 'content entry))
               (template
                (alist-get 'template entry)))
            (insert
             (propertize
              (concat role ":")
              'face 'bold))
            (when template
              (insert
               (format " [Template: %s]" template)))
            (insert "\n\n")
            (insert content)
            (insert "\n\n"
                    (make-string 80 ?-)
                    "\n\n")))
        (goto-char
         (point-min))
        (--el-history-mode)
        (display-buffer buffer)))))

;; Ensure History File
;; ----------------------------------------

(defun --el-history-ensure
    ()
  "Ensure history directory, file, and backup exist for conversation management."
  (--el-history-ensure-dir)
  (--el-history-ensure-file)
  (--el-history-ensure-rotation))

(defun --el-history-ensure-dir
    ()
  "Ensure history directory exists for storing conversation data."
  (unless
      (file-exists-p --el-history-dir)
    (make-directory --el-history-dir t)))

(defun --el-history-ensure-file
    ()
  "Ensure history file exists for writing conversation data."
  (--el-history-ensure-dir)
  (if
      (file-exists-p --el-history-file)
      ;; Check if file exists but is empty
      (when
          (= 0
             (file-attribute-size
              (file-attributes --el-history-file)))
        (with-temp-file --el-history-file
          (insert "[]")))
    ;; File doesn't exist, create it with empty array
    (with-temp-file --el-history-file
      (insert "[]"))))

(defun --el-history-ensure-rotation
    ()
  "Backup history file with timestamp if its size exceeds `--el-history-max-size'."
  (--el-history-ensure-dir)
  (--el-history-ensure-file)
  (when
      (and
       (file-exists-p --el-history-file)
       (>
        (file-attribute-size
         (file-attributes --el-history-file))
        --el-history-max-size))
    (let
        ((backup-file
          (expand-file-name
           (format "history-%s.json"
                   (format-time-string "%Y-%m-%d-%H-%M-%S"))
           --el-history-dir)))
      (copy-file --el-history-file backup-file)
      (message "Backed up history to %s" backup-file)
      ;; Create new empty history file
      (--el-history-ensure-file))))

(provide 'emacs-llm-history)

(when
    (not load-file-name)
  (message "emacs-llm-history.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))