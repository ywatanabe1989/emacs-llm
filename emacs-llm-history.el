;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-27 09:24:41>
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

;; History File Handling
;; ----------------------------------------

(defun --el-history-varidate-state
    ()
  "Ensure history directory exists."
  (--el-history-ensure-dir)
  (--el-history-ensure-file)
  (--el-history-backup-file-if))

(defun --el-history-ensure-dir
    ()
  "Ensure history directory exists."
  (unless
      (file-exists-p --el-history-dir)
    (make-directory --el-history-dir t)))

(defun --el-history-ensure-file
    ()
  "Ensure history file exists."
  (--el-history-ensure-file)
  (unless
      (file-exists-p --el-history-file)
    (with-temp-file history-file
      (insert ""))))

(defun --el-history-backup-file-if
    ()
  "Backup history file with timestamp if it exceeds max size."
  (when
      (and
       (file-exists-p --el-history-file)
       (>
        (file-attribute-size
         (file-attributes --el-history-file))
        --el-history-max-size))
    (--el-history-ensure-file)
    (let
        ((backup-file
          (expand-file-name
           (format "history-%s.json"
                   (format-time-string "%Y-%m-%d-%H-%M-%S"))
           --el-history-dir)))
      (copy-file --el-history-file backup-file)
      (message "Backed up history to %s" backup-file)
      ;; Create new empty history file
      (with-temp-file --el-history-file
        (insert "[]")))))

(defun el-history-clear
    ()
  "Clear the conversation history."
  (interactive)
  (setq --el-history
        '())
  (--el-history-save)
  (message "Emacs-LLM conversation history cleared."))

;; Loader/Saver
;; ----------------------------------------

(defun --el-history-load
    ()
  "Load conversation history from `--el-history-file`."
  (--el-history-backup-file-if)
  (when
      (file-exists-p --el-history-file)
    (with-temp-buffer
      (insert-file-contents --el-history-file)
      (setq --el-history
            (json-read-from-string
             (buffer-string))))))

(defun --el-history-save
    ()
  "Save conversation history to `--el-history-file`."
  (--el-history-ensure-file)
  (with-temp-file --el-history-file
    (insert
     (json-encode --el-history))))

(defun --el-history-append
    (role content &optional template)
  "Append a message with ROLE and CONTENT to the conversation history.
If TEMPLATE is provided, include it as part of the metadata."
  (let
      ((entry
        `((role . ,role)
          (content . ,content)
          ,@(when template
              `((template . ,template))))))
    (setq --el-history
          (append --el-history
                  (list entry)))
    (--el-history-save)))

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

(defun --el-history-get-recent
    (&optional n-histories)
  "Get the most recent conversation history limited by `--el-n-histories`."
  (--el-history-load)
  (let*
      ((history-length
        (length --el-history))
       (n-histories
        (or n-histories --el-n-histories))
       (start-index
        (max 0
             (- history-length n-histories))))
    (if
        (> history-length 0)
        (cl-subseq --el-history start-index)
      nil)))

(provide 'emacs-llm-history)

(when
    (not load-file-name)
  (message "emacs-llm-history.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))