;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 21:30:20>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-history.el

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

(defcustom --el-max-history-interactions 10
  "Maximum number of interactions to include in API calls."
  :type 'integer
  :group 'emacs-llm)

(defvar --el-history
  '()
  "List to keep track of conversation history.")

(defun --el-ensure-history-dir
    ()
  "Ensure history directory exists."
  (unless
      (file-exists-p --el-history-dir)
    (make-directory --el-history-dir t)))

(defun --el-backup-history-file
    ()
  "Backup history file with timestamp if it exceeds max size."
  (when
      (and
       (file-exists-p --el-history-file)
       (>
        (file-attribute-size
         (file-attributes --el-history-file))
        --el-history-max-size))
    (--el-ensure-history-dir)
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

(defun --el-load-history
    ()
  "Load conversation history from `--el-history-file`."
  (--el-backup-history-file)
  (when
      (file-exists-p --el-history-file)
    (with-temp-buffer
      (insert-file-contents --el-history-file)
      (setq --el-history
            (json-read-from-string
             (buffer-string))))))

(defun --el-save-history
    ()
  "Save conversation history to `--el-history-file`."
  (--el-ensure-history-dir)
  (with-temp-file --el-history-file
    (insert
     (json-encode --el-history))))

(defun --el-append-to-history
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
    (--el-save-history)))

(defun --el-clear-history
    ()
  "Clear the conversation history."
  (interactive)
  (setq --el-history
        '())
  (--el-save-history)
  (message "Emacs-LLM conversation history cleared."))

;;;###autoload
(defun el-show-history
    (&optional num-interactions)
  "Display the conversation history in a buffer.
Show NUM-INTERACTIONS most recent interactions (default: 20)."
  (interactive
   (list
    (read-number "Number of interactions to show: " 20)))
  (--el-load-history)
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

(defun --el-scroll-history
    ()
  "Scroll to current position in history buffer."
  (interactive)
  (when-let
      ((history-buffer
        (get-buffer "*Emacs-LLM History*")))
    (with-current-buffer history-buffer
      (let
          ((window
            (get-buffer-window history-buffer)))
        (when window
          (with-selected-window window
            (recenter)))))))

;; (defun --el-copy-last-response
;;     ()
;;   "Copy the last AI response from history to kill ring."
;;   (interactive)
;;   (--el-load-history)
;;   (if --el-history
;;       (let*
;;           ((last-entries
;;             (reverse --el-history))
;;            (last-response
;;             (cl-find-if
;;              (lambda
;;                (entry)
;;                (string=
;;                 (alist-get 'role entry)
;;                 "assistant"))
;;              last-entries)))
;;         (if last-response
;;             (let
;;                 ((content
;;                   (alist-get 'content last-response)))
;;               (kill-new content)
;;               (message "Last AI response copied to kill ring"))
;;           (message "No AI response found in history")))
;;     (message "No history found")))

(defun --el-get-recent-history
    ()
  "Get the most recent conversation history limited by `--el-max-history-interactions`."
  (--el-load-history)
  (let*
      ((history-length
        (length --el-history))
       (start-index
        (max 0
             (- history-length --el-max-history-interactions))))
    (if
        (> history-length 0)
        (cl-subseq --el-history start-index)
      nil)))

;; (defun --el-scroll-history-forward
;;     ()
;;   "Scroll forward in history buffer."
;;   (interactive)
;;   (when-let
;;       ((history-buffer
;;         (get-buffer "*Emacs-LLM History*")))
;;     (with-selected-window
;;         (get-buffer-window history-buffer)
;;       (scroll-up-command))))

;; (defun --el-scroll-history-backward
;;     ()
;;   "Scroll backward in history buffer."
;;   (interactive)
;;   (when-let
;;       ((history-buffer
;;         (get-buffer "*Emacs-LLM History*")))
;;     (with-selected-window
;;         (get-buffer-window history-buffer)
;;       (scroll-down-command))))

;; (defun --el-history-mode
;;     ()
;;   "Major mode for viewing LLM conversation history."
;;   (interactive)
;;   (kill-all-local-variables)
;;   (use-local-map
;;    (let
;;        ((map
;;          (make-sparse-keymap)))
;;      (define-key map
;;                  (kbd "n")
;;                  #'--el-scroll-history-forward)
;;      (define-key map
;;                  (kbd "p")
;;                  #'--el-scroll-history-backward)
;;      (define-key map
;;                  (kbd "SPC")
;;                  #'--el-scroll-history-forward)
;;      (define-key map
;;                  (kbd "S-SPC")
;;                  #'--el-scroll-history-backward)
;;      (define-key map
;;                  (kbd "q")
;;                  #'quit-window)
;;      map))
;;   (setq major-mode '--el-history-mode
;;         mode-name "LLM-History")
;;   (read-only-mode 1))

(provide 'emacs-llm-history)

(when
    (not load-file-name)
  (message "emacs-llm-history.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))