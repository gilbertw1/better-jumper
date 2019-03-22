;;; better-jumper.el --- description -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

(defgroup better-jumper nil
  "Better jumper configuration options."
  :prefix "better-jumper"
  :group 'jump)

(defcustom better-jumper-context 'window
  "Determines the context that better jumper operates within."
  :type '(choice (const :tag "Buffer" 'buffer)
                 (other :tag "Window" 'window))
  :group 'better-jumper)

(defcustom better-jumper-isolate-perspectives t
  "When non-nil, the jump commands respect buffer isolation provided by persp-mode perspectives."
  :type 'boolean
  :group 'better-jumper)

(defcustom better-jumper-new-window-behavior 'copy-last-buffer
  "Determines the behavior when a new window is created."
  :type '(choice (const :tag "Empty jump list" empty)
                 (const :tag "Copy last window" copy-last)
                 (other :tag "Copy last window w/ buffer" copy-last-buffer))
  :group 'better-jumper)

(defcustom better-jumper-new-perspective-behavior 'empty
  "Determines the behavior when a new window is created."
  :type '(choice (const :tag "Copy last window" copy-last)
                 (other :tag "Empty jump list" empty))
  :group 'better-jumper)

(defcustom better-jumper-max-length 100
  "The maximum number of jumps to keep track of."
  :type 'integer
  :group 'better-jumper)

(defcustom better-jumper-pre-jump-hook nil
  "Hooks to run just before jumping to a location in the jump list."
  :type 'hook
  :group 'better-jumper)

(defcustom better-jumper-post-jump-hook nil
  "Hooks to run just after jumping to a location in the jump list."
  :type 'hook
  :group 'better-jumper)

(defcustom better-jumper-ignored-file-patterns '("COMMIT_EDITMSG$" "TAGS$")
  "A list of regexps used to exclude files from the jump list."
  :type '(repeat string)
  :group 'better-jumper)

(defcustom better-jumper-use-evil-jump-advice t
  "When non-nil, advice is added to add jumps whenever `evil-set-jump' is invoked."
  :type 'boolean
  :group 'better-jumper)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar better-jumper--jumping nil
  "Flag inidicating jump in progress to prevent recording jumps in jump table.")

(defvar better-jumper--buffer-targets "\\*\\(new\\|scratch\\)\\*"
  "Regexp to match against `buffer-name' to determine whether it's a valid jump target.")

(defvar better-jumper--jump-table (make-hash-table)
  "Hashtable which stores all jumps on a per perspective/window basis.")

(cl-defstruct better-jumper-jump-list-struct
  ring
  (idx -1))

(defun better-jumper--persp-disable-window-config-update ()
  "Set persp parameter to disable window config update."
  (set-persp-parameter 'better-jumper-window-config-update-disabled t))

(defun better-jumper--persp-enable-window-config-update ()
  "Set persp parameter to disable window config update."
  (set-persp-parameter 'better-jumper-window-config-update-disabled nil))

(defun better-jumper--persp-window-config-update-disabled ()
  "Indicate if window config update should be disabled for this persp."
  (persp-parameter 'better-jumper-window-config-update-disabled ))

(defun better-jumper--get-jump-table-perspective (&optional persp)
  "Get jump table from PERSP or current perspective.
This should be used when `better-jumper-isolate-perspectives' is non-nil."
  (unless persp
    (setq persp (get-current-persp)))
  (let* ((jump-table (persp-parameter 'better-jumper-jump-table persp)))
    (unless jump-table
      (setq jump-table (make-hash-table))
      (set-persp-parameter 'better-jumper-jump-table jump-table persp))
    jump-table))

(defun better-jumper--get-jump-table-global ()
  "Get jump table stored in the variable `better-jumper--jump-table'."
  better-jumper--jump-table)

(defun better-jumper--get-jump-table ()
  "Get jump table.
The jump table is sourced either globally or from the current perspective
depending on configuration."
  (if better-jumper-isolate-perspectives
      (better-jumper--get-jump-table-perspective)
    (better-jumper--get-jump-table-global)))

(defun better-jumper--set-window-struct (window struct)
  (puthash window struct (better-jumper--get-jump-table)))

(defun better-jumper--get-current-struct (&optional window)
  "Get current jump struct for WINDOW.
Creates and adds jump struct to perspective if missing. Uses current frame
window if WINDOW parameter is missing."
  (unless window
    (setq window (frame-selected-window)))
  (let* ((jump-table (better-jumper--get-jump-table))
         (jump-struct (gethash window jump-table)))
    (unless jump-struct
      (setq jump-struct (make-better-jumper-jump-list-struct))
      (puthash window jump-struct jump-table))
    jump-struct))

(defun better-jumper--get-jump-list (struct)
  "Gets and potentially initialize jumps for STRUCT."
  (let ((ring (better-jumper-jump-list-struct-ring struct)))
    (unless ring
      (setq ring (make-ring better-jumper-max-length))
      (setf (better-jumper-jump-list-struct-ring struct) ring))
    ring))

(defun better-jumper--get-window-jump-list (&optional window)
  "Gets jump list for WINDOW.
Uses the current window if WINDOW is nil."
  (let ((struct (better-jumper--get-current-struct window)))
    (better-jumper--get-jump-list struct)))

(defun better-jumper--jumps-jump (idx shift &optional window)
  "Jump from position IDX using SHIFT on WINDOW or current window."
  (let ((jump-list (better-jumper--get-window-jump-list window)))
    (setq idx (+ idx shift))
    (let* ((current-file-name (or (buffer-file-name) (buffer-name)))
           (size (ring-length jump-list)))
      (when (and (< idx size) (>= idx 0))
        ;; actual jump
        (run-hooks 'better-jumper-pre-jump-hook)
        (let* ((place (ring-ref jump-list idx))
               (pos (car place))
               (file-name (cadr place)))
          (setq better-jumper--jumping t)
          (if (string-match-p better-jumper--buffer-targets file-name)
              (switch-to-buffer file-name)
            (find-file file-name))
          (setq better-jumper--jumping nil)
          (goto-char pos)
          (setf (better-jumper-jump-list-struct-idx (better-jumper--get-current-struct window)) idx)
          (run-hooks 'better-jumper-post-jump-hook))))))

(defun better-jumper--jumps-push ()
  "Pushes the current cursor/file position to the jump list."
  (let* ((jump-list (better-jumper--get-window-jump-list))
         (file-name (buffer-file-name))
         (buffer-name (buffer-name))
         (current-pos (point))
         (first-pos nil)
         (first-file-name nil)
         (excluded nil))
    (when (and (not file-name)
                 (string-match-p better-jumper--buffer-targets buffer-name))
        (setq file-name buffer-name))
      (when file-name
        (dolist (pattern better-jumper-ignored-file-patterns)
          (when (string-match-p pattern file-name)
            (setq excluded t)))
        (unless excluded
          (unless (ring-empty-p jump-list)
            (setq first-pos (car (ring-ref jump-list 0)))
            (setq first-file-name (car (cdr (ring-ref jump-list 0)))))
          (unless (and (equal first-pos current-pos)
                       (equal first-file-name file-name))
            (ring-insert jump-list `(,current-pos ,file-name)))))))

(defun better-jumper-set-jump (&optional pos)
  "Set jump point at POS.
POS defaults to point."
  (unless (region-active-p)
    (push-mark pos t))

  (unless better-jumper--jumping
    ;; clear out intermediary jumps when a new one is set
    (let* ((struct (better-jumper--get-current-struct))
           (jump-list (better-jumper--get-jump-list struct))
           (idx (better-jumper-jump-list-struct-idx struct)))
      (cl-loop repeat idx
               do (ring-remove jump-list))
      (setf (better-jumper-jump-list-struct-idx struct) -1))
    (save-excursion
      (when pos
        (goto-char pos))
      (better-jumper--jumps-push))))

(defun better-jumper-jump-backward ()
  "Jump backward to previous location in jump list."
  (interactive)
  (let* ((struct (better-jumper--get-current-struct))
         (idx (better-jumper-jump-list-struct-idx struct)))
    (when (= idx -1)
      (setq idx 0)
      (setf (better-jumper-jump-list-struct-idx struct) 0)
      (better-jumper--jumps-push))
    (better-jumper--jumps-jump idx 1)))

(defun better-jumper-jump-forward ()
  "Jump forward to previous location in jump list."
  (interactive)
  (let* ((struct (better-jumper--get-current-struct))
         (idx (better-jumper-jump-list-struct-idx struct)))
        (when (= idx -1)
          (setq idx 0)
          (setf (better-jumper-jump-list-struct-idx struct) 0)
          (better-jumper--jumps-push))
        (better-jumper--jumps-jump idx -1)))

(defun better-jumper--window-configuration-hook (&rest args)
  "Run on window configuration change (Ignore ARGS).
Cleans up deleted windows and copies history to newly created windows."
  (when (not (better-jumper--persp-window-config-update-disabled))
    (let* ((jump-table (better-jumper--get-jump-table))
           (window-list (window-list-1 nil nil t))
           (existing-window (selected-window))
           (new-window (previous-window)))
      (when (and (not (eq existing-window new-window))
                 (> (length window-list) 1))
        (let* ((target-jump-struct (better-jumper--get-current-struct new-window))
               (target-jump-list (better-jumper--get-jump-list target-jump-struct)))
          (when (ring-empty-p target-jump-list)
            (let* ((source-jump-struct (better-jumper--get-current-struct existing-window))
                   (source-list (better-jumper--get-jump-list source-jump-struct)))
              (setf (better-jumper-jump-list-struct-idx target-jump-struct) (better-jumper-jump-list-struct-idx source-jump-struct))
              (setf (better-jumper-jump-list-struct-ring target-jump-struct) (ring-copy source-list))))))
      ;; delete obsolete windows
      (maphash (lambda (key val)
                 (unless (member key window-list)
                   (remhash key jump-table)))
               jump-table))))

(if better-jumper-use-evil-jump-advice
    (defadvice evil-set-jump (before better-jumper activate)
      (better-jumper-set-jump)))

(defun better-jumper--save-perspective-jump-state ()
  "Save the current window location state of the current perspective."
  (let ((jump-state '())
        (window-list (window-list-1)))
    (dolist (window window-list)
      (let* ((jump-struct (better-jumper--get-current-struct window))
             (buf (buffer-name (window-buffer window)))
             (pos (window-point window)))
        (when buf
          (push `(,buf ,pos ,jump-struct) jump-state))))
    (set-persp-parameter 'better-jumper-persp-state jump-state)))

(defun better-jumper--restore-perspective-jump-state ()
  "Restore the jump list using saved window location state."
  (let* ((jump-state (persp-parameter 'better-jumper-persp-state))
         (window-list (window-list-1)))
    (when jump-state
      (dolist (window window-list)
        (let* ((target-jump-list (better-jumper--get-window-jump-list window))
               (buf (buffer-name (window-buffer window)))
               (pos (window-point window)))
          (when (and buf (ring-empty-p target-jump-list))
            (let* ((matched-state (nth 2 (seq-find (lambda (e)
                                                     (and (string= (nth 0 e) buf)
                                                          (= (nth 1 e) pos))) jump-state))))
              (unless matched-state
                (setq matched-state (nth 2 (seq-find (lambda (e)
                                                       (string= (nth 0 e) buf)) jump-state))))
              (when matched-state
                (better-jumper--set-window-struct window matched-state)))))))))

(defun better-jumper--before-persp-deactivate (&rest args)
  "Save jump state when a perspective is deactivated. Ignore ARGS."
  (better-jumper--persp-disable-window-config-update)
  (better-jumper--save-perspective-jump-state))

(defun better-jumper--on-persp-activate (&rest args)
  "Restore jump state when a perspective is activated. Ignore ARGS."
  (better-jumper--restore-perspective-jump-state)
  (better-jumper--persp-enable-window-config-update))

(add-hook! 'persp-before-deactivate-functions #'better-jumper--before-persp-deactivate)
(add-hook! 'persp-activated-functions #'better-jumper--on-persp-activate)
(add-hook! 'persp-before-save-state-to-file-functions #'better-jumper--before-persp-deactivate)

(provide 'better-jumper)
;;; better-jumper.el ends here
