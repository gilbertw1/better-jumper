;;; better-jumper.el --- description -*- lexical-binding: t; -*-

;;; Commentary:
;;;  A configurable jump list implementation for Emacs.
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
  "When non-nil, jump commands respect buffer isolation provided by persp-mode.
Better jumper will store jump lists as persp-parameters that will be saved and
loaded along with pserspectives."
  :type 'boolean
  :group 'better-jumper)

(defcustom better-jumper-new-window-behavior 'copy
  "Determines the behavior when a new window is created."
  :type '(choice (const :tag "Empty jump list" empty)
                 (other :tag "Copy last window" copy))
  :group 'better-jumper)

(defcustom better-jumper-max-length 100
  "The maximum number of jumps to keep track of."
  :type 'integer
  :group 'better-jumper)

(defcustom better-jumper-use-evil-jump-advice t
  "When non-nil, advice is added to add jumps whenever `evil-set-jump' is invoked."
  :type 'boolean
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar better-jumper--jumping nil
  "Flag inidicating jump in progress to prevent recording jumps in jump table.")

(defvar better-jumper--buffer-targets "\\*\\(new\\|scratch\\)\\*"
  "Regexp to match against `buffer-name' to determine whether it's a valid jump target.")

(defvar better-jumper--jump-table (make-hash-table)
  "Hashtable which stores all jumps on a per perspective/window/buffer basis.")

(cl-defstruct better-jumper-jump-list-struct
  ring
  (idx -1))

(defun better-jumper-use-perspectives ()
  "Return bool values indicating if perspectives should be used."
  (and better-jumper-isolate-perspectives
       (boundp 'persp-mode)))

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
  (if (better-jumper-use-perspectives)
      (better-jumper--get-jump-table-perspective)
    (better-jumper--get-jump-table-global)))

(defun better-jumper--set-struct (context struct)
  "Set jump struct for CONTEXT to STRUCT in jump table."
  (puthash context struct (better-jumper--get-jump-table)))

(defun better-jumper--get-current-context ()
  "Get current context item. Either current window or buffer."
  (if (eq better-jumper-context 'buffer)
      (current-buffer)
    (frame-selected-window)))

(defun better-jumper--get-struct (&optional context)
  "Get current jump struct for CONTEXT.
Creates and adds jump struct if one does not exist. Uses current window or
buffer if CONTEXT parameter is missing."
  (unless context
    (setq context (better-jumper--get-current-context)))
  (let* ((jump-table (better-jumper--get-jump-table))
         (jump-struct (gethash context jump-table)))
    (unless jump-struct
      (setq jump-struct (make-better-jumper-jump-list-struct))
      (puthash context jump-struct jump-table))
    jump-struct))

(defun better-jumper--get-struct-jump-list (struct)
  "Gets and potentially initialize jumps for STRUCT."
  (let ((ring (better-jumper-jump-list-struct-ring struct)))
    (unless ring
      (setq ring (make-ring better-jumper-max-length))
      (setf (better-jumper-jump-list-struct-ring struct) ring))
    ring))

(defun better-jumper--get-jump-list (&optional context)
  "Gets jump list for CONTEXT.
Uses the current context if CONTEXT is nil."
  (let ((struct (better-jumper--get-struct context)))
    (better-jumper--get-struct-jump-list struct)))

(defun better-jumper--jump (idx shift &optional context)
  "Jump from position IDX using SHIFT on CONTEXT.
Uses current context if CONTEXT is nil."
  (let ((jump-list (better-jumper--get-jump-list context)))
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
          (setf (better-jumper-jump-list-struct-idx (better-jumper--get-struct context)) idx)
          (run-hooks 'better-jumper-post-jump-hook))))))

(defun better-jumper--push (&optional context)
  "Pushes the current cursor/file position to the jump list for CONTEXT.
Uses current context if CONTEXT is nil."
  (let* ((jump-list (better-jumper--get-jump-list context))
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

(defun better-jumper--set-persp-window-config-update-disabled (disabled)
  "If DISABLED is non-nil then disable window config update."
  (set-persp-parameter 'better-jumper-window-config-update-disabled disabled))

(defun better-jumper--persp-window-config-update-disabled ()
  "Indicate if window config update should be disabled for this persp."
  (persp-parameter 'better-jumper-window-config-update-disabled))

(defun better-jumper--create-window-jump-state ()
  "Create dump of the window specific jump state."
  (let ((jump-state '())
        (window-list (window-list-1)))
    (dolist (window window-list)
      (let* ((jump-struct (better-jumper--get-struct window))
             (buf (buffer-name (window-buffer window)))
             (pos (window-point window)))
        (when buf
          (push `(,buf ,pos ,jump-struct) jump-state))))
    jump-state))

(defun better-jumper--create-buffer-jump-state ()
  "Create dump of the buffer specific jump state."
  (let* ((jump-state '())
         (buffer-list (buffer-list)))
    (dolist (buffer buffer-list)
      (let* ((jump-struct (better-jumper--get-struct buffer))
             (buf (buffer-name buffer)))
        (push `(,buf 0 ,jump-struct) jump-state)))
    jump-state))

(defun better-jumper--save-perspective-jump-state ()
  "Save the jump state state of the current perspective."
  (let ((jump-state (if (eq better-jumper-context 'buffer)
                        (better-jumper--create-buffer-jump-state)
                      (better-jumper--create-window-jump-state))))
    (set-persp-parameter 'better-jumper-persp-state jump-state)))

(defun better-jumper--restore-perspective-window-jump-state ()
  "Restore the jump list using saved window location jump state."
  (let* ((jump-state (persp-parameter 'better-jumper-persp-state))
         (window-list (window-list-1)))
    (when jump-state
      (dolist (window window-list)
        (let* ((target-jump-list (better-jumper--get-jump-list window))
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
                (better-jumper--set-struct window matched-state)))))))))

(defun better-jumper--restore-perspective-buffer-jump-state ()
  "Restore the jump list using saved buffer jump state."
  (let* ((jump-state (persp-parameter 'better-jumper-persp-state))
         (buffer-list (buffer-list)))
    (when jump-state
      (dolist (buffer buffer-list)
        (let* ((target-jump-list (better-jumper--get-jump-list buffer))
               (buf (buffer-name buffer)))
          (when (and buf (ring-empty-p target-jump-list))
            (let* ((matched-state (nth 2 (seq-find (lambda (e)
                                                       (string= (nth 0 e) buf)) jump-state))))
              (when matched-state
                (better-jumper--set-struct buffer matched-state)))))))))

(defun better-jumper--restore-perspective-jump-state ()
  "Restore the jump list using saved jump state."
  (if (eq better-jumper-context 'buffer)
      (better-jumper--restore-perspective-buffer-jump-state)
    (better-jumper--restore-perspective-window-jump-state)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   PUBLIC FUNCTIONS    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun better-jumper-set-jump (&optional pos)
  "Set jump point at POS.
POS defaults to point."
  (unless (or (region-active-p)
              (and (boundp 'evil-visual-state-p)
                   (evil-visual-state-p)))
    (push-mark pos t))

  (unless better-jumper--jumping
    ;; clear out intermediary jumps when a new one is set
    (let* ((struct (better-jumper--get-struct))
           (jump-list (better-jumper--get-struct-jump-list struct))
           (idx (better-jumper-jump-list-struct-idx struct)))
      (cl-loop repeat idx
               do (ring-remove jump-list))
      (setf (better-jumper-jump-list-struct-idx struct) -1))
    (save-excursion
      (when pos
        (goto-char pos))
      (better-jumper--push))))

;;;###autoload
(defun better-jumper-jump-backward (&optional count)
  "Jump backward COUNT positions to previous location in jump list.
If COUNT is nil then defaults to 1."
  (interactive)
  (let* ((count (or count 1))
         (struct (better-jumper--get-struct))
         (idx (better-jumper-jump-list-struct-idx struct)))
    (when (= idx -1)
      (setq idx 0)
      (setf (better-jumper-jump-list-struct-idx struct) 0)
      (better-jumper--push))
    (better-jumper--jump idx count)))

;;;###autoload
(defun better-jumper-jump-forward (&optional count)
  "Jump forward COUNT positions to location in jump list.
If COUNT is nil then defaults to 1."
  (interactive)
  (let* ((count (or count 1))
         (struct (better-jumper--get-struct))
         (idx (better-jumper-jump-list-struct-idx struct)))
        (when (= idx -1)
          (setq idx 0)
          (setf (better-jumper-jump-list-struct-idx struct) 0)
          (better-jumper--push))
        (better-jumper--jump idx (- 0 count))))

;;;;;;;;;;;;;;;;;;
;;;   HOOKS    ;;;
;;;;;;;;;;;;;;;;;;

(defun better-jumper--before-persp-deactivate (&rest args)
  "Save jump state when a perspective is deactivated. Ignore ARGS."
  (when (better-jumper-use-perspectives)
    (better-jumper--set-persp-window-config-update-disabled t)
    (better-jumper--save-perspective-jump-state)))

(defun better-jumper--on-persp-activate (&rest args)
  "Restore jump state when a perspective is activated. Ignore ARGS."
  (when (better-jumper-use-perspectives)
    (better-jumper--restore-perspective-jump-state)
    (better-jumper--set-persp-window-config-update-disabled nil)))

(with-eval-after-load 'persp-mode
  (add-hook 'persp-before-deactivate-functions #'better-jumper--before-persp-deactivate)
  (add-hook 'persp-activated-functions #'better-jumper--on-persp-activate)
  (add-hook 'persp-before-save-state-to-file-functions #'better-jumper--before-persp-deactivate))

(defun better-jumper--window-configuration-hook (&rest args)
  "Run on window configuration change (Ignore ARGS).
Cleans up deleted windows and copies history to newly created windows."
  (when (eq better-jumper-context 'window)
    (let* ((window-list (window-list-1 nil nil t))
           (jump-table (better-jumper--get-jump-table)))
      (when (and (eq better-jumper-new-window-behavior 'copy)
                 (and (better-jumper-use-perspectives)
                      (not (better-jumper--persp-window-config-update-disabled))))
        (let* ((curr-window (selected-window))
               (source-jump-struct (better-jumper--get-struct curr-window))
               (source-jump-list (better-jumper--get-struct-jump-list source-jump-struct)))
          (when (not (ring-empty-p source-jump-list))
            (dolist (window window-list)
              (let* ((target-jump-struct (better-jumper--get-struct window))
                     (target-jump-list (better-jumper--get-struct-jump-list target-jump-struct)))
                (when (ring-empty-p target-jump-list)
                  (setf (better-jumper-jump-list-struct-idx target-jump-struct) (better-jumper-jump-list-struct-idx source-jump-struct))
                  (setf (better-jumper-jump-list-struct-ring target-jump-struct) (ring-copy source-jump-list))))))))

      (maphash (lambda (key val)
                 (unless (member key window-list)
                   (remhash key jump-table)))
               jump-table))))

(add-hook 'window-configuration-change-hook #'better-jumper--window-configuration-hook)

(defun better-jumper--kill-buffer-hook ()
  "Run when a buffer is killed and remove buffer from jump list.
Only runs if context is set to 'buffer."
  (when (eq better-jumper-context 'buffer)
    (let* ((jump-table (better-jumper--get-jump-table))
           (buffer (current-buffer)))
      (remhash buffer jump-table))))

(add-hook 'kill-buffer-hook #'better-jumper--kill-buffer-hook)

(if better-jumper-use-evil-jump-advice
    (with-eval-after-load 'evil
      (defadvice evil-set-jump (before better-jumper activate)
        (better-jumper-set-jump))))

(provide 'better-jumper)
;;; better-jumper.el ends here
