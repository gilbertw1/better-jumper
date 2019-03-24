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
  "Flag inidicating jump in progress to prevent recording unnecessary jumps.")

(defvar better-jumper-switching-perspectives nil
  "Flag indicating if perspective switch is in progress.")

(defvar better-jumper--buffer-targets "\\*\\(new\\|scratch\\)\\*"
  "Regexp to match against `buffer-name' to determine whether it's a valid jump target.")

(cl-defstruct better-jumper-jump-list-struct
  ring
  (idx -1))

(defun better-jumper--copy-struct (struct)
  "Return a copy of STRUCT."
  (let* ((jump-list (better-jumper--get-struct-jump-list struct))
         (struct-copy (make-better-jumper-jump-list-struct)))
    (setf (better-jumper-jump-list-struct-idx struct-copy) (better-jumper-jump-list-struct-idx struct))
    (setf (better-jumper-jump-list-struct-ring struct-copy) (ring-copy jump-list))
    struct-copy))

(defun better-jumper--set-window-struct (window struct)
  "Set jump struct for WINDOW to STRUCT."
   (set-window-parameter window 'better-jumper-struct struct))

(defun better-jumper--set-buffer-struct (buffer struct)
  "TODO: Set jump struct for BUFFER to STRUCT."
    nil)

(defun better-jumper--set-struct (context struct)
  "Set jump struct for CONTEXT to STRUCT."
  (cond ((eq better-jumper-context 'buffer)
         (better-jumper--set-buffer-struct context struct))
        ((eq better-jumper-context 'window)
         (better-jumper--set-window-struct context struct))))

(defun better-jumper--get-current-context ()
  "Get current context item. Either current window or buffer."
  (cond ((eq better-jumper-context 'buffer)
         (current-buffer))
        ((eq better-jumper-context 'window)
         (frame-selected-window))))

(defun better-jumper--get-buffer-struct (&optional buffer)
  "TODO: Get current jump struct for BUFFER.
Creates and adds jump struct if one does not exist. buffer if BUFFER parameter
is missing."
  (let* ((buffer (or buffer (current-buffer))))
    nil))

(defun better-jumper--get-window-struct (&optional window)
  "Get current jump struct for WINDOW.
Creates and adds jump struct if one does not exist. buffer if WINDOW parameter
is missing."
  (let* ((window (or window (frame-selected-window)))
         (jump-struct (window-parameter window 'better-jumper-struct)))
    (unless jump-struct
      (setq jump-struct (make-better-jumper-jump-list-struct))
      (better-jumper--set-struct window jump-struct))
    jump-struct))

(defun better-jumper--get-struct (&optional context)
  "Get current jump struct for CONTEXT.
Creates and adds jump struct if one does not exist. Uses current window or
buffer if CONTEXT parameter is missing."
  (if (eq better-jumper-context 'buffer)
      (better-jumper--get-buffer-struct context)
    (better-jumper--get-window-struct context)))

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

;;;###autoload
(defun better-jumper-get-jumps (window-or-buffer)
  "Get jumps for WINDOW-OR-BUFFER.
The argument should be either a window or buffer depending on the context."
  (let* ((struct (better-jumper--get-struct window-or-buffer))
         (struct-copy (better-jumper--copy-struct struct)))
    struct-copy))

;;;###autoload
(defun better-jumper-set-jumps (window-or-buffer jumps)
  "Set jumps to JUMPS for WINDOW-OR-BUFFER.
The argument should be either a window or buffer depending on the context."
  (let* ((struct-copy (better-jumper--copy-struct jumps)))
    (better-jumper--set-struct window-or-buffer struct-copy)))

;;;;;;;;;;;;;;;;;;
;;;   HOOKS    ;;;
;;;;;;;;;;;;;;;;;;

(defun better-jumper--before-persp-deactivate (&rest args)
  "Save jump state when a perspective is deactivated. Ignore ARGS."
  (setq better-jumper-switching-perspectives t))

(defun better-jumper--on-persp-activate (&rest args)
  "Restore jump state when a perspective is activated. Ignore ARGS."
  (setq better-jumper-switching-perspectives nil))

(with-eval-after-load 'persp-mode
  (add-hook 'persp-before-deactivate-functions #'better-jumper--before-persp-deactivate)
  (add-hook 'persp-activated-functions #'better-jumper--on-persp-activate))

(defun better-jumper--window-configuration-hook (&rest args)
  "Run on window configuration change (Ignore ARGS).
Cleans up deleted windows and copies history to newly created windows."
  (when (and (eq better-jumper-context 'window)
             (eq better-jumper-new-window-behavior 'copy)
             (not better-jumper-switching-perspectives))
    (let* ((window-list (window-list-1 nil nil t)))
      (let* ((curr-window (selected-window))
             (source-jump-struct (better-jumper--get-struct curr-window))
             (source-jump-list (better-jumper--get-struct-jump-list source-jump-struct)))
        (when (not (ring-empty-p source-jump-list))
          (dolist (window window-list)
            (let* ((target-jump-struct (better-jumper--get-struct window))
                   (target-jump-list (better-jumper--get-struct-jump-list target-jump-struct)))
              (when (ring-empty-p target-jump-list)
                (setf (better-jumper-jump-list-struct-idx target-jump-struct) (better-jumper-jump-list-struct-idx source-jump-struct))
                (setf (better-jumper-jump-list-struct-ring target-jump-struct) (ring-copy source-jump-list))))))))))

(add-hook 'window-configuration-change-hook #'better-jumper--window-configuration-hook)

(if better-jumper-use-evil-jump-advice
    (with-eval-after-load 'evil
      (defadvice evil-set-jump (before better-jumper activate)
        (better-jumper-set-jump))))

(push '(better-jumper-struct . writable) window-persistent-parameters)

(provide 'better-jumper)
;;; better-jumper.el ends here
