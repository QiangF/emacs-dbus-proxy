;;; emacs-eim.el *- lexical-binding: t; -*-

(require 'seq)
(require 'dash)
(require 'popup nil t)
(require 'posframe)

(defgroup eim nil
  "eim is a frontend to external input method"
  :group 'leim)

(defvar eim-backend 'fcitx-dbus-backend "definition for eim-backend functions")

(require eim-backend)

(defun eim--predicate-program-mode-p ()
  "当前为`prog-mode'或`conf-mode'，且光标在注释或字符串当中。"
  (when (derived-mode-p 'prog-mode 'conf-mode)
    (not (or (nth 3 (syntax-ppss))
             (nth 4 (syntax-ppss))))))

(defvar eim--default-cursor '(bar . 4))
(setq-default cursor-type eim--default-cursor)
(setq-default cursor-in-non-selected-windows 'hollow)

(defvar eim--overlay nil
  "Inline english overlay.")

(defun eim--delete-overlay ()
  (delete-overlay eim--overlay)
  (setq cursor-type eim--default-cursor)
  (setq eim--overlay nil))

(defvar eim--inline-cursor '(hbar . 4)
  "Inline english cursor.")

(defface eim--inline-face '((t (:weight bold :box nil :inverse-video nil)))
  "Face to show inline english (input method temperarily disabled) is active.")

(defun eim--predicate-english-context-p ()
  "Return t if English should be inputed at cursor point."
  ;; (message "real this command %s" real-this-command)
  (unless (or (eq real-this-command 'eim--english-inline-deactivate)
              (eq real-this-command 'eim--english-inline-quit)
              (eq real-this-command 'toggle-input-method))
    (let* ((visual-line-beginning (line-beginning-position))
           (point (point))
           (overlay-active (overlayp eim--overlay))
           (english-context
            (or
             ;; 中文后面紧接1个空格切换到英文输入
             ;; \cC represents any character of category “C”, according to “M-x describe-categories”
             (looking-back "\\cC " (max visual-line-beginning (- point 2)))
             (string-match "^\\s-*[0-9]+$" (buffer-substring-no-properties visual-line-beginning point))
             ;; (looking-at-p "^\\*")    ; org heading
             (looking-back "[a-zA-Z\\-]" (max visual-line-beginning (1- point))))))
      (if overlay-active
          (if english-context
              (progn (move-overlay eim--overlay visual-line-beginning (line-end-position))
                     (message "Activate input method with [return]. Quit with [C-g]"))
            (eim--delete-overlay))
        (when english-context
          (setq eim--overlay (make-overlay visual-line-beginning (line-end-position) nil t t))
          (setq cursor-type eim--inline-cursor)
          (overlay-put eim--overlay 'priority 900)
          (overlay-put eim--overlay
                       'face 'eim--inline-face)
          (overlay-put eim--overlay
                       'keymap (let ((keymap (make-sparse-keymap)))
                                 (define-key keymap (kbd "C-g")
                                             #'eim--english-inline-quit)
                                 (define-key keymap (kbd "RET")
                                             #'eim--english-inline-deactivate)
                                 (define-key keymap (kbd "<return>")
                                             #'eim--english-inline-deactivate)
                                 (define-key keymap (kbd "C-\\")
                                             #'eim--english-inline-deactivate)
                                 keymap))))
      english-context)))

(defun eim--english-inline-deactivate ()
  "Deactivate the inline english overlay."
  (interactive)
  (when (overlayp eim--overlay)
    (eim--delete-overlay))
  (eim--activate))

(defun eim--english-inline-quit ()
  "Quit the inline english overlay."
  (interactive)
  (when eim--overlay
    (eim--delete-overlay)
    (eim--deactivate)))

(defvar eim--disable-predicates
  '(eim--predicate-english-context-p
    eim--predicate-program-mode-p))

(defvar eim--suppressed nil)
(make-variable-buffer-local 'eim--suppressed)

(defun eim--suppress-check ()
  (if (equal eim-function 'eim-input-method)
      (let ((suppressed (or (string-match " *temp*" (buffer-name))
                            (seq-find 'funcall eim--disable-predicates nil))))
        (set-cursor-color "red")
        (if suppressed
            (setq cursor-type eim--inline-cursor)
          (setq cursor-type eim--default-cursor))
        (unless (equal eim--suppressed suppressed)
          (redisplay t)
          (setq eim--suppressed suppressed)))
    (eim--restore-cursor)))

(defun eim--text-read-only-p ()
  "Return t if the text at point is read-only."
  (and (get-pos-property (point) 'read-only)
       (not (or inhibit-read-only
                (get-pos-property (point) 'inhibit-read-only)))))

(defun eim--tooltip-minibuffer (tooltip)
  (unless (string-blank-p tooltip)
    (with-selected-window (minibuffer-window)
      (erase-buffer)
      (insert tooltip))))

(defun eim--tooltip-message (tooltip)
  "Concatenate tooltip and message contents."
  (unless (string-blank-p tooltip)
    (let ((message-log-max nil))
      (save-window-excursion
        (with-temp-message
            tooltip
          (sit-for most-positive-fixnum))))))

(defun eim--minibuffer-message (string)
  "Concatenate STRING and minibuffer contents.

Used to display in minibuffer when we are using input method in minibuffer."
  (message nil)
  (unless (string-blank-p string)
    (let ((inhibit-quit t)
          point-1)
      (save-excursion
        (insert (concat "\n" string))
        (setq point-1 (point)))
      (sit-for 1000000)
      (delete-region (point) point-1)
      (when quit-flag
        (setq quit-flag nil
              unread-command-events '(7))))))

(defface eim--tooltip-face
  '((((background light)) :background "#bfffff") (t :background "#400000"))
  "Face with a (presumably) dimmed background for popup.")

(defvar eim--popup nil
  "The current in-use popup.")

(defvar eim-tooltip-max-width 200)

(defun eim--tooltip-popup (tooltip)
  "Display CONTENT with popup.el."
  (when eim--popup
    (popup-delete eim--popup)
    (setq eim--popup nil))
  (unless (string-blank-p tooltip)
    (let* ((col-row (posn-col-row (posn-at-point)))
           (row (cdr col-row))
           (column (car col-row))
           (margin-left 0)
           (margin-right 0)
           (popup-width (+ (reduce 'max (mapcar 'string-width (split-string tooltip "\n")) :initial-value 0)
                           margin-left margin-right 1))
           (win-width (window-width))
           (win-height (window-height))
           (extra-width (- popup-width (- win-width column)))
           (old-point (point))
           (visual-line-beginning (save-excursion
                                    (move-to-column 0)
                                    (point)))
           (new-point (if (> extra-width 0)
                          (- old-point extra-width)
                        old-point)))
      (setq eim--popup
            (eim-popup-tip tooltip
                            :point (if (> new-point visual-line-beginning)
                                       new-point visual-line-beginning)
                            :nowait t)))))

(cl-defun eim-popup-tip (string
                          &key point (height 15) nowait
                          &aux tip lines)
  (let ((it (popup-fill-string string nil eim-tooltip-max-width)))
    (setq width (car it)
          lines (cdr it))
    (setq tip (popup-create point width height
                            :max-width eim-tooltip-max-width
                            :around t
                            :face 'popup-tip-face))
    (unwind-protect
        (when (> (popup-width tip) 0)   ; not to be corrupted
          (when (not (eq width (popup-width tip)))
            ;; Refill once again to lines be fitted to popup width
            (setq width (popup-width tip))
            (setq lines (cdr (popup-fill-string string width width))))
          (popup-set-list tip lines)
          (popup-draw tip)
          tip))))

(defvar eim--posframe-buffer " *eim-posframe*"
  "The buffer name for candidate posframe.")

(defun eim--tooltip-posframe (tooltip)
  (if (string-blank-p tooltip)
      (posframe-hide eim--posframe-buffer)
    (posframe-show eim--posframe-buffer
                   :foreground-color (face-attribute 'eim--tooltip-face :foreground)
                   :background-color (face-attribute 'eim--tooltip-face :background)
                   :string tooltip)))

(defvar eim--tooltip-method 'posframe
  "选词框显示方式，`popup' `posframe' `message' or `minibuffer'.")

(defvar eim--overriding nil)

(defun eim--map-set ()
  (setq overriding-terminal-local-map eim--map)
  (setq eim--overriding t))

(defun eim--map-unset ()
  (setq overriding-terminal-local-map nil)
  (setq eim--overriding nil))

(defun eim--tooltip-update (&optional context)
  (let* ((context (or context (eim-backend-get-context)))
         (tooltip (eim--build-tooltip context)))
    (if tooltip
        (eim--map-set)
      (setq tooltip "")
      (eim--map-unset))
    (case eim--tooltip-method
      (popup (eim--tooltip-popup tooltip))
      (posframe (eim--tooltip-posframe tooltip))
      (message (eim--tooltip-message tooltip))
      (minibuffer (eim--tooltip-minibuffer tooltip))
      (t (eim--tooltip-minibuffer tooltip)))))

(defun eim--activate (&optional _name)
  (unless buffer-read-only
    (setq-local eim-function 'eim-input-method)
    (setq-local deactivate-current-eim-function #'eim--deactivate)
    (add-hook 'post-command-hook 'eim--suppress-check)
    (advice-add 'keyboard-quit :after 'eim--map-unset)
    (eim-backend-clear-composition)
    (redisplay t)))

;; Another special face is the cursor face.
;; On graphical displays, the background color of this face is used to draw the text cursor.
;; None of the other attributes of this face have any effect.
;; As the foreground color for text under the cursor is taken from the background color of the underlying text.
;; On text terminals, the appearance of the text cursor is determined by the terminal, not by the cursor face.
(defun eim--restore-cursor ()
  (custom-set-faces
   '(cursor ((t (:inherit font-lock-keyword-face)))))
  (setq cursor-type eim--default-cursor))

(defun eim--deactivate ()
  (kill-local-variable 'eim-function)
  (remove-hook 'post-command-hook 'eim--suppress-check)
  (advice-remove 'keyboard-quit 'eim--map-unset)
  (eim-backend-clear-composition)
  (eim--tooltip-update)
  (eim--restore-cursor)
  (redisplay t))

(defun eim--build-tooltip (context)
  "Build candidate menu tooltip from eim context."
  (let* ((context (or context (eim-backend-get-context)))
         (composition (alist-get 'composition context))
         (composition-length (alist-get 'length composition))
         (preedit (alist-get 'preedit composition)))
    (when preedit
      (progn
        ;; format preedit
        (let* ((cursor-pos (alist-get 'cursor-pos composition))
               (cursor-distance-to-end (- composition-length cursor-pos))
               ;; (select-labels (alist-get 'select-labels context))
               ;; (commit-text-preview (alist-get 'commit-text-preview context))
               ;; (sel-start (alist-get 'sel-start composition))
               ;; (sel-end (alist-get 'sel-end composition))
               ;; (input (eim-backend-get-input))
               (menu (alist-get 'menu context))
               prompt-str page-str candidate-str-list candidate-str-draft)
          (setq prompt-str (with-temp-buffer
                             (insert preedit)
                             (backward-char cursor-distance-to-end)
                             (insert "˰")
                             (buffer-string)))
          (when menu
            (let* ((highlighted-candidate-index (alist-get 'highlighted-candidate-index menu))
                   (last-page-p (alist-get 'last-page-p menu))
                   (num-candidates (alist-get 'num-candidates menu))
                   (page-no (alist-get 'page-no menu))
                   (candidates (alist-get 'candidates menu)))
              (setq page-str (if last-page-p (format "(%s<)" (1+ page-no))
                               (format "(%s)" (1+ page-no))))
              (dolist (i (number-sequence 0 (1- num-candidates)))
                (push (if (= i highlighted-candidate-index)
                          (format "[%d%s]" (1+ i) (car (nth i candidates)))
                        (format "%d%s" (1+ i) (car (nth i candidates))))
                      candidate-str-list))))
          (setq candidate-str-list (reverse candidate-str-list))
          (setq candidate-str-draft (mapconcat #'identity candidate-str-list " "))
          (when (> (string-width candidate-str-draft) eim-tooltip-max-width)
              (setq candidate-str-draft (mapconcat #'identity candidate-str-list "\n")))
          (concat prompt-str page-str "\n" candidate-str-draft))))))

(defun eim--escape ()
  "Clear the composition."
  (interactive)
  (eim-backend-clear-composition)
  (eim--tooltip-update))

(defun eim--return ()
  "Commit the raw input."
  (interactive)
  (when-let ((input (eim-backend-get-input)))
    (insert input)
    (eim-backend-clear-composition)
    (eim--tooltip-update)))

(defun eim--send-functional-key ()
  (interactive)
  (let* ((keyseq (vector last-input-event))
         (keyseq-name (key-description keyseq))
         (state 0)
         (keysym (cdr (or (assoc keyseq-name eim-backend-menu-keys)
                          (assoc keyseq-name eim-backend-composition-keys)))))
    (when (listp keysym)
      (setq state (cadr keysym)
            keysym (car keysym)))
    (eim-backend-process-key keysym state)
    (eim--tooltip-update)))

(defvar eim--map
  (let ((map (make-sparse-keymap)))
    (dolist (i (append eim-backend-menu-keys eim-backend-composition-keys))
      (define-key map (kbd (car i)) 'eim--send-functional-key))
    (define-key map (kbd "<return>") 'eim--return)
    (define-key map (kbd "<escape>") 'eim--escape)
    (define-key map (kbd "C-g") 'eim--escape)
    map))

(defun eim-input-method (key)
  "Process character KEY with input method, other keys not handled."
  (if (or eim--suppressed
          ;; (lookup-key overriding-terminal-local-map (vector key))
          ;; (eq (cadr overriding-terminal-local-map) universal-argument-map)
          ;; (and overriding-terminal-local-map
          ;;      (not (equal (cadr overriding-terminal-local-map) eim--map)))
          (and (or overriding-local-map overriding-terminal-local-map)
               (not eim--overriding))
          ;; upper case letter
          ;; (and (> key 64) (< key 91))
          (eim--text-read-only-p))
      (list key)
    (let ((handled (eim-backend-process-key key 0)))
      (with-silent-modifications
        (let* ((context (eim-backend-get-context))
               (composition (alist-get 'composition context))
               (preedit (alist-get 'preedit composition))
               (commit (eim-backend-get-commit)))
          (eim--tooltip-update context)
          ;; commit is still nil when composition is active
          (unwind-protect
              (cond (commit
                     (eim-backend-clear-composition)
                     (mapcar 'identity commit))
                    ((not handled)
                     (list key))
                    (t))))))))

(register-input-method "eim" "euc-cn" 'eim--activate "ㄓ" "chinese input method")

(provide 'emacs-eim)

;;; emacs-eim.el ends here
