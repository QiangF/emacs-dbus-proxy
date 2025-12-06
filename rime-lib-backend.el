
(defvar eim-backend-menu-keys `(("M-n" . 65366) ; Next PageDown
                                ("M-p" . 65365) ; Prior PageUp
                                ("C-n" . 65364) ; C-n Down
                                ("C-p" . 65362) ; Up
                                ("SPC" . 32)    ; Space
                                ,@(mapcar (lambda (x) `(,(char-to-string x) . ,x))
                                          (number-sequence ?0 ?9))))

(defvar eim-backend-composition-keys '(("C-d" . 65535)
                                       ("<deletechar>" . 65535)
                                       ("C-k" . (65505 65535)) ; Shift+Delete
                                       ("DEL" . 65288) ; BackSpace
                                       ("<backspace>" . 65288)
                                       ("<delete>" . 65288)
                                       ("C-b" . 65361) ; Left
                                       ("C-f" . 65363) ; Right
                                       ("C-a" . 65360) ; Home
                                       ("C-e" . 65367))); End

(defun eim-backend-process-key ()
  (rime-lib-process-key))

(defun eim-backend-get-context ()
  (rime-lib-get-context))

(defun eim-backend-get-commit ()
  (rime-lib-get-commit))

(defun eim-backend-clear-composition ()
  (rime-lib-clear-composition))

(defun eim-backend-get-input ()
  (rime-lib-get-input))

(provide 'rime-lib-backend)
