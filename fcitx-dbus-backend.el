(require 'dbus)

;; https://github.com/fcitx/fcitx5/discussions/350
;; dbus 接口的定义：
;; https://github.com/fcitx/fcitx5-qt/blob/master/qt5/dbusaddons/interfaces/org.fcitx.Fcitx.InputContext1.xml
;; https://github.com/fcitx/fcitx5-qt/blob/master/qt5/dbusaddons/interfaces/org.fcitx.Fcitx.InputMethod1.xml

;; 首先 CreateInputContext，可以理解为创建了一个会话/连接，然后返回一个 dbus object path
;; 之后继续用这个 object 和 fcitx 互相通信，发送按键，通过dbus signal获取预编辑和候选词列表

;; (defvar fcitx-service "org.freedesktop.portal.Fcitx")
(defvar fcitx-service "org.fcitx.Fcitx5")
(defvar fcitx-ic-path nil)
(defvar fcitx-ic-interface "org.fcitx.Fcitx.InputContext1")
(defvar fcitx-im-path "/org/freedesktop/portal/inputmethod")
(defvar fcitx-im-interface "org.fcitx.Fcitx.InputMethod1")

(defvar fcitx-pre-edit nil)
(defvar fcitx-commit-string nil)

(defun fcitx-alive ()
  "Check if theres a running fcitx."
  (dbus-ping :session fcitx-service 100))

(defun fcitx-controller-call (method)
  (let ((event last-input-event)
        (result (dbus-call-method :session fcitx-service "/controller"
                                  "org.fcitx.Fcitx.Controller1"
                                  method :timeout 600)))
    (setq last-input-event event)
    result))

;; 1st engine is english input method
(defun fcitx-active-p ()
  (not (equal (fcitx-controller-call "State") 1)))

;; If the CreateInputContext method requires input arguments (as D-Bus methods often do),
;; you would append them as additional arguments to the function call.
;; You can use dbus-introspect-get-signature to determine the exact arguments required for the method.

(defun fcitx-create-input-context (client-name)
  "Input argument: A single string (DBus type s) named client_name.
   Return type: A single object path (DBus type o).

The object path returned points to the newly created input context object,
 which implements the org.fcitx.Fcitx.InputContext1 interface (or similar).
You then interact with this new object path for input method operations. "
  (let ((ic (dbus-call-method :session fcitx-service
                              fcitx-im-path fcitx-im-interface
                              "CreateInputContext"
                              `((:struct "emacs-fcitx" ,client-name)))))
    (setq fcitx-ic-path (car ic))))

(defun fcitx-input-context-call (method &optional args)
  (if args
      (dbus-call-method :session fcitx-service
                        fcitx-ic-path fcitx-ic-interface
                        method args)
    (dbus-call-method :session fcitx-service
                        fcitx-ic-path fcitx-ic-interface
                        method)))

;; method without argument
(defun fcitx-focusin ()
  (fcitx-input-context-call "FocusIn"))

(defun fcitx-focusout ()
  (fcitx-input-context-call "FocusOut"))

(defun fcitx-reset ()
  (fcitx-input-context-call "Reset"))

(defun fcitx-destroy-ic ()
  (fcitx-input-context-call "DestroyIC"))

(defun fcitx-destroy-prevpage ()
  (fcitx-input-context-call "PrevPage"))

(defun fcitx-destroy-nextpage ()
  (fcitx-input-context-call "NextPage"))

;; SelectCandidate(i index)
(defun fcitx-select-candidate (index)
  (fcitx-input-context-call "SelectCandidate" index))

;; keycode can be looked up in keyboard.py
;; keyval can be looked up in keysyms.py
;; or use xev for keysym and keycode
;; ProcessKeyEvent(u keyval, u keycode, u state, b type, u time) = (b ret)
(defun fcitx-process-key (keyval keycode state type)
  (fcitx-input-context-call "ProcessKeyEvent" keyval keycode state type
                            (round (time-to-seconds))))

;; ProcessKeyEventBatch(u nil, u nil, u nil, b nil, u nil) = (a(uv) nil, b nil)
;; SetSurroundingText(s nil, u nil, u nil)
;; SetSurroundingTextPosition(u nil, u nil)
;; InvokeAction(u nil, i nil)
;; SetSupportedCapability(t nil)
;; SetCapability(t nil)

;; signal handler:
;; (s str)
(defun fcitx-handler-for-commit-string (s)
  (setq fcitx-commit-string s))

(dbus-register-signal :session fcitx-service
                      fcitx-ic-path "CommitString"
                      'fcitx-handler-for-commit-string)

;; ForwardKey(u keyval, u state, b type)
(defun fcitx-handler-for-forward-key (keyval state type)
  ;; lookup key in keymap
  )

;; (a(si) preedit, i cursorpos, a(si) auxUp, a(si) auxDown, a(ss) candidates,
;; i candidateIndex, i layoutHint, b hasPrev, b hasNext)
(defun fcitx-handler-for-client-ui (preedit cursorpos auxUp auxDown candidates
                                            candidateIndex layoutHint hasPrev hasNext)
  (setq fcitx-preedit-string s))

(dbus-register-signal :session fcitx-service
                      fcitx-ic-path "UpdateClientSideUI"
                      'fcitx-handler-for-client-ui)

;; UpdateFormattedPreedit(a(si) str, i cursorpos)
(defun fcitx-handler-for-preedit-update (str cursorpos)
  (setq fcitx-preedit-string str))

(dbus-register-signal :session fcitx-service
                      fcitx-ic-path "UpdateFormattedPreedit"
                      'fcitx-handler-for-preedit-update)

;; backend specific key definition
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

;; backend interface functions
(defun eim-backend-activate ()
  (fcitx-controller-call "Activate"))

(defun eim-backend-deactivate ()
  (fcitx-controller-call "Deactivate"))

(defun eim-backend-process-key ())

(defun eim-backend-get-context ())

(defun eim-backend-get-commit ())

(defun eim-backend-clear-composition ())

(defun eim-backend-get-input ())

(provide 'fcitx-dbus-backend)
