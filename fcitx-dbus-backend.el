(require 'dbus)

;; https://github.com/fcitx/fcitx5/discussions/350
;; dbus 接口的定义：
;; https://github.com/fcitx/fcitx5-qt/blob/master/qt5/dbusaddons/interfaces/org.fcitx.Fcitx.InputContext1.xml
;; https://github.com/fcitx/fcitx5-qt/blob/master/qt5/dbusaddons/interfaces/org.fcitx.Fcitx.InputMethod1.xml

;; 输入法状态是和每个 input context 绑定的
;; 首先 CreateInputContext，可以理解为创建了一个会话/连接，然后返回一个 dbus object path
;; 之后继续用这个 object 和 fcitx 互相通信，发送按键，通过dbus signal获取预编辑和候选词列表
;; 可以使用以下命令查看发送的事件：
;; dbus-monitor --session "type='signal',sender='org.fcitx.Fcitx5'"

(defun fcitx-monitor-handler (keyval keycode state type time)
  (message "keyval %s state %s" keyval state))

;; (dbus-register-monitor :session 'fcitx-monitor-handler
;;                        :type "method_call"
;;                        ;; :sender "org.fcitx.Fcitx5"
;;                        ;; :destination
;;                        :path fcitx-ic-path
;;                        :interface "org.fcitx.Fcitx.InputContext1"
;;                        :member "ProcessKeyEvent")

;; (defvar fcitx-service "org.freedesktop.portal.Fcitx")
(defvar fcitx-service "org.fcitx.Fcitx5")
(defvar fcitx-ic-path nil)
(defvar fcitx-ic-interface "org.fcitx.Fcitx.InputContext1")
(defvar fcitx-im-path "/org/freedesktop/portal/inputmethod")
(defvar fcitx-im-interface "org.fcitx.Fcitx.InputMethod1")

(defvar fcitx-preedit-string nil)
(defvar fcitx-commit-string nil)

(defun fcitx-alive ()
  "Check if theres a running fcitx."
  (dbus-ping :session fcitx-service 100))

(defun fcitx-find-correct-service ()
  "List all registered D-Bus services containing 'Fcitx'."
  (interactive)
  (let ((services (dbus-call-method :session "org.freedesktop.DBus"
                                    "/org/freedesktop/DBus"
                                    "org.freedesktop.DBus"
                                    "ListNames")))
    (message "Found Fcitx-related services: %s"
             (seq-filter (lambda (s) (string-match-p "Fcitx" s)) services))))

(defun fcitx-list-all-im ()
  "Get a list of all available input methods and their unique names."
  (interactive)
  (let ((im-list (dbus-call-method :session "org.fcitx.Fcitx5"
                                   "/controller"
                                   "org.fcitx.Fcitx.Controller1"
                                   "AvailableInputMethods")))
    (with-current-buffer (get-buffer-create "*fcitx-engines*")
      (erase-buffer)
      (dolist (im im-list)
        ;; im is a list: (name native-name icon-name unique-name)
        (insert (format "Name: %s | ID: %s\n" (car im) (nth 3 im))))
      (display-buffer (current-buffer)))
    (message "Listed %d engines in *fcitx-engines*" (length im-list))))

(defun fcitx-get-current-im ()
  "Get the unique name of the currently active Input Method."
  (interactive)
  (let ((im (dbus-call-method :session "org.fcitx.Fcitx5"
                              "/controller"
                              "org.fcitx.Fcitx.Controller1"
                              "CurrentInputMethod")))
    (message "Current IM: %s" im)
    im))

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

(defun fcitx-ic-call-method (method &rest args)
  (apply 'dbus-call-method `(:session ,fcitx-service ,fcitx-ic-path ,fcitx-ic-interface ,method
                                      ,@args)))

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
    (setq fcitx-ic-path (car ic))
    ;; set capability CapabilityFlag::ClientSideInputPanel = (1ULL << 39)
    (fcitx-ic-call-method "SetCapability" :uint64 (ash 1 39))
    ;; focus in
    (fcitx-ic-focusin)
    ;; toggle controller, activate input method
    (dbus-call-method :session
                      "org.fcitx.Fcitx5"
                      "/controller"
                      "org.fcitx.Fcitx.Controller1"
                      "Activate"
                      :timeout 600)
    (dbus-register-signal :session fcitx-service
                          fcitx-ic-path fcitx-ic-interface "CommitString"
                          'fcitx-handler-for-commit-string)
    (dbus-register-signal :session fcitx-service
                          fcitx-ic-path fcitx-ic-interface "UpdateClientSideUI"
                          'fcitx-handler-for-client-ui)
    (dbus-register-signal :session fcitx-service
                          fcitx-ic-path fcitx-ic-interface "UpdateFormattedPreedit"
                          'fcitx-handler-for-preedit-update)))

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

;; ForwardKey(u keyval, u state, b type)
(defun fcitx-handler-for-forward-key (key mask release)
  ;; lookup key in keymap
  )

;; (a(si) preedit, i cursorpos, a(si) auxUp, a(si) auxDown, a(ss) candidates,
;; i candidateIndex, i layoutHint, b hasPrev, b hasNext)
(defun fcitx-handler-for-client-ui (preedit cursorpos auxUp auxDown candidates
                                            candidateIndex layoutHint hasPrev hasNext)
  (setq fcitx-preedit-string preedit))

;; UpdateFormattedPreedit(a(si) str, i cursorpos)
(defun fcitx-handler-for-preedit-update (str cursorpos)
  (setq fcitx-preedit-string str))

;; method without argument
(defun fcitx-ic-focusin ()
  (fcitx-ic-call-method "FocusIn"))

(defun fcitx-ic-focusout ()
  (fcitx-ic-call-method "FocusOut"))

(defun fcitx-ic-reset ()
  (fcitx-ic-call-method "Reset"))

(defun fcitx-ic-destroy ()
  (fcitx-ic-call-method "DestroyIC"))

(defun fcitx-ic-prevpage ()
  (fcitx-ic-call-method "PrevPage"))

(defun fcitx-ic-nextpage ()
  (fcitx-ic-call-method "NextPage"))

;; SelectCandidate(i index)
(defun fcitx-ic-select-candidate (index)
  (fcitx-ic-call-method "SelectCandidate" index))

;; backend interface functions
(defun eim-backend-activate ()
  (fcitx-controller-call "Activate"))

(defun eim-backend-deactivate ()
  (fcitx-controller-call "Deactivate"))

;; keycode can be looked up in keyboard.py
;; keyval can be looked up in keysyms.py
;; or use xev for keysym and keycode
;; ProcessKeyEvent(u keyval, u keycode, u state, b type, u time) = (b ret)
;; bool processKeyEvent
;; (uint32_t keyval, uint32_t keycode, uint32_t state, bool isRelease, uint32_t time)
;; state representing the state of modifier keys (like Shift, Ctrl, Alt) at the time of the event. nil suggests no modifiers were active or the state is not specified. (shift: state 1
;; The last argument, which likely provides a timestamp for the event, probably in milliseconds since a certain epoch, for timing purposes.
;; ProcessKeyEvent(code, 0, mask, false, 0)
;; nil (False) for the type parameter usually means Key Release in some DBus specs, or Key Press depending on the specific implementation. For Fcitx, usually 0 is press and 1 is release. Ensure you are sending a "Press" event to trigger a response.

(defun fcitx-process-key (keysym state)
  ;; (fcitx-ic-call-method "ProcessKeyEvent" keysym 0
  ;;                       state nil (round (time-to-seconds)))
  (fcitx-ic-call-method "ProcessKeyEvent" keysym 0 state nil 0))

;; backend specific key definition
(defvar eim-backend-menu-keys `(("M-n" . #xFF56) ; Next PageDown
                                ("M-p" . #xFF55) ; Prior PageUp
                                ("C-n" . #xFF54) ; C-n Down
                                ("C-p" . #xFF52) ; Up
                                ("SPC" . #x020)  ; Space
                                ,@(mapcar (lambda (x) `(,(char-to-string x) . ,x))
                                          (number-sequence ?0 ?9))))

;; Select = 0xFF60
;; #define XK_Select 0xff60  /* Select, mark */
(defvar eim-backend-composition-keys '(("C-d" . #xFFFF)
                                       ("<deletechar>" . #xFFFF)
                                       ("C-k" . (#xFFFF 1)) ; Shift+Delete
                                       ("DEL" . #xFF08) ; BackSpace
                                       ("<backspace>" . #xFF08)
                                       ("<delete>" . #xFF08)
                                       ("C-b" . #xFF51)   ; Left
                                       ("C-f" . #xFF53)   ; Right
                                       ("C-a" . #xFF50)   ; Home
                                       ("C-e" . #xFF57))) ; End

(defun eim-backend-process-key (keycode &optional mask)
  (fcitx-process-key keysym keycode))

(defun eim-backend-get-context ())

(defun eim-backend-get-commit ())

(defun eim-backend-clear-composition ())

(defun eim-backend-get-input ())

;; after-focus-change-function triggers both at focusin and focusout
(defun eim-focusin ()
  (fcitx-ic-focusin)
  ;; toggle input method based on buffer local variable
  )

(provide 'fcitx-dbus-backend)
