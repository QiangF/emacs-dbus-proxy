(require 'dbus)

;; create an input context，save the path to fcitx-ic-path
(setq fcitx-ic-path (car (dbus-call-method :session
                                           "org.fcitx.Fcitx5"
                                           "/org/freedesktop/portal/inputmethod"
                                           "org.fcitx.Fcitx.InputMethod1"
                                           "CreateInputContext"
                                           `((:struct "emacs-fcitx" "emacs")))))

;; set capability CapabilityFlag::ClientSideInputPanel = (1ULL << 39)
(dbus-call-method :session
                  "org.fcitx.Fcitx5"
                  fcitx-ic-path
                  "org.fcitx.Fcitx.InputContext1"
                  "SetCapability" :uint64 (ash 1 39))

;; focus in
(dbus-call-method :session
                  "org.fcitx.Fcitx5"
                  fcitx-ic-path
                  "org.fcitx.Fcitx.InputContext1"
                  "FocusIn")

;; activate input method
;; (dbus-call-method :session
;;                   "org.fcitx.Fcitx5"
;;                   "/controller"
;;                   "org.fcitx.Fcitx.Controller1"
;;                   "Activate"
;;                   :timeout 600)

;; create a variable named "fcitx-preedit-string" to save the preedit string
(defvar fcitx-preedit-string nil)
(defun fcitx-handler-for-client-ui (preedit cursorpos auxUp auxDown candidates
                                            candidateIndex layoutHint hasPrev hasNext)
  (setq fcitx-preedit-string preedit))

;; update the preedit string through dbus signal
(dbus-register-signal :session
                      "org.fcitx.Fcitx5"
                      fcitx-ic-path
                      "org.fcitx.Fcitx.InputContext1"
                      "UpdateClientSideUI"
                      'fcitx-handler-for-client-ui)

(defun fcitx-handler-for-preedit-update (str cursorpos)
  (setq fcitx-preedit-string str))

(dbus-register-signal :session
                      "org.fcitx.Fcitx5"
                      fcitx-ic-path
                      "org.fcitx.Fcitx.InputContext1"
                      "UpdateFormattedPreedit"
                      'fcitx-handler-for-preedit-update)

;; send key “n”，keysym is 110
(dbus-call-method :session
                  "org.fcitx.Fcitx5"
                  fcitx-ic-path
                  ;; "/org/freedesktop/portal/inputcontext/301"
                  "org.fcitx.Fcitx.InputContext1"
                  "ProcessKeyEvent"
                  110 0 0 nil 0)
