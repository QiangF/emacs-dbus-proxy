(require 'dbus)

(defvar fcitx-ic-path nil)
(defvar fcitx-preedit-string "")
(defvar fcitx-commit-string "")
(defvar fcitx-candidates nil)

;; 1. Create Context
(setq fcitx-ic-path
      (car (dbus-call-method :session
                             "org.fcitx.Fcitx5"
                             "/org/freedesktop/portal/inputmethod"
                             "org.fcitx.Fcitx.InputMethod1"
                             "CreateInputContext"
                             '((:struct "emacs-fcitx" "emacs")))))

;; 2. Setup Capabilities (Client Side UI)
(let* ((Capability_ClientSideControlState (ash 1 2))
       (Capability_ClientSideInputPanel (ash 1 39))
       ;; Combine them. Note: We are EXCLUDING bits related to shared state
       (cap-flag (logior Capability_ClientSideControlState
                         Capability_ClientSideInputPanel)))
  (dbus-call-method :session "org.fcitx.Fcitx5" fcitx-ic-path
                    "org.fcitx.Fcitx.InputContext1" "SetCapability" :uint64
                    cap-flag))


;; 3. Register Signal Handler
(dbus-register-signal :session
                      "org.fcitx.Fcitx5"
                      fcitx-ic-path
                      "org.fcitx.Fcitx.InputContext1"
                      "UpdateClientSideUI"
                      (lambda (preedit cursorpos auxUp auxDown candidates
                                       candidateIndex layoutHint hasPrev hasNext)
                        (setq fcitx-preedit-string preedit)
                        (setq fcitx-candidates candidates)
                        (message "Preedit: %s Candidates: %s" preedit candidates)))

(dbus-register-signal :session
                      "org.fcitx.Fcitx5"
                      fcitx-ic-path
                      "org.fcitx.Fcitx.InputContext1"
                      "CommitString"
                      (lambda (str)
                        ;; (insert str)
                        ;; Clear preedit since the cycle is finished
                        ;; (setq fcitx-preedit-string "")
                        (setq fcitx-commit-string str)))

;; 4. Focus and Test
(dbus-call-method :session "org.fcitx.Fcitx5" fcitx-ic-path
                  "org.fcitx.Fcitx.InputContext1" "FocusIn")
(dbus-call-method :session
                    "org.fcitx.Fcitx5"
                    "/controller"
                    "org.fcitx.Fcitx.Controller1"
                    "SetCurrentIM"
                    :string "rime")

(defun my-fcitx-send-key (keysym)
  "Send a key press to Fcitx."
  (dbus-call-method :session "org.fcitx.Fcitx5" fcitx-ic-path
                    "org.fcitx.Fcitx.InputContext1" "ProcessKeyEvent"
                    :uint32 keysym :uint32 0 :uint32 0 :boolean nil :uint32 0))

(defun fcitx-send-key (keysym)
  "Sends keysym to Fcitx and returns t if Fcitx handled it."
  (let ((handled (dbus-call-method :session "org.fcitx.Fcitx5" fcitx-ic-path
                                   "org.fcitx.Fcitx.InputContext1" "ProcessKeyEvent"
                                   :uint32 keysym :uint32 0 :uint32 0 :boolean nil :uint32 0)))
    (sleep-for 0.1)
    ;; Fcitx returns a boolean-like integer.
    ;; If handled is non-zero, Fcitx is using it for the IM logic.
    (message "Fcitx handled key %s: %s, candidates %s"
             keysym
             (if handled "YES" "NO")
             fcitx-candidates)))

;; Test: Send 'n'
(fcitx-send-key 110)
(sleep-for 0.5)
;; h
(fcitx-send-key 104)
