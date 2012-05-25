(defcustom network-manager-connect-hook nil
  "List of functions to execute upon successful establishment of
  a network connection."
  :type 'hook
  :group 'network-manager)

(defcustom network-manager-disconnect-hook nil
  "List of functions to execute upon disconnection from the
  network."
  :type 'hook
  :group 'network-manager)

(provide 'network-manager-settings)

;; Only enable all of this if dbus is found
(when (require 'dbus nil nil)

  (defun network-manager-dbus-signal-handler (nmstate)
    "Execute the appropriate hook when Network Manager connects or
  disconnects."
    (case nmstate
      ((4 1) (run-hooks 'network-manager-disconnect-hook))
      (3 (run-hooks 'network-manager-connect-hook))))

  (defvar network-manager-dbus-registration nil)

  (defun network-manager-integration-enable ()
    (interactive)
    (network-manager-integration-disable) ; Make sure to clean up first
    (setq network-manager-dbus-registration
          (dbus-register-signal :system
                                "org.freedesktop.NetworkManager" "/org/freedesktop/NetworkManager"
                                "org.freedesktop.NetworkManager" "StateChanged"
                                'network-manager-dbus-signal-handler)))

  (defun network-manager-integration-disable ()
    (interactive)
    (when network-manager-dbus-registration
      (dbus-unregister-object network-manager-dbus-registration)
      (setq network-manager-dbus-registration nil)))

  ;; Finally, enable it
  ;(network-manager-integration-enable)
  )

