;; Sync to xcf.com
(defun sync ()
  "Synchronize to xcf.com."
  (interactive)
  (shell-command "python2 /home/alex/src/xcf/sync push"))
