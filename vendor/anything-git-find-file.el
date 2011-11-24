(require 'anything)
(defun list-project-files (repo)
  "Return an alist of all filenames in the project and their path."
  (split-string (shell-command-to-string (format "cd %s && git ls-files" repo))))

(defun find-git-repo (dir)
  "Find base git directory"
  (if (string= "/" dir)
      (message "Not In A Git Repository")
    (if (file-exists-p (expand-file-name ".git/" dir))
        dir
      (find-git-repo (expand-file-name "../" dir)))))

(defvar anything-c-source-git-project-files
  '((name . "Files from Current GIT Project")
    (init . (lambda ()
              (setq anything-git-top-dir
                    (find-git-repo (if (buffer-file-name)
                                       (file-name-directory (buffer-file-name))
                                     default-directory)))))
    (candidates . (lambda ()
                    (if anything-git-top-dir
                        (mapcar (lambda (file)
                                  (concat anything-git-top-dir file))
                                (list-project-files anything-git-top-dir)))))
    (type . file)))

(defun anything-git-find-file ()
  (interactive)
  (anything-other-buffer '(anything-c-source-git-project-files)
                         "*anything-find-in-git*"))

(provide 'anything-git-find-file)
;;; git-find-file.el ends here
