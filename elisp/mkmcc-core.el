;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; emacs-core: Initialization functions for emacs.  Sets up the emacs
;;     path and load path and initializes some constants.  Most of the
;;     work is done in ~/.emacs.d/elisp/personal/
;;
;; Begun by Mike McCourt 2009
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; system type: useful to have
(defconst mkmcc-linux-p
  (or (eq system-type 'gnu/linux) (eq system-type 'linux)))
(defconst mkmcc-macosx-p
  (eq system-type 'darwin))
(defconst mkmcc-aquamacs-p
  (boundp 'aquamacs-version))
(defconst mkmcc-console-p
  (eq (symbol-value 'window-system) nil))
(defconst mkmcc-machine
  (substring (shell-command-to-string "hostname") 0 -1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require-maybe
(defmacro require-maybe (feature &optional file)
  "Try to require FEATURE, but don't signal an error if `require' fails."
  `(require ,feature ,file 'noerror))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; upward find file: used in general and c mode
(defun mkmcc-upward-find-file (filename &optional startdir)
  "Move up directories until we find a certain filename. If we
manage to find it, return the containing directory.  Else if we
get to the toplevel directory and still can't find it, return
nil. Start at startdir or . if startdir not given"
  (let ((dirname (expand-file-name (if startdir startdir ".")))
        (found nil)
        (top nil))
    ;; Traverse directory, looking for filename
    (while (not (or found top))
      (if (string= (expand-file-name dirname) "/")
          (setq top t))
      (if (file-exists-p (expand-file-name filename dirname))
          (setq found t)
        (setq dirname (directory-file-name (file-name-directory dirname)))))
    ;; Return dirname or nil
    (if found dirname nil)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tags
;; update tags file
(defun mkmcc-update-tags ()
  "Create or update the ctags tag file.  Prompts for the top of
the source tree using ido."
  (interactive)
  (let ((olddir default-directory)
        (topdir)
        (dir-buffer)
        (tag-flag))

    ; guess TAGS location, then verify using ido
    (setq topdir (mkmcc-upward-find-file "TAGS" default-directory))
    (unless topdir
      (setq topdir default-directory))
    (setq topdir (ido-read-directory-name "ctags: top of source tree:"
                                          topdir))

    ; run ctags
    (save-excursion
      (setq dir-buffer (find-file-noselect topdir))
      (set-buffer dir-buffer)
      (setq tag-flag
            (call-process "ctags" nil "*ctags errors*" nil
                          "-e" "-a" "-o TAGS" "-R" "."))
      (if (eq tag-flag 0)
          (progn
            (message "Tags created")
            (kill-buffer "*ctags errors*"))
          (progn
            (message "Ctags returned an error")
            (switch-to-buffer-other-window "*ctags errors*")))
      (kill-buffer dir-buffer))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run current file
(defun run-current-file ()
  "Execute or compile the current file.
For example, if the current buffer is the file x.pl, then it'll
call “perl x.pl” in a shell.  The file can be php, perl, python,
ruby, javascript, bash, ocaml, java.  File suffix is used to
determine what program to run."
  (interactive)
  (let (extention-alist fname suffix progName cmdStr)
    (setq extention-alist
          '(("php" . "php")
            ("pl" . "perl")
            ("py" . "python")
            ("rb" . "ruby")
            ("js" . "js")
            ("sh" . "bash")
            ("ml" . "ocaml")
            ("vbs" . "cscript")
            ("java" . "javac")
            ("clj" . "clojure")))
    (setq fname (buffer-file-name))
    (setq suffix (file-name-extension fname))
    (setq progName (cdr (assoc suffix extention-alist)))
    (setq cmdStr (concat progName " \""   fname "\""))

    (if (string-equal suffix "el")
        (load-file fname)
      (if (string-equal suffix "m")
          (progn
            (message "Running...")
            (setq cmdStr
                  (concat "/Applications/Mathematica.app/Contents/MacOS/MathKernel"
                          " -noinit -noprompt -run \"<<"
                          fname "\""))
            (shell-command cmdStr))
        (if progName                    ; is not nil
            (progn
              (message "Running...")
              (shell-command cmdStr))
          (message "No recognized program file suffix for this file."))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(provide 'mkmcc-core)
