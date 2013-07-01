;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions used in later init files.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; system type: useful to have
(defconst mkmcc-linux-p
  (or (eq system-type 'gnu/linux) (eq system-type 'linux)))
(defconst mkmcc-macosx-p
  (eq system-type 'darwin))
(defconst mkmcc-console-p
  (eq (symbol-value 'window-system) nil))
(defconst mkmcc-machine
  (substring (shell-command-to-string "hostname") 0 -1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define shortcuts for frequently used things
(defmacro λ (&rest body)
  `(lambda ()
     (interactive)
     ,@body))

(defmacro after-load (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,feature
     '(progn ,@body)))

(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))
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
call \"perl x.pl\" in a shell.  The file can be php, perl, python,
ruby, javascript, bash, ocaml, java.  File suffix is used to
determine what program to run."
  (interactive)
  (let ((extension-alist '(("pl"  . "perl")
                           ("py"  . "python")
                           ("rb"  . "ruby")
                           ("sh"  . "bash")
                           ("m"   . "mash")
                           ("clj" . "clojure")))
        (executable-name nil)
        file-name
        file-suffix
        command-string)

    (setq file-name         (buffer-file-name)
          file-suffix       (file-name-extension file-name)
          executable-name   (cdr (assoc file-suffix extension-alist))
          command-string    (concat executable-name " \"" file-name "\""))

    (cond ((string-equal file-suffix "el")
           (load-file file-name))
          ((stringp executable-name)
           (shell-command command-string))
          (t
           (message "Couldn't interpret file extension.")))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'mkmcc-core)
