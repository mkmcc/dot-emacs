;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dired enhancements
;;
(defun dired-ediff-marked-files ()
  "Run ediff on marked ediff files."
  (interactive)
  (set 'marked-files (dired-get-marked-files))
  (when (= (safe-length marked-files) 2)
    (ediff-files (nth 0 marked-files) (nth 1 marked-files)))

  (when (= (safe-length marked-files) 3)
    (ediff3 (buffer-file-name (nth 0 marked-files))
            (buffer-file-name (nth 1 marked-files))
            (buffer-file-name (nth 2 marked-files)))))

(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 4))

(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

; remap 'o' in dired mode to open a file
(defun dired-open-mac ()
  (interactive)
  (let ((file-name (dired-get-file-for-visit)))
    (if (file-exists-p file-name)
        (call-process "/usr/bin/open" nil 0 nil file-name))))

(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map "o" 'dired-open-mac)
     (define-key dired-mode-map "-" 'dired-up-directory)

     (define-key dired-mode-map
       (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
     (define-key dired-mode-map
       (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)

     (setq dired-recursive-deletes 'always
           dired-recursive-copies 'always
           dired-dwim-target t)

     (put 'dired-find-alternate-file 'disabled nil)

     (defun dired-multi-occur (string)
       "Search string in files marked by dired."
       (interactive "MList lines matching regexp: ")
       (multi-occur (mapcar 'find-file (dired-get-marked-files)) string))))

; prefer dired over dumping dir list to buffer
(global-set-key "\C-x\C-d" 'dired)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'mkmcc-dired)
