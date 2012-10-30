(deftheme birds-of-paradise
  "Port of the brown-based warm light-on-dark theme by Joe
Bergantine.  Modified significantly by mkmcc, with no attempt at
compatibility.")

(let* ((class '((class color) (min-colors 89)))
       (brown-1  "#452E2E")
       (brown-2  "#865C38")
       (brown-3  "#4B3330")
       (brown-4  "#523D2B")
       (brown-5  "#7D504A")
       (brown-6  "#392626")
       (brown-7  "#3E2929")
       ;;
       (white-1  "#E6E1C4")
       (white-2  "#E6E1DC")
       (white-3  "#654D4D")
       ;;
       (black-1  "#1F1611")
       (black-2  "#16120E")
       (black-3  "#000000")
       (gray-1   "#4e4e4e")
       ;;
       (yellow-1 "#D9D762")
       (yellow-2 "#EFAC32")
       (yellow-3 "#EFCB43")
       (yellow-4 "#EFC232")
       (yellow-5 "#FFC05C")
       ;;
       (orange-1 "#EF5D32")
       (orange-2 "#CC762E")
       (orange-3 "#C74725")
       (red-1    "#990000")
       (red-2    "#660000")
       (red-3    "#CC4232")
       (red-4    "#BE3250")
       (red-5    "#D23850")
       (red-6    "firebrick4")
       (red-7    "#FF7575")
       ;;
       (blue-1   "#7DAF9C")
       (blue-2   "#6C99BB")
       (blue-3   "#5798AE")
       (blue-4   "#93C1BC")
       (blue-5   "#2F33AB")
       (purple-1 "#BB99BB")
       (purple-2 "#8856D2")
       (purple-3 "#BE73FD")
       (green-1  "#144212")
       (green-2  "#8CFF8C"))
  (custom-theme-set-faces
   'birds-of-paradise

   `(button ((t (:foreground ,yellow-1 :background ,brown-3 :weight bold :underline t))))
   `(link ((t (:foreground ,yellow-1))))
   `(link-visited ((,class (:foreground ,yellow-1 :underline t :weight normal))))

   ;; basic coloring
   `(default ((,class (:foreground ,white-1 :background ,brown-1))))
   `(cursor ((,class (:foreground ,brown-2 :background ,white-1))))
   `(escape-glyph ((t (:foreground ,purple-3))))
   `(column-marker-1 ((t (:background ,brown-4))))
   `(fringe ((t (:background ,brown-7 :foreground ,white-3))))
   `(header-line ((,class (:foreground ,yellow-2
                                       :background ,brown-4
                                       :box (:line-width -1 :style nil)))))
   `(highlight ((t (:background ,black-1 :foreground ,white-1))))
   `(highlight-indentation-current-column-face ((t (:background ,brown-4))))
   `(highlight-indentation-face ((t (:background ,brown-3))))

   ;; compilation
   `(compilation-column-face ((,class (:foreground ,yellow-5))))
   `(compilation-enter-directory-face ((,class (:foreground ,blue-1))))
   `(compilation-error-face ((,class (:foreground ,red-6 :weight bold :underline t))))
   `(compilation-face ((,class (:foreground ,white-1))))
   `(compilation-info-face ((,class (:foreground ,blue-2))))
   `(compilation-info ((,class (:foreground ,blue-4 :underline t))))
   `(compilation-leave-directory-face ((,class (:foreground ,blue-1))))
   `(compilation-line-face ((,class (:foreground ,yellow-4))))
   `(compilation-line-number ((,class (:foreground ,yellow-1))))
   `(compilation-message-face ((,class (:foreground ,blue-2))))
   `(compilation-warning-face ((,class (:foreground ,orange-1 :weight bold :underline t))))

   ;; grep
   `(grep-context-face ((,class (:foreground ,white-1))))
   `(grep-error-face ((,class (:foreground ,red-6 :weight bold :underline t))))
   `(grep-hit-face ((,class (:foreground ,blue-2))))
   `(grep-match-face ((,class (:foreground ,orange-1 :weight bold))))
   `(match ((,class (:background ,brown-4 :foreground ,orange-1 :weight bold))))

   ;; faces used by isearch
   `(isearch ((,class (:foreground ,yellow-4 :background ,brown-4))))
   `(isearch-fail ((,class (:foreground ,white-1 :background ,red-6))))
   `(lazy-highlight ((,class (:foreground ,yellow-2 :background ,brown-4))))

   `(menu ((,class (:foreground ,white-1 :background ,brown-1))))
   `(minibuffer-prompt ((t (:foreground ,blue-2 :weight bold)))) ;todo
   `(region ((t (:background ,brown-4 :foreground ,white-1))))
   `(secondary-selection ((,class (:background ,brown-1))))
   `(trailing-whitespace ((,class (:background ,red-1))))
   `(vertical-border ((t (:foreground ,brown-4))))

  ;; modeline
   `(mode-line
     ((,class (:foreground ,white-1
                           :background ,brown-2
                           :box (:line-width 6 :color ,brown-2 :style nil)))))
   `(mode-line-inactive
     ((,class (:foreground ,white-1
                           :background ,brown-4
                           :box (:line-width 6 :color ,brown-4 :style nil)))))
   `(mode-line-read-only-face
     ((,class (:foreground ,blue-4
                           :box (:line-width 2 :color ,blue-4 :style nil)))))
   `(mode-line-blank-face
     ((,class (:inherit mode-line-face
                        :box (:line-width 2 :color ,brown-2
                                          :style nil)))))
   `(mode-line-modified-face
     ((,class (:inherit font-lock-warning-face :underline nil
                        :box (:line-width 2)))))
   `(mode-line-folder-face
     ((,class (:inherit mode-line-face :foreground ,brown-1))))

   `(mode-line-folder-face
     ((,class (:inherit mode-line-face :foreground ,yellow-1 :weight bold))))

   `(mode-line-position-face
     ((,class (:foreground ,brown-1))))

   `(mode-line-mode-face
     ((,class (:foreground ,blue-1 :weight bold))))

   `(mode-line-minor-mode-face
     ((,class (:foreground ,blue-3 :height 0.75))))

   `(mode-line-process-face
     ((,class (:inherit mode-line-face :foreground ,green-2))))

   `(mode-line-80col-face
     ((,class (:foreground "black" :background ,yellow-2))))

   `(mode-line-buffer-id ((,class (:foreground ,white-3 :weight bold))))


   ;; font lock
   `(font-lock-builtin-face ((,class (:foreground ,blue-2))))
   `(font-lock-comment-face ((,class (:foreground ,brown-2 :slant italic))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,brown-2))))
   `(font-lock-constant-face ((,class (:foreground ,blue-3))))
   `(font-lock-doc-face ((,class (:foreground ,brown-2))))
   `(font-lock-doc-string-face ((,class (:foreground ,brown-2))))
   `(font-lock-function-name-face ((,class (:foreground ,yellow-2))))
   `(font-lock-keyword-face ((,class (:foreground ,orange-1 :weight bold))))
   `(font-lock-negation-char-face ((,class (:foreground ,blue-1))))
   `(font-lock-preprocessor-face ((,class (:foreground ,red-4))))
   `(font-lock-string-face ((,class (:foreground ,yellow-1))))
   `(font-lock-type-face ((,class (:foreground ,yellow-2))))
   `(font-lock-variable-name-face ((,class (:foreground ,blue-1))))
   `(font-lock-warning-face ((t (:background ,yellow-2 :foreground ,white-3))))


   `(c-annotation-face ((,class (:inherit font-lock-constant-face))))

   `(nxml-comment-content ((t (:inherit 'font-lock-comment-face))))
   `(nxml-attribute-local-name ((t (:foreground ,orange-1))))
   `(nxml-attribute-value ((t (:foreground ,yellow-1))))
   `(nxml-cdata-section-content ((t (:foreground ,red-3))))
   `(nxml-comment-content ((t (:inherit 'font-lock-comment-face))))
   `(nxml-element-local-name ((t (:foreground ,yellow-3))))
   `(nxml-entity-ref-delimiter ((t (:foreground ,blue-2))))
   `(nxml-entity-ref-name ((t (:foreground ,blue-2))))
   `(nxml-processing-instruction-target ((t (:foreground ,brown-2))))
   `(nxml-tag-delimiter ((t (:foreground ,yellow-3))))

   `(elscreen-tab-background-face ((t (:background ,brown-3))))
   `(elscreen-tab-control-face ((t (:background ,brown-2 :foreground ,white-1 :underline nil))))
   `(elscreen-tab-current-screen-face ((t (:background ,brown-2 :foreground ,white-1))))
   `(elscreen-tab-other-screen-face ((t (:background ,brown-3 :foreground ,white-1 :underline nil))))


   ;;; external

   ;; diff
   `(diff-added ((,class (:foreground ,blue-1))))
   `(diff-changed ((,class (:foreground ,yellow-1))))
   `(diff-removed ((,class (:foreground ,red-1))))
   `(diff-header ((,class (:background ,brown-1))))
   `(diff-file-header ((,class (:background ,brown-1 :foreground ,white-1 :bold t))))

   ;; eshell
   `(eshell-prompt ((,class (:foreground ,yellow-4 :weight bold))))
   `(eshell-ls-archive ((,class (:foreground ,red-4 :weight bold))))
   `(eshell-ls-backup ((,class (:inherit font-lock-comment))))
   `(eshell-ls-clutter ((,class (:inherit font-lock-comment))))
   `(eshell-ls-directory ((,class (:foreground ,blue-2 :weight bold))))
   `(eshell-ls-executable ((,class (:foreground ,red-5 :weight bold))))
   `(eshell-ls-unreadable ((,class (:foreground ,white-1))))
   `(eshell-ls-missing ((,class (:inherit font-lock-warning))))
   `(eshell-ls-product ((,class (:inherit font-lock-doc))))
   `(eshell-ls-special ((,class (:foreground ,yellow-2 :weight bold))))
   `(eshell-ls-symlink ((,class (:foreground ,blue-1 :weight bold))))

   ;; flymake
   `(flymake-errline ((,class (:foreground ,red-3 :weight bold :underline t))))
   `(flymake-warnline ((,class (:foreground ,yellow-5 :weight bold :underline t))))

   ;; flyspell
   `(flyspell-duplicate ((,class (:foreground ,yellow-4 :weight bold :underline t))))
   `(flyspell-incorrect ((,class (:foreground ,red-6 :weight bold :underline t))))

   ;; erc
   `(erc-action-face ((,class (:inherit erc-default-face))))
   `(erc-bold-face ((,class (:weight bold))))
   `(erc-current-nick-face ((,class (:foreground ,blue-2 :weight bold))))
   `(erc-dangerous-host-face ((,class (:inherit font-lock-warning))))
   `(erc-default-face ((,class (:foreground ,white-1))))
   `(erc-direct-msg-face ((,class (:inherit erc-default))))
   `(erc-error-face ((,class (:inherit font-lock-warning))))
   `(erc-fool-face ((,class (:inherit erc-default))))
   `(erc-highlight-face ((,class (:inherit hover-highlight))))
   `(erc-input-face ((,class (:foreground ,yellow-1))))
   `(erc-keyword-face ((,class (:foreground ,blue-2 :weight bold))))
   `(erc-nick-default-face ((,class (:foreground ,yellow-1 :weight bold))))
   `(erc-my-nick-face ((,class (:foreground ,red-1 :weigth bold))))
   `(erc-nick-msg-face ((,class (:inherit erc-default))))
   `(erc-notice-face ((,class (:foreground ,green-2))))
   `(erc-pal-face ((,class (:foreground ,orange-3 :weight bold))))
   `(erc-prompt-face ((,class (:foreground ,orange-3 :background ,brown-1 :weight bold))))
   `(erc-timestamp-face ((,class (:foreground ,green-2))))
   `(erc-underline-face ((t (:underline t))))

   ;; gnus
   `(gnus-group-mail-1-face ((,class (:bold t :inherit gnus-group-mail-1-empty))))
   `(gnus-group-mail-1-empty-face ((,class (:inherit gnus-group-news-1-empty))))
   `(gnus-group-mail-2-face ((,class (:bold t :inherit gnus-group-mail-2-empty))))
   `(gnus-group-mail-2-empty-face ((,class (:inherit gnus-group-news-2-empty))))
   `(gnus-group-mail-3-face ((,class (:bold t :inherit gnus-group-mail-3-empty))))
   `(gnus-group-mail-3-empty-face ((,class (:inherit gnus-group-news-3-empty))))
   `(gnus-group-mail-4-face ((,class (:bold t :inherit gnus-group-mail-4-empty))))
   `(gnus-group-mail-4-empty-face ((,class (:inherit gnus-group-news-4-empty))))
   `(gnus-group-mail-5-face ((,class (:bold t :inherit gnus-group-mail-5-empty))))
   `(gnus-group-mail-5-empty-face ((,class (:inherit gnus-group-news-5-empty))))
   `(gnus-group-mail-6-face ((,class (:bold t :inherit gnus-group-mail-6-empty))))
   `(gnus-group-mail-6-empty-face ((,class (:inherit gnus-group-news-6-empty))))
   `(gnus-group-mail-low-face ((,class (:bold t :inherit gnus-group-mail-low-empty))))
   `(gnus-group-mail-low-empty-face ((,class (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-1-face ((,class (:bold t :inherit gnus-group-news-1-empty))))
   `(gnus-group-news-2-face ((,class (:bold t :inherit gnus-group-news-2-empty))))
   `(gnus-group-news-3-face ((,class (:bold t :inherit gnus-group-news-3-empty))))
   `(gnus-group-news-4-face ((,class (:bold t :inherit gnus-group-news-4-empty))))
   `(gnus-group-news-5-face ((,class (:bold t :inherit gnus-group-news-5-empty))))
   `(gnus-group-news-6-face ((,class (:bold t :inherit gnus-group-news-6-empty))))
   `(gnus-group-news-low-face ((,class (:bold t :inherit gnus-group-news-low-empty))))
   `(gnus-header-content-face ((,class (:inherit message-header-other))))
   `(gnus-header-from-face ((,class (:inherit message-header-from))))
   `(gnus-header-name-face ((,class (:inherit message-header-name))))
   `(gnus-header-newsgroups-face ((,class (:inherit message-header-other))))
   `(gnus-header-subject-face ((,class (:inherit message-header-subject))))
   `(gnus-summary-cancelled-face ((,class (:foreground ,orange-2))))
   `(gnus-summary-high-ancient-face ((,class (:foreground ,blue-2))))
   `(gnus-summary-high-read-face ((,class (:foreground ,blue-1 :weight bold))))
   `(gnus-summary-high-ticked-face ((,class (:foreground ,orange-1 :weight bold))))
   `(gnus-summary-high-unread-face ((,class (:foreground ,white-1 :weight bold))))
   `(gnus-summary-low-ancient-face ((,class (:foreground ,blue-2))))
   `(gnus-summary-low-read-face ((t (:foreground ,blue-1))))
   `(gnus-summary-low-ticked-face ((,class (:foreground ,orange-1 :weight bold))))
   `(gnus-summary-low-unread-face ((,class (:foreground ,white-1))))
   `(gnus-summary-normal-ancient-face ((,class (:foreground ,blue-2))))
   `(gnus-summary-normal-read-face ((,class (:foreground ,green-2))))
   `(gnus-summary-normal-ticked-face ((,class (:foreground ,orange-3 :weight bold))))
   `(gnus-summary-normal-unread-face ((,class (:foreground ,white-1))))
   `(gnus-summary-selected-face ((,class (:foreground ,yellow-1 :weight bold))))
   `(gnus-cite-1-face ((,class (:foreground ,blue-2))))
   `(gnus-cite-10-face ((,class (:foreground ,yellow-1))))
   `(gnus-cite-11-face ((,class (:foreground ,yellow-1))))
   `(gnus-cite-2-face ((,class (:foreground ,blue-2))))
   `(gnus-cite-3-face ((,class (:foreground ,blue-2))))
   `(gnus-cite-4-face ((,class (:foreground ,blue-1))))
   `(gnus-cite-5-face ((,class (:foreground ,blue-4))))
   `(gnus-cite-6-face ((,class (:foreground ,blue-1))))
   `(gnus-cite-7-face ((,class (:foreground ,red-6))))
   `(gnus-cite-8-face ((,class (:foreground ,red-7))))
   `(gnus-cite-9-face ((,class (:foreground ,red-6))))
   `(gnus-group-news-1-empty-face ((,class (:foreground ,yellow-1))))
   `(gnus-group-news-2-empty-face ((,class (:foreground ,green-2))))
   `(gnus-group-news-3-empty-face ((,class (:foreground ,green-2))))
   `(gnus-group-news-4-empty-face ((,class (:foreground ,blue-2))))
   `(gnus-group-news-5-empty-face ((,class (:foreground ,blue-2))))
   `(gnus-group-news-6-empty-face ((,class (:foreground ,brown-1))))
   `(gnus-group-news-low-empty-face ((,class (:foreground ,brown-1))))
   `(gnus-signature-face ((,class (:foreground ,yellow-2))))
   `(gnus-x-face ((,class (:background ,white-1 :foreground ,brown-1))))

   ;; hl-line-mode
   `(hl-line ((t (:background ,brown-7))))

   ;; ido-mode
   `(ido-first-match ((,class (:foreground ,orange-1 :weight bold))))
   `(ido-only-match ((,class (:foreground ,purple-3 :weight bold))))
   `(ido-subdir ((,class (:foreground ,yellow-2))))

   ;; linum-mode
   `(linum ((t (:background ,brown-1 :foreground ,white-3))))

   ;; magit
   `(magit-item-highlight ((t (:background ,brown-6))))
   `(magit-section-title ((,class (:foreground ,yellow-2 :weight bold))))
   `(magit-branch ((,class (:foreground ,orange-2 :weight bold))))
   `(magit-diff-add ((t (:foreground ,green-2))))
   `(magit-diff-del ((t (:foreground ,red-7))))

   ;; message-mode
   `(message-cited-text ((t (:foreground ,yellow-2))))
   `(message-header-cc ((t (:foreground ,brown-2))))
   `(message-header-name ((t (:foreground ,orange-1))))
   `(message-header-other ((t (:foreground ,brown-2))))
   `(message-header-subject ((t (:foreground ,white-1))))
   `(message-header-to ((t (:foreground ,white-1))))
   `(message-separator ((t (:foreground ,yellow-2))))
   `(message-header-from-face ((,class (:foreground ,yellow-4 :weight bold))))
   `(message-header-newsgroups-face ((,class (:foreground ,yellow-1 :weight bold))))
   `(message-header-subject-face ((,class (:foreground ,orange-1 :weight bold))))
   `(message-header-xheader-face ((,class (:foreground ,green-2))))
   `(message-mml-face ((,class (:foreground ,yellow-3 :weight bold))))
   `(message-separator-face ((,class (:inherit font-lock-comment))))

   ;; mew
   `(mew-face-header-subject ((,class (:foreground ,orange-1))))
   `(mew-face-header-from ((,class (:foreground ,yellow-2))))
   `(mew-face-header-date ((,class (:foreground ,green-2))))
   `(mew-face-header-to ((,class (:foreground ,red-1))))
   `(mew-face-header-key ((,class (:foreground ,green-2))))
   `(mew-face-header-private ((,class (:foreground ,green-2))))
   `(mew-face-header-important ((,class (:foreground ,blue-2))))
   `(mew-face-header-marginal ((,class (:foreground ,white-1 :weight bold))))
   `(mew-face-header-warning ((,class (:foreground ,red-3))))
   `(mew-face-header-xmew ((,class (:foreground ,green-2))))
   `(mew-face-header-xmew-bad ((,class (:foreground ,red-4))))
   `(mew-face-body-url ((,class (:foreground ,orange-2))))
   `(mew-face-body-comment ((,class (:foreground ,white-1 :slant italic))))
   `(mew-face-body-cite1 ((,class (:foreground ,green-2))))
   `(mew-face-body-cite2 ((,class (:foreground ,blue-2))))
   `(mew-face-body-cite3 ((,class (:foreground ,orange-3))))
   `(mew-face-body-cite4 ((,class (:foreground ,yellow-5))))
   `(mew-face-body-cite5 ((,class (:foreground ,red-5))))
   `(mew-face-mark-review ((,class (:foreground ,blue-2))))
   `(mew-face-mark-escape ((,class (:foreground ,green-2))))
   `(mew-face-mark-delete ((,class (:foreground ,red-6))))
   `(mew-face-mark-unlink ((,class (:foreground ,yellow-4))))
   `(mew-face-mark-refile ((,class (:foreground ,green-2))))
   `(mew-face-mark-unread ((,class (:foreground ,red-7))))
   `(mew-face-eof-message ((,class (:foreground ,green-2))))
   `(mew-face-eof-part ((,class (:foreground ,yellow-3))))

   ;; nav
   `(nav-face-heading ((,class (:foreground ,yellow-1))))
   `(nav-face-button-num ((,class (:foreground ,blue-3))))
   `(nav-face-dir ((,class (:foreground ,green-2))))
   `(nav-face-hdir ((,class (:foreground ,red-1))))
   `(nav-face-file ((,class (:foreground ,white-1))))
   `(nav-face-hfile ((,class (:foreground ,red-3))))

   ;; org-mode
   `(org-agenda-date-today ((,class (:foreground ,white-1 :slant italic :weight bold))) t)
   `(org-agenda-structure ((,class (:inherit font-lock-comment-face))))
   `(org-archived ((,class (:foreground ,white-1 :weight bold))))
   `(org-checkbox ((,class (:background ,brown-1 :foreground ,white-1 :box (:line-width 1 :style nil)))))
   `(org-date ((,class (:foreground ,blue-2 :underline t))))
   `(org-deadline-announce ((,class (:foreground ,red-1))))
   `(org-done ((,class (:bold t :weight bold :foreground ,green-2))))
   `(org-formula ((,class (:foreground ,yellow-5))))
   `(org-headline-done ((,class (:foreground ,green-2))))
   `(org-hide ((,class (:foreground ,brown-1))))
   `(org-level-1 ((,class (:foreground ,orange-1))))
   `(org-level-2 ((,class (:foreground ,blue-1))))
   `(org-level-3 ((,class (:foreground ,blue-2))))
   `(org-level-4 ((,class (:foreground ,yellow-3))))
   `(org-level-5 ((,class (:foreground ,blue-4))))
   `(org-level-6 ((,class (:foreground ,green-2))))
   `(org-level-7 ((,class (:foreground ,red-3))))
   `(org-level-8 ((,class (:foreground ,blue-2))))
   `(org-link ((,class (:foreground ,yellow-1 :underline t))))
   `(org-scheduled ((,class (:foreground ,blue-2))))
   `(org-scheduled-previously ((,class (:foreground ,red-1))))
   `(org-scheduled-today ((,class (:foreground ,blue-2))))
   `(org-special-keyword ((,class (:foreground ,yellow-5))))
   `(org-table ((,class (:foreground ,green-2))))
   `(org-tag ((,class (:bold t :weight bold))))
   `(org-time-grid ((,class (:foreground ,orange-2))))
   `(org-todo ((,class (:bold t :foreground ,red-1 :weight bold))))
   `(org-upcoming-deadline ((,class (:inherit font-lock-keyword-face))))
   `(org-warning ((,class (:bold t :foreground ,red-3 :weight bold))))

   ;; outline
   `(outline-8 ((,class (:inherit default))))
   `(outline-7 ((,class (:inherit outline-8 :height 1.0))))
   `(outline-6 ((,class (:inherit outline-7 :height 1.0))))
   `(outline-5 ((,class (:inherit outline-6 :height 1.0))))
   `(outline-4 ((,class (:inherit outline-5 :height 1.0))))
   `(outline-3 ((,class (:inherit outline-4 :height 1.0))))
   `(outline-2 ((,class (:inherit outline-3 :height 1.0))))
   `(outline-1 ((,class (:inherit outline-2 :height 1.0))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,blue-1))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground ,yellow-1))))
   `(rainbow-delimiters-depth-3-face ((,class (:foreground ,blue-2))))
   `(rainbow-delimiters-depth-4-face ((,class (:foreground ,red-1))))
   `(rainbow-delimiters-depth-5-face ((,class (:foreground ,orange-3))))
   `(rainbow-delimiters-depth-6-face ((,class (:foreground ,blue-5))))
   `(rainbow-delimiters-depth-7-face ((,class (:foreground ,green-2))))
   `(rainbow-delimiters-depth-8-face ((,class (:foreground ,red-3))))
   `(rainbow-delimiters-depth-9-face ((,class (:foreground ,yellow-5))))
   `(rainbow-delimiters-depth-10-face ((,class (:foreground ,green-2))))
   `(rainbow-delimiters-depth-11-face ((,class (:foreground ,blue-2))))
   `(rainbow-delimiters-depth-12-face ((,class (:foreground ,red-5))))

   ;; rpm-mode
   `(rpm-spec-dir-face ((,class (:foreground ,green-2))))
   `(rpm-spec-doc-face ((,class (:foreground ,green-2))))
   `(rpm-spec-ghost-face ((,class (:foreground ,red-1))))
   `(rpm-spec-macro-face ((,class (:foreground ,yellow-4))))
   `(rpm-spec-obsolete-tag-face ((,class (:foreground ,red-1))))
   `(rpm-spec-package-face ((,class (:foreground ,red-1))))
   `(rpm-spec-section-face ((,class (:foreground ,yellow-2))))
   `(rpm-spec-tag-face ((,class (:foreground ,blue-2))))
   `(rpm-spec-var-face ((,class (:foreground ,red-1))))

   `(show-paren-match ((,class (:foreground ,blue-4 :background ,brown-4 :weight bold))))

   ;; SLIME
   `(slime-repl-inputed-output-face ((,class (:foreground ,red-1))))

   ;; whitespace-mode
   `(whitespace-space ((,class (:background ,brown-1 :foreground ,brown-4))))
   `(whitespace-hspace ((,class (:background ,brown-1 :foreground ,brown-4))))
   `(whitespace-tab ((,class (:background ,brown-1 :foreground ,red-2))))
   `(whitespace-newline ((,class (:foreground ,brown-4))))
   `(whitespace-trailing ((,class (:foreground ,red-1 :background ,brown-1))))
   `(whitespace-line ((,class (:background ,brown-1 :foreground ,purple-3))))
   `(whitespace-space-before-tab ((,class (:background ,orange-3 :foreground ,orange-3))))
   `(whitespace-indentation ((,class (:background ,brown-1 :foreground ,brown-4))))
   `(whitespace-empty ((,class (:background ,yellow-2 :foreground ,red-2))))
   `(whitespace-space-after-tab ((,class (:background ,brown-1 :foreground ,brown-4))))

   ;; wanderlust
   `(wl-highlight-folder-few-face ((,class (:foreground ,red-1))))
   `(wl-highlight-folder-many-face ((,class (:foreground ,red-1))))
   `(wl-highlight-folder-path-face ((,class (:foreground ,orange-3))))
   `(wl-highlight-folder-unread-face ((,class (:foreground ,blue-2))))
   `(wl-highlight-folder-zero-face ((,class (:foreground ,white-1))))
   `(wl-highlight-folder-unknown-face ((,class (:foreground ,blue-2))))
   `(wl-highlight-message-citation-header ((,class (:foreground ,red-1))))
   `(wl-highlight-message-cited-text-1 ((,class (:foreground ,red-1))))
   `(wl-highlight-message-cited-text-2 ((,class (:foreground ,green-2))))
   `(wl-highlight-message-cited-text-3 ((,class (:foreground ,blue-2))))
   `(wl-highlight-message-cited-text-4 ((,class (:foreground ,blue-2))))
   `(wl-highlight-message-header-contents-face ((,class (:foreground ,green-2))))
   `(wl-highlight-message-headers-face ((,class (:foreground ,red-1))))
   `(wl-highlight-message-important-header-contents ((,class (:foreground ,green-2))))
   `(wl-highlight-message-header-contents ((,class (:foreground ,green-2))))
   `(wl-highlight-message-important-header-contents2 ((,class (:foreground ,green-2))))
   `(wl-highlight-message-signature ((,class (:foreground ,green-2))))
   `(wl-highlight-message-unimportant-header-contents ((,class (:foreground ,white-1))))
   `(wl-highlight-summary-answered-face ((,class (:foreground ,blue-2))))
   `(wl-highlight-summary-disposed-face ((,class (:foreground ,white-1
                                                              :slant italic))))
   `(wl-highlight-summary-new-face ((,class (:foreground ,blue-2))))
   `(wl-highlight-summary-normal-face ((,class (:foreground ,white-1))))
   `(wl-highlight-summary-thread-top-face ((,class (:foreground ,yellow-5))))
   `(wl-highlight-thread-indent-face ((,class (:foreground ,purple-3))))
   `(wl-highlight-summary-refiled-face ((,class (:foreground ,white-1))))
   `(wl-highlight-summary-displaying-face ((,class (:underline t :weight bold))))

   ;; which-func-mode
   `(which-func ((,class (:foreground ,green-2)))))

  ;;
  (custom-theme-set-variables
   'birds-of-paradise

   ;; Fill Column Indicator mode
   `(fci-rule-color ,brown-1) ;; renders much brighter for some reason
   `(fci-rule-character-color ,brown-1)

   ;; Misc.
   `(ansi-color-names-vector
     ;; black, red, green, yellow, blue, magenta, cyan, white
     [,black-1 ,red-2 ,green-1 ,yellow-4 ,blue-3 ,purple-3 ,blue-4 ,white-2])
   `(ansi-term-color-vector
     [unspecified ,black-1 ,red-2 ,green-1 ,yellow-4 ,
                  blue-3 ,purple-3 ,blue-4 ,white-2])))



;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'birds-of-paradise)
