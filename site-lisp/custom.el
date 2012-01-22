(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "unknown" :family "Ubuntu Mono"))))
 '(cursor ((t (:background "black"))))
 '(diff-added ((t (:inherit diff-changed :foreground "#00aa00"))))
 '(diff-changed ((nil (:weight bold))))
 '(diff-indicator-added ((t (:inherit diff-added))))
 '(diff-indicator-removed ((t (:inherit diff-removed))))
 '(diff-removed ((t (:inherit diff-changed :foreground "#ee0000"))))
 '(highlight-current-line-face ((t (:background "cornsilk"))))
 '(highlight-symbol-face ((((class color) (background light)) (:background "gray90" :weight bold))))
 '(hl-line ((t (:background "cornsilk"))))
 '(hl-paren-face ((t (:weight bold))) t)
 '(magit-diff-add ((((class color) (background light)) (:foreground "green3" :weight bold))))
 '(magit-diff-del ((((class color) (background light)) (:foreground "red" :weight bold))))
 '(magit-item-highlight ((t (:inherit highlight :background "honeydew1"))))
 '(magit-log-head-label-wip ((t (:background "Grey95" :foreground "LightSkyBlue3" :box 1))))
 '(mwe:nesting-face-0 ((((class color)) (:background "#90b0f0"))))
 '(mwe:nesting-face-1 ((((class color)) (:background "#b090f0"))))
 '(mwe:nesting-face-2 ((((class color)) (:background "#f0b090"))))
 '(mwe:nesting-face-3 ((((class color)) (:background "#90b0f0"))))
 '(mwe:nesting-face-4 ((((class color)) (:background "#90f0b0"))))
 '(mwe:nesting-face-5 ((((class color)) (:background "#b0f090"))))
 '(mwe:nesting-face-6 ((((class color)) (:background "#b090f0"))))
 '(mwe:nesting-face-7 ((((class color)) (:background "#90b0f0"))))
 '(mwe:nesting-face-8 ((((class color)) (:background "#b0f090"))))
 '(paren-face-match ((((class color)) (:background "lightcyan"))))
 '(show-paren-match ((((class color) (background light)) (:background "lightcyan"))))
 '(yaoddmuse-dialog ((t (:foreground "sienna"))))
 '(yaoddmuse-level-1 ((t (:foreground "Grey60"))))
 '(yaoddmuse-level-2 ((t (:foreground "Grey45"))))
 '(yaoddmuse-level-3 ((t (:foreground "Grey30"))))
 '(yaoddmuse-link ((t (:foreground "goldenrod"))))
 '(yaoddmuse-lisp-file ((t (:foreground "olive drab"))))
 '(yaoddmuse-lisp-keyword ((t (:foreground "DarkGreen"))))
 '(yaoddmuse-source-code ((t (:foreground "Yellow3"))))
 '(yaoddmuse-tag ((t (:foreground "dark Goldenrod"))))
 '(yaoddmuse-url-name ((t (:foreground "dark Orange")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-comphist-file "/home/ryan/.emacs.d/persistence/ac-comphist.dat")
 '(ac-modes (quote (ess-mode emacs-lisp-mode lisp-interaction-mode c-mode cc-mode c++-mode java-mode malabar-mode clojure-mode scala-mode scheme-mode ocaml-mode tuareg-mode haskell-mode perl-mode cperl-mode sepia-mode python-mode ruby-mode ecmascript-mode javascript-mode js-mode js2-mode php-mode css-mode makefile-mode makefile-gmake-mode sh-mode fortran-mode f90-mode ada-mode xml-mode sgml-mode)))
 '(ack-arguments (quote ("-i")))
 '(ack-command "ack-grep")
 '(ack-default-arguments "-i --nocolor")
 '(ack-prompt-for-directory (quote unless-guessed))
 '(ack-root-directory-functions nil)
 '(auto-install-buffer-name "*auto-install*")
 '(auto-install-directory "~/.emacs.d/site-lisp/packages/auto-install/")
 '(auto-save-file-name-transforms (quote ((".*" "/home/ryan/.emacs.d/persistence/auto-save-files" t) ("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" "/tmp/\\2" t))))
 '(auto-save-list-file-prefix "~/.emacs.d/persistence/auto-save-list/.saves-")
 '(auto-save-timeout 30)
 '(autopair-global-mode t)
 '(ba/org-adjust-tags-column t)
 '(backup-by-copying t)
 '(backup-directory-alist (quote ((".*" . "/home/ryan/.emacs.d/persistence/backup-files"))))
 '(backup-file-exclusion-regexps (quote ("^/home/ryan/.emacs.d/persistence/")))
 '(backward-delete-char-untabify-method (quote untabify))
 '(bar-cursor-mode t nil (bar-cursor))
 '(blink-cursor-mode t)
 '(bookmark-default-file "~/.emacs.d/persistence/bookmark")
 '(browse-url-browser-function (quote browse-url-firefox))
 '(browse-url-firefox-new-window-is-tab t)
 '(browse-url-firefox-program "firefox")
 '(browse-url-generic-program "xdg-open")
 '(c-default-style (quote ((java-mode . "java") (awk-mode . "awk") (other . "bsd"))))
 '(clean-buffer-list-delay-general 1)
 '(color-theme-is-cumulative nil)
 '(comint-prompt-read-only t)
 '(comint-scroll-to-bottom-on-input (quote this))
 '(command-remapping-alist (quote ((global-map (list-buffers . ibuffer) (apropos-command . apropos)))))
 '(comment-column 0)
 '(company-dabbrev-code-modes (quote (asm-mode batch-file-mode c++-mode c-mode cperl-mode csharp-mode css-mode emacs-lisp-mode erlang-mode espresso-mode f90-mode fortran-mode haskell-mode java-mode javascript-mode jde-mode js2-mode lisp-mode lua-mode objc-mode sepia-mode cperl-mode perl-mode php-mode python-mode ruby-mode scheme-mode shell-script-mode)))
 '(company-show-numbers t)
 '(cperl-break-one-line-blocks-when-indent nil)
 '(cperl-close-paren-offset -4)
 '(cperl-continued-brace-offset -4)
 '(cperl-continued-statement-offset 4)
 '(cperl-electric-keywords nil)
 '(cperl-electric-linefeed nil)
 '(cperl-extra-newline-before-brace nil)
 '(cperl-extra-newline-before-brace-multiline nil)
 '(cperl-font-lock t)
 '(cperl-hairy nil)
 '(cperl-highlight-variables-indiscriminately t)
 '(cperl-indent-level 4)
 '(cperl-indent-parens-as-block t)
 '(cperl-indent-region-fix-constructs 1)
 '(cperl-indent-subs-specially nil)
 '(cperl-invalid-face (quote default))
 '(cperl-lazy-help-time 0)
 '(cperl-merge-trailing-else nil)
 '(cperl-under-as-char t)
 '(csv-align-style (quote auto))
 '(csv-comment-start-default "")
 '(csv-header-lines 0)
 '(csv-separators (quote ("	")))
 '(cua-auto-mark-last-change t)
 '(cua-enable-cua-keys nil)
 '(cua-enable-cursor-indications t)
 '(cua-enable-modeline-indications t)
 '(cua-enable-region-auto-help t)
 '(cua-global-mark-cursor-color "cyan3")
 '(cua-mode nil nil (cua-base))
 '(cua-normal-cursor-color "black")
 '(cua-overwrite-cursor-color "yellow3")
 '(cua-read-only-cursor-color "green3")
 '(cua-rectangle-mark-key [ignore])
 '(curchg-default-cursor-color "black")
 '(custom-raised-buttons t)
 '(debug-on-error nil)
 '(default-frame-alist (quote ((menu-bar-mode . 0) (tool-bar-mode . 0))))
 '(delete-by-moving-to-trash t)
 '(delete-old-versions t)
 '(delete-selection-mode t)
 '(desktop-file-name-format (quote tilde))
 '(desktop-files-not-to-save "^/[^/:]*:")
 '(desktop-globals-to-clear (quote (kill-ring kill-ring-yank-pointer search-ring search-ring-yank-pointer regexp-search-ring regexp-search-ring-yank-pointer)))
 '(desktop-globals-to-save (quote ((extended-command-history . 30) (file-name-history . 100) (grep-history . 30) (compile-history . 30) (minibuffer-history . 50) (query-replace-history . 60) (read-expression-history . 60) (regexp-history . 60) (regexp-search-ring . 20) (search-ring . 20))))
 '(desktop-load-locked-desktop nil)
 '(desktop-not-loaded-hook (quote (desktop-save-mode-off)))
 '(desktop-path (quote ("~/.emacs.d/persistence")))
 '(desktop-restore-eager t)
 '(desktop-save t)
 '(desktop-save-mode nil)
 '(develock-auto-enable nil)
 '(dict-server "localhost")
 '(diff-switches "-u -w")
 '(diminished-minor-modes (quote ((volatile-highlights-mode . "") (eldoc-mode . "") (filladapt-mode . "") (highlight-parentheses-mode . "") (autopair-mode . "") (auto-complete-mode . "") (cua-mode . ""))))
 '(dired-listing-switches "-alh")
 '(dired-mode-hook (quote ((lambda nil (if (and (boundp (quote font-lock-maximum-decoration)) font-lock-maximum-decoration) (set (make-local-variable (quote font-lock-defaults)) (quote (diredp-font-lock-keywords-1 t))))) dired-extra-startup (lambda nil (dired-omit-mode 1)))))
 '(dired-no-confirm nil)
 '(dired-omit-files "^\\.?#")
 '(dired-omit-size-limit 30000)
 '(dired-recursive-deletes (quote top))
 '(dta-cfg-dir "/home/ryan/.emacs.d/persistence/desktopaid/")
 '(dta-default-cfg "default.conf")
 '(dta-max-history-length 100)
 '(el-get-notify-type (quote both))
 '(eldoc-idle-delay 0.25)
 '(elscreen-buffer-list-enabled t)
 '(elscreen-tab-display-kill-screen (quote right))
 '(emms-completing-read-function (quote completing-read))
 '(enable-recursive-minibuffers t)
 '(escreen-goto-screen-hook (quote (escreen-enable-number-mode-if-more-than-one-screen)))
 '(escreen-prefix-char "")
 '(ess-fancy-comments nil)
 '(ffap-require-prefix t)
 '(filladapt-turn-on-mode-hooks (quote (lisp-mode-hook emacs-lisp-mode-hook perl-mode-hook)))
 '(flymake-gui-warnings-enabled nil)
 '(focus-follows-mouse nil)
 '(frame-background-mode nil)
 '(fringe-mode nil nil (fringe))
 '(global-company-mode t)
 '(global-hl-line-mode nil)
 '(global-linum-mode nil)
 '(global-pabbrev-mode t)
 '(global-rainbow-delimiters-mode t)
 '(global-remapping-alist (quote ((list-buffers . electric-buffer-list) (apropos-command . apropos) (move-beginning-of-line . back-to-indentation))))
 '(global-smart-tab-mode nil)
 '(global-undo-tree-mode t)
 '(global-visual-line-mode nil)
 '(gnus-init-file "~/.emacs.d/gnus")
 '(gnus-secondary-select-methods (quote ((nnimap "gmail" (nnimap-address "imap.gmail.com") (nnimap-server-port 993) (nnimap-stream ssl)))))
 '(gnus-select-method (quote (nntp "news://news.gmane.org/gmane.emacs.orgmode")))
 '(highlight-current-line-globally t nil (highlight-current-line))
 '(highlight-current-line-ignore-regexp "Faces\\|Colors\\| \\*Mini\\|^\\*magit")
 '(highlight-current-line-whole-line t)
 '(highlight-symbol-idle-delay 0.5)
 '(history-length t)
 '(hl-paren-colors (quote ("DeepSkyBlue" "dodgerblue" "medium blue" "navy" "midnight blue" "black")))
 '(ibuffer-movement-cycle nil)
 '(ibuffer-old-time 4)
 '(ibuffer-saved-filter-groups nil)
 '(icicle-file-sort (quote icicle-last-modified-first-p))
 '(icicle-show-Completions-initially-flag t)
 '(icomplete-mode nil)
 '(ido-confirm-unique-completion t)
 '(ido-enable-dot-prefix t)
 '(ido-enable-flex-matching t)
 '(ido-enabled (quote both) t)
 '(ido-enter-matching-directory (quote first))
 '(ido-everywhere t)
 '(ido-ignore-files (quote ("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "\\.emacs\\.(desktop|bmk)" "auto-save-list" "\\.rsync-filter" "\\.org-id-locations")))
 '(ido-mode (quote both) nil (ido))
 '(ido-save-directory-list-file "~/.emacs.d/persistence/ido.last")
 '(ido-ubiquitous t)
 '(ido-ubiquitous-function-exceptions (quote (grep-read-files read-file-name)))
 '(ido-ubiquitous-mode t)
 '(ido-use-filename-at-point (quote guess))
 '(ido-use-virtual-buffers t)
 '(ido-yes-or-no-mode t)
 '(indent-tabs-mode nil)
 '(indicate-buffer-boundaries (quote left))
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice nil)
 '(ipython-command "/home/ryan/.pythonbrew/pythons/Python-2.7.1-multiarch/bin/ipython")
 '(isearch-allow-scroll t)
 '(isearch-resume-in-command-history t)
 '(js2-basic-offset 4)
 '(js2-highlight-level 3)
 '(js2-mirror-mode nil)
 '(kept-new-versions 6)
 '(kill-read-only-ok t)
 '(kill-whole-line t)
 '(linum-delay t)
 '(list-directory-verbose-switches "-lh")
 '(longlines-auto-wrap nil)
 '(longlines-wrap-follows-window-size t)
 '(magit-completing-read-function (quote magit-ido-completing-read))
 '(magit-create-branch-behaviour (quote at-point))
 '(magit-highlight-indentation nil)
 '(magit-log-auto-more t)
 '(magit-repo-dirs (quote ("~/Projects/public" "~/Projects/private")))
 '(magit-save-some-buffers nil)
 '(magit-set-upstream-on-push t)
 '(magit-status-buffer-switch-function (quote switch-to-buffer))
 '(magit-wip-mode t)
 '(mail-host-address "aeolus")
 '(markdown-command "Markdown.pl")
 '(marmalade-server "http://marmalade-repo.org")
 '(marmalade-token "jxzMp9uu6+16QPYKyarL9glfVfekWdWta8EEdMSkKlE=")
 '(marmalade-username "DarwinAwardWinner")
 '(matlab-auto-fill nil)
 '(matlab-fill-code t)
 '(menu-bar-mode nil)
 '(message-log-max 10000)
 '(mew-config-alist (quote (("UVA Mail" ("name" . "Ryan C. Thompson") ("user" . "ryan") ("mail-domain" . "virginia.edu") ("proto" . "%") ("imap-user" . "rct2c@virginia.edu") ("imap-server" . "imap.gmail.com") ("imap-ssl" . t) ("imap-ssl-port" . "993") ("imap-delete") \.\.\.) ("Gmail" ("name" . "Ryan C. Thompson") ("user" . "ryan") ("mail-domain" . "gmail.com") ("proto" . "%") ("imap-user" . "darwinawdwinner") ("imap-server" . "imap.gmail.com") ("imap-ssl" . t) ("imap-ssl-port" . "993") ("imap-delete") \.\.\.))))
 '(midnight-delay "4:00am")
 '(midnight-mode t nil (midnight))
 '(minibuffer-depth-indicate-mode t)
 '(mode-compile-modes-alist (quote ((c-mode cc-compile kill-compilation) (java-mode java-compile kill-compilation) (c++-mode c++-compile kill-compilation) (ada-mode ada-compile kill-compilation) (fortran-mode f77-compile kill-compilation) (dired-mode dired-compile kill-compilation) (emacs-lisp-mode elisp-compile keyboard-quit) (lisp-interaction-mode elisp-compile keyboard-quit) (makefile-mode makefile-compile kill-compilation) (sh-mode sh-compile kill-compilation) (csh-mode csh-compile kill-compilation) (zsh-mode zsh-compile kill-compilation) (perl-mode perl-compile kill-compilation) (cperl-mode perl-compile kill-compilation) (sepia-mode perl-compile kill-compilation) (tcl-mode tcl-compile kill-compilation) (python-mode python-compile kill-compilation) (ruby-mode ruby-compile kill-compilation) (fundamental-mode guess-compile nil) (text-mode guess-compile nil) (indented-text-mode guess-compile nil) (compilation-mode default-compile kill-compilation))))
 '(mouse-wheel-progressive-speed nil)
 '(mouse-yank-at-point nil)
 '(muse-project-alist (quote (("WikiPlanner" ("~/Plans" :default "TaskPool" :major-mode planner-mode :visit-link planner-visit-link) (:base "planner-xhtml" :path "~/Public/Plans")))))
 '(network-manager-disconnect-hook (quote (tramp-cleanup-all-connections)))
 '(octave-auto-indent t)
 '(octave-auto-newline t)
 '(octave-block-offset 4)
 '(octave-continuation-string "...")
 '(octave-mode-hook (quote ((lambda nil (setq octave-comment-start "% " octave-comment-char 37)))))
 '(org-agenda-compact-blocks nil)
 '(org-agenda-dim-blocked-tasks nil)
 '(org-agenda-files (quote ("~/org/shopping-list.org" "~/org/anniv.org" "~/org/classes.org" "~/org/tasks.org" "~/org/main.org")))
 '(org-agenda-restore-windows-after-quit t)
 '(org-agenda-skip-additional-timestamps-same-entry nil)
 '(org-agenda-skip-deadline-prewarning-if-scheduled 7)
 '(org-agenda-skip-scheduled-if-deadline-is-shown (quote not-today))
 '(org-agenda-start-on-weekday 0)
 '(org-agenda-time-grid (quote ((daily today require-timed remove-match) "----------------" (800 1000 1200 1400 1600 1800 2000))))
 '(org-agenda-todo-list-sublevels nil)
 '(org-agenda-window-setup (quote reorganize-frame))
 '(org-archive-location "~/org/archive.org::* From %s")
 '(org-archive-save-context-info (quote (time file category todo priority itags olpath ltags)))
 '(org-auto-align-tags t)
 '(org-completion-use-ido t)
 '(org-deadline-warning-days 7)
 '(org-default-notes-file "main.org")
 '(org-directory "~/org")
 '(org-drawers (quote ("PROPERTIES" "CLOCK" "LOGBOOK" "DESCRIPTION")))
 '(org-enforce-todo-checkbox-dependencies t)
 '(org-enforce-todo-dependencies nil)
 '(org-export-html-inline-images t)
 '(org-export-with-archived-trees nil)
 '(org-file-apps (quote ((auto-mode . emacs) ("\\.x?html?\\'" . system) ("\\.pdf\\'" . system) (system . "xdg-open"))))
 '(org-hide-leading-stars t)
 '(org-id-locations-file "~/.emacs.d/persistence/.org-id-locations")
 '(org-link-frame-setup (quote ((vm . vm-visit-folder-other-frame) (gnus . gnus-other-frame) (file . find-file))))
 '(org-log-into-drawer t)
 '(org-modules (quote (org-info)))
 '(org-mouse-features (quote (context-menu move-tree yank-link activate-stars activate-bullets activate-checkboxes)))
 '(org-odd-levels-only t)
 '(org-outline-path-complete-in-steps nil)
 '(org-priority-faces (quote ((65 :foreground "red" :weight bold) (66 :foreground "green4" :weight bold) (67 :foreground "blue" :weight bold))))
 '(org-refile-targets (quote ((org-agenda-files :maxlevel . 10))))
 '(org-refile-use-outline-path t)
 '(org-remember-default-headline "Notes")
 '(org-remember-templates (quote (("Note" 110 "* %^{Description}
  %U
  %?" nil nil nil) ("Clipboard note" 99 "* %^{Description}
  %U
  %x
  %?" nil nil nil) ("TODO" 116 "* TODO %^{Description}
  SCHEDULED: %^{Scheduled }T
  %U
  %?
" "tasks.org" "Unsorted Tasks" nil) ("TODO unscheduled" 84 "* TODO %^{Description}
  %U
  %?" "tasks.org" "Unsorted Tasks" nil) ("Event" 101 "* %^{Description}
  %U
  %^{When is the event?}T
  %?" nil "Events" nil) ("Shopping List" 115 "* TODO %^{Item Name}
  %U
  %?" "shopping-list.org" "Shopping List" nil) ("Weight Log" 119 "* %^{Scale Reading (lbs)} pounds, on %u
  %?%!" "excercise.org" "Weight Log" nil))))
 '(org-return-follows-link t)
 '(org-reverse-note-order t)
 '(org-support-shift-select t)
 '(org-tags-column-auto-adjust t)
 '(org-todo-keyword-colors (quote (("IN PROGRESS" . "dark orange") ("WAITING" . "red4") ("CANCELED" . "saddle brown"))))
 '(org-todo-keyword-faces (quote (("IN PROGRESS" . "dark orange") ("BLOCKED" . "red3") ("WAITING" . "red4") ("CANCELED" . "saddle brown"))))
 '(org-todo-keywords (quote ((sequence "TODO(t!)" "IN PROGRESS(p!)" "BLOCKED(b!)" "WAITING(w@/!)" "TURN IN(i!)" "|" "DONE(d!)" "CANCELED(c@)"))))
 '(org-todo-keywords-sort-order (quote ("IN PROGRESS")))
 '(org-treat-S-cursor-todo-selection-as-state-change nil)
 '(org-use-fast-todo-selection t)
 '(org-yank-adjusted-subtrees t)
 '(pabbrev-scavenge-some-chunk-size 100)
 '(package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/") ("elpa" . "http://tromey.com/elpa/") ("marmalade" . "http://marmalade-repo.org/packages/"))))
 '(paren-delay 0.2)
 '(paren-dont-load-timer nil)
 '(paren-highlight-offscreen t)
 '(paren-match-face (quote show-paren-match))
 '(paren-mismatch-face (quote show-paren-mismatch))
 '(paren-sexp-mode t)
 '(partial-completion-mode t)
 '(pc-select-override-scroll-error t)
 '(pc-select-remapping-alist (quote ((beginning-of-line . back-to-indentation))))
 '(pc-select-selection-keys-only t)
 '(pc-selection-mode nil)
 '(perl-test-file-boilerplate "#!perl
use Test::More;



done_testing();")
 '(printer-name "PDF")
 '(protect-buffer-names (quote ("*scratch*" "*Messages*" "*Completions*" "*Help*" "*Apropos*" "*Perldoc*" "*info*" "main.org")))
 '(ps-printer-name "Photosmart-C3100-series")
 '(py-electric-colon-active-p t)
 '(py-electric-colon-dedent-only t)
 '(py-python-command "python" t)
 '(python-python-command "/home/ryan/.pythonbrew/pythons/Python-2.7.1-multiarch/bin/python")
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(recentf-auto-cleanup 300)
 '(recentf-exclude (quote ("^/home/ryan/.emacs.d/persistence/")))
 '(recentf-max-menu-items 30)
 '(recentf-max-saved-items 50)
 '(recentf-mode t)
 '(recentf-save-file "~/.emacs.d/persistence/recentf")
 '(remember-frame-alist (quote ((width . 80) (height . 15))))
 '(remember-in-new-frame nil)
 '(remember-mode-hook (quote (org-remember-apply-template delete-other-windows)))
 '(require-final-newline (quote visit-save))
 '(ropemacs-autoimport-underlineds t)
 '(ropemacs-completing-read-function (quote completing-read))
 '(ropemacs-enable-shortcuts nil)
 '(ropemacs-guess-project t)
 '(safe-local-variable-values (quote ((py-indent-offset . 8) (major-mode . markdown) (eval set-face-attribute (quote whitespace-line) nil :background "red1" :foreground "yellow" :weight (quote bold)) (eval set-face-attribute (quote whitespace-tab) nil :background "red1" :foreground "yellow" :weight (quote bold)) (whitespace-style face trailing lines-tail) (whitespace-line-column . 80) (eval require (quote whitespace)) (org-time-stamp-custom-formats "<%Y-%m-%d %a>" . "<%I:%M %p>") (org-time-stamp-custom-formats "<%m/%d/%y %a>" . "<%m/%d/%y %a %H:%M>") (org-time-stamp-custom-formats quote ("<%m/%d/%y %a>" . "<%m/%d/%y %a %H:%M>")) (folded-file . t) (TeX-master . cv\.ltx) (TeX-master . cv\.tex) (TeX-master . t))))
 '(save-abbrevs (quote silently))
 '(save-interprogram-paste-before-kill t)
 '(save-place t nil (saveplace))
 '(save-place-file "~/.emacs.d/persistence/saveplace")
 '(save-visited-files-auto-restore t)
 '(save-visited-files-kill-temp-buffer t)
 '(save-visited-files-mode t)
 '(savehist-additional-variables (quote (compile-command)))
 '(savehist-autosave-interval nil)
 '(savehist-file "/home/ryan/.emacs.d/persistence/history")
 '(savehist-ignored-variables (quote (ido-cur-list)))
 '(savehist-mode t nil (savehist))
 '(scroll-bar-mode nil)
 '(scroll-preserve-screen-position t)
 '(send-mail-function (quote mailclient-send-it))
 '(sentence-end-double-space nil)
 '(server-mode nil)
 '(server-visit-hook (quote (save-place-find-file-hook)))
 '(sh-basic-offset 2)
 '(sh-indent-comment t)
 '(sh-learn-basic-offset (quote usually))
 '(shift-select-mode t)
 '(show-paren-delay 0.125)
 '(show-paren-mode t)
 '(show-paren-style (quote expression))
 '(smart-tab-using-hippie-expand t)
 '(smex-auto-update nil)
 '(smex-save-file "~/.emacs.d/persistence/smex.save")
 '(sml-modeline-borders nil)
 '(sml-modeline-mode t)
 '(sml-modeline-numbers (quote line-numbers))
 '(system-trash-exclude-matches (quote ("^#.+#$" ".*~$" "^session\\.[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]$" "^\\.emacs\\.desktop.*")))
 '(system-trash-exclude-names nil)
 '(system-trash-exclude-paths (quote ("/tmp")))
 '(tabkey2-mode nil)
 '(tabkey2-modes-that-just-complete (quote (shell-mode)))
 '(tempbuf-temporary-major-modes (quote (Custom-mode w3-mode Man-mode view-mode help-mode apropos-mode completion-list-mode dired-mode ibuffer-mode bookmark-bmenu-mode ess-help-mode compilation-mode vc-dir-mode vc-hg-log-view-mode occur-mode diff-mode magit-key-mode magit-log-edit-mode ess-help-mode)))
 '(tool-bar-mode nil)
 '(tramp-copy-size-limit 0)
 '(tramp-default-method "ssh")
 '(tramp-default-proxies-alist (quote (((regexp-quote (system-name)) nil nil) ("localhost" "nil" "nil") (nil "\\`root\\'" "/ssh:%h:"))))
 '(tramp-gvfs-methods (quote ("dav" "davs" "obex" "synce")))
 '(tramp-mode t)
 '(tramp-persistency-file-name "/home/ryan/.emacs.d/persistence/tramp")
 '(transient-mark-mode t)
 '(trash-only-inside-home t)
 '(truncate-lines nil)
 '(undo-outer-limit 24000000)
 '(undo-tree-mode-lighter "")
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(uniquify-trailing-separator-p t)
 '(user-mail-address "rct@thompsonclan.org")
 '(version-control t)
 '(vhl/use-kill-region-extension-p t)
 '(visible-bell nil)
 '(visual-line-fringe-indicators (quote (left-curly-arrow right-curly-arrow)))
 '(volatile-highlights-mode t)
 '(w3m-use-cookies t)
 '(warning-suppress-types nil)
 '(wdired-enable t)
 '(wdired-use-dired-vertical-movement (quote sometimes))
 '(wdired-use-interactive-rename t)
 '(wget-download-directory nil)
 '(wget-download-directory-filter nil)
 '(whole-line-or-region-mode nil)
 '(woman-use-own-frame t)
 '(x-select-enable-clipboard t)
 '(yaoddmuse-use-always-minor t)
 '(yaoddmuse-username "RyanThompson"))
(put 'scroll-left 'disabled nil)
(put 'upcase-region 'disabled nil)
