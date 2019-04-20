;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; .emacs                                                                     ;;
;; Author: Jon Atack                                                          ;;
;; Licence: MIT                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file configures Emacs 26 with an emphasis on Common Lisp and Ruby.
;;; Feel free to customize it.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section I: Generic settings                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq user-full-name "Jon Atack" user-mail-address "jon@atack.com")

;; Turn off GC during Emacs startup
(setq gc-cons-threshold most-positive-fixnum)
;; Resume GC after startup with 50 MB threshold, sacrificing memory for speed
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 50 1000 1000))))

;; Display Emacs startup stats
(add-hook 'emacs-startup-hook
          (lambda () (message "Emacs ready in %s with %d garbage collections."
                         (format (emacs-init-time)) gcs-done)))

;; Resize the window to my screen
(if (display-graphic-p)
    (progn
      (setq initial-frame-alist
            '(
              ; (tool-bar-lines . 0)
              (width . 105) ; chars
              (height . 88) ; lines
              ; (background-color . "honeydew")
              (left . 0)
              (top . 0)))
      (setq default-frame-alist
            '(
              ; (tool-bar-lines . 0)
              (width . 105)
              (height . 88)
              ; (background-color . "honeydew")
              (left . 0)
              (top . 0))))
  (progn
    (setq initial-frame-alist '( (tool-bar-lines . 0)))
    (setq default-frame-alist '( (tool-bar-lines . 0)))))

;; Typography
;; (set-face-attribute 'default nil
;;                    :family "Source Code Pro"
;;                    :height 150
;;                    :weight 'normal
;;                    :width 'normal)

;; Set Emacs display to be transparent
;; (set-frame-parameter (selected-frame) 'alpha '(<active> [<inactive>]))
;; (set-frame-parameter (selected-frame) 'alpha '(88 70))
;; (add-to-list 'default-frame-alist '(alpha 88 70))

;; Disable the menu bar, the tool bar and the scroll bar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(global-display-line-numbers-mode) ; Globally display line numbers

(column-number-mode 1)
(visual-line-mode 1) ; Better word wrapping

;; Auto-insert newline at end of file
(setq-default require-final-newline t mode-require-final-newline t )

;; Display filepath in window title bar
(setq-default frame-title-format '((:eval (if (buffer-file-name)
                                              (abbreviate-file-name (buffer-file-name)) "%f"))))

(setq resize-mini-windows nil) ; Do not resize the mini-buffer to keep it to one line.

(setq inhibit-startup-screen t) ; Don’t display the Emacs splash screen
(setq initial-scratch-message nil) ; Don't show scratch buffer on startup

;; Highlight matching parentheses globally.
(run-with-idle-timer 1 nil (lambda () (show-paren-mode t)))

;; Display “lambda” as “λ”
(run-with-idle-timer 1 nil (lambda () (global-prettify-symbols-mode t)))

;; (setq case-fold-search t) ; Make searches case-insensitive
(setq browse-url-browser-function 'eww-browse-url)
;; (set-face-attribute 'default nil :height 90) ; Make the default font slightly smaller.
(setq-default indent-tabs-mode nil) ; Make indentation use spaces.

;; Save desktop between sessions
;;
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Emacs-Sessions.html
;;
;; M-x desktop-save -> saves the desktop manually
;;
(setq desktop-path '("~/.emacs.d/")) ; Load/save desktop in .emacs.d directory
;; (setq desktop-path '("."))        ; Load/save desktop in dir Emacs opened from
;;
;; (desktop-save-mode t) ; Auto-save desktop on exiting, and reload on startup

;; (setq default-directory "/")

;; Use 2 spaces for indentation everywhere
(setq-default tab-width 2)
(setq-default c-basic-offset 2)
(setq css-indent-offset 2)
(setq js-indent-level 2)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-css-indent-offset 2)

(setq-default fill-column 80) ; Set max character line length
(setq large-file-warning-threshold nil) ; Don’t warn me about opening large files
(setq x-select-enable-clipboard t) ; Enable copy/past-ing from clipboard
(setq system-uses-terminfo nil) ; Fix weird color escape sequences
(setq confirm-kill-emacs 'yes-or-no-p) ; Ask for confirmation before closing emacs

(fset 'yes-or-no-p 'y-or-n-p) ; Enable answering with y/n instead of yes/no

;; Always reload the file if it changed on disk
(run-with-idle-timer 2 nil (lambda () (global-auto-revert-mode t)))

;; (setq-default line-spacing t) ; A nice line height
;; (setq mac-command-modifier 'meta) ; Treat the CMD key like meta on OSX
;; (setq ns-use-srgb-colorspace t) ; SRGB support for OSX

;; Open splits horizontally
(setq split-height-threshold 0)
(setq split-width-threshold nil)

(set-fringe-mode '(5 . 4)) ; Show a fringe

;; Remove the fringe indicators
(when (boundp 'fringe-indicator-alist)
  (setq-default fringe-indicator-alist
		'(
		  (continuation . nil)
		  (overlay-arrow . nil)
		  (up . nil)
		  (down . nil)
		  (top . nil)
		  (bottom . nil)
		  (top-bottom . nil)
		  (empty-line . nil)
		  (unknown . nil))))

;; A simple backup setup to ensure sure I don't foo~ and #.foo files in
;; directories with edited files.
(setq
  backup-by-copying t                    ; don't clobber symlinks-
  backup-directory-alist
  '(("." . "~/.saves"))                  ; don't litter my fs tree
  auto-save-file-name-transforms
  `((".*" ,temporary-file-directory t))
  delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)                     ; use versioned backups

;; Store all backup and autosave files in the tmp dir
;;(setq backup-directory-alist
;;      '((".*" . ,temporary-file-directory)))
;;(setq auto-save-file-name-transforms
;;      '((".*" ,temporary-file-directory t)))

;; Making dabbrev a bit nicer
(setq dabbrev-abbrev-skip-leading-regexp ":")
(setq dabbrev-backward-only t)

;; Mute beep sound and display message instead
(setq visible-bell nil)
(setq ring-bell-function (lambda () (message "*beep*")))

(put 'upcase-region 'disabled nil) ; enable using upcase-region without warnings

;; UTF-8
(set-language-environment "UTF-8")
(setenv "LANG" "en_US.UTF-8")
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section II: Packages                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; No GC during this memory-heavy init phase
(let ((gc-cons-threshold most-positive-fixnum))

  (package-initialize)
  (setq use-package-always-ensure t)
  (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")))
  (unless (assoc-default "melpa" package-archives)
    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
  (unless (assoc-default "marmalade" package-archives)
    (add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t))
  (unless (assoc-default "org" package-archives)
    (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t))
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

  ;; Fetch the list of packages available if needed
  (unless package-archive-contents (package-refresh-contents))

  ;; Install any missing packages
  ;;
  (dolist (package package-selected-packages)
    (unless (package-installed-p package)
      (package-refresh-contents)
      (package-install package)))

  ;; Ensure Use Package library is installed and configured
  ;; Repository: https://github.com/jwiegley/use-package
  ;;
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  ;; (setq use-package-verbose t)
  (setq use-package-always-ensure t)
  (require 'use-package)
  (use-package auto-compile :config (auto-compile-on-load-mode))
  (setq load-prefer-newer t)

  (use-package better-defaults)

  ;; Saveplace saves the cursor position between sessions
  (use-package saveplace
    :defer t
    :ensure t
    :init
    (setq save-place-file (expand-file-name ".places" user-emacs-directory)
          save-place t))

  (use-package flx-ido
    :defer t
    :ensure t
    :init
    (ido-mode t)
    (ido-everywhere t)
    (flx-ido-mode t)
    (setq ido-enable-flex-matching t
          ;; Disable ido faces to see flx highlights:
          ;; (setq ido-use-faces nil)
          ;; If you don't want to use the flx's highlights you can turn them off like this:
          flx-ido-use-faces nil))

  ;; Projectile key bindings:
  ;;
  ;; Grep             ->  C-c p s g
  ;; Grep all project ->  M-- C-c p s g
  (use-package projectile
    ;; :defer t
    :ensure t
    :config
    (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
    (setq projectile-project-search-path '("~/projects/" "~/common-lisp/"))
          ;; projectile-keymap-prefix (kbd "C-c C-p")
          ;; projectile-completion-system 'grizzl)
    (projectile-mode +1))
    ;; (projectile-global-mode))
    ;; (add-hook 'enh-ruby-mode-hook 'projectile-mode))

  ;; (add-hook 'clojure-mode-hook #'smartparens-strict-mode)

  ;; (setq ns-pop-up-frames nil)

  ;; SLIME
  ;; (add-to-list 'load-path "~/quicklisp/dists/quicklisp/software/slime-v2.23")
  ;; (load (expand-file-name "~/quicklisp/slime-helper.el"))
  (require 'slime-autoloads)

  (slime-setup '(slime-fancy))
  (setq slime-contribs '(slime-fancy slime-repl slime-asdf
      slime-indentation slime-cl-indent slime-banner slime-tramp
      slime-mdot-fu slime-scratch slime-company
      slime-editing-commands slime-quicklisp
      slime-repl-ansi-color))
  ;; (setq slime-contribs '(slime-repl)) ; repl only

  (setq slime-startup-animation nil)

  ;; (setq inferior-lisp-program "/usr/bin/sbcl --noinform")
  ;; (setq inferior-lisp-program "/usr/local/bin/ccl -K utf-8")
  ;; (setq inferior-lisp-program "ecl")

  ;; Load Swank faster, as per:
  ;; https://common-lisp.net/project/slime/doc/html/Loading-Swank-faster.html
  ;;
  ;; For SBCL, we recommend that you create a custom core file with socket
  ;; support and POSIX bindings included because those modules take the most
  ;; time to load. To create such a core, execute the following steps:
  ;;
  ;; $ sbcl
  ;; * (mapc 'require '(sb-bsd-sockets sb-posix sb-introspect sb-cltl2 asdf))
  ;; * (save-lisp-and-die "sbcl.core-for-slime")
  ;;
  ;; After that, add something like this to your .emacs:
  ;;
  ;; (setq slime-lisp-implementations '((sbcl ("sbcl" "--core" "sbcl.core-for-slime"))))
  ;;
  ;; For maximum startup speed you can include the Swank server directly in a core file.
  ;; This setup is a bit more involved and you need to create a new core file after
  ;; updating SLIME or SBCL. The steps to execute are:
  ;;
  ;; $ cd ~/quicklisp/dists/quicklisp/software
  ;;  (or ~/quicklisp/local-projects or whichever directory contains Slime)
  ;;
  ;; $ sbcl
  ;; * (load "slime/swank-loader.lisp")
  ;; * (swank-loader:dump-image "sbcl.core-with-swank")
  ;;
  ;; Then add this to your .emacs:
  ;;
  ;; (setq slime-lisp-implementations
  ;;       '((sbcl ("sbcl" "--core" "path-to.../sbcl.core-with-swank")
  ;;               :init (lambda (port-file _)
  ;;                       (format "(swank:start-server %S)\n" port-file)))))
  (setq slime-lisp-implementations
        '((sbcl ("sbcl" "--core" "/home/jon/quicklisp/local-projects/sbcl.core-with-swank")
                :init (lambda (port-file _)
                        (format "(swank:start-server %S)\n" port-file)))))

  (setf slime-default-lisp 'sbcl)
  (setq slime-net-coding-system 'utf-8-unix)

  (eval-after-load 'slime
    `(define-key slime-prefix-map (kbd "M-h") 'slime-documentation-lookup))

  ;; You can install packages by typing M-x package-install <package-name>. I
  ;; recomend you install the following packages: smex, which adds an improved
  ;; version of M-x. I highly recomend this. You can read more about smex at:
  ;; https://github.com/nonsequitur/smex/

  ;; Another often used mode is magit, which is an interface to git, allowing
  ;; you to manage your repos through emacs. You can read more about it at:
  ;; http://magit.vc/ It is one of the most useful modes available for emacs


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Section III: Global Key Bindings                                           ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (global-set-key (kbd "<f12>") #'slime-selector)
  (global-set-key (kbd "C-c s") #'slime-selector)
  (global-set-key (kbd "C-c h") #'clhs-lookup)
  (global-set-key (kbd "C-c r") #'slime-pop-find-definition-stack)

  ;; Set the enter key to newline-and-indent which inserts a new line and then
  ;; indents according to the major mode. This is very convenient.
  ;; (global-set-key (kbd "RET") 'newline-and-indent)
  ;; (define-key global-map (kbd "RET") 'newline-and-indent)

  ;; By default C-x o is bound to 'other window, but I find I use it much more
  ;; ofther than open-line, which is bound to C-o, so I swap their definitions
  (global-set-key (kbd "C-o") 'other-window)
  (global-set-key (kbd "C-x o") 'open-line)

  ;; Make "C-t" act like "C-x", so it's easier to type on Dvorak layout
  (keyboard-translate ?\C-t ?\C-x)
  ;; (keyboard-translate ?\C-x ?\C-t) ; inversely, make "C-x" act like "C-t"

  ;; Make function-8 act like "C-x" as a shortcut to avoid left pinky stress
  (global-set-key (kbd "<f8>") ctl-x-map)

  ;; Toggle S-expression comments on/off with auto-formatting
  (global-set-key (kbd "C-M-;") #'comment-or-uncomment-sexp)

  ;; Control-i to open a Ruby/Rails console in a new buffer
  ;; (global-set-key (kbd "C-i") 'inf-ruby-console-rails)

  ;; M-0..3 are bound to 'digit-argument. To be used with C-u. I don't use them
  ;; often, so I prefer to rebind them to the window commands, since M-1 is
  ;; easier to type than C-x 1.
  ;; (global-set-key (kbd "M-1") 'delete-other-windows)
  ;; (global-set-key (kbd "M-2") 'split-window-vertically)
  ;; (global-set-key (kbd "M-3") 'split-window-horizontally)
  ;; (global-set-key (kbd "M-0") 'delete-window)

  ;; If you have installed smex, you can uncomment the following lines. To
  ;; activate it without restarting Emacs select the lines and type
  ;; M-x eval-region:
  ;; (require 'smex)
  ;; (global-set-key (kbd "M-x") 'smex)
  ;; (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

  ;; find-in-project and cmd-t
  ;; “Cmd-T”-style command to fuzzily open a file in the current project
  ;;  Find-in-Project text search for finding an arbitrary string in the current project
  ;; (global-set-key (kbd "M-x") #'helm-M-x)
  ;; (global-set-key (kbd "C-i") #'helm-projectile-ag)
  ;; (global-set-key (kbd "s-t") #'helm-projectile-find-file-dwim)


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Section IV: Emacs Lisp mode behavior                                       ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (add-to-list 'auto-mode-alist
               '("\\(?:\\.elisp\\|emacs\\|/\\(?:emacs\\)\\)\\'"
               . emacs-lisp-mode))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Section V: Lisp mode behavior                                              ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Ensure Lisp mode for Lisp init files
  (add-to-list 'auto-mode-alist '("\\clrc" . lisp-mode))
  (add-to-list 'auto-mode-alist '("\\lisprc" . lisp-mode))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Section V: Slime Lisp mode behavior                                        ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun lisp-hook-fn ()
    (interactive)
    (slime-mode) ; start slime mode
    (local-set-key [tab] 'slime-complete-symbol) ; bind tab key to slime-complete-symbol

    ;; Common Lisp indentation
    ;;
    ;; C-M-q on the first paren reindents an expression
    ;; C-c M-q reindents a function
    ;;
    (local-set-key (kbd "M-q") 'slime-reindent-defun)
    ;;
    ;; Set indent function so common-lisp-indent-function will indent correctly
    (set (make-local-variable lisp-indent-function) 'common-lisp-indent-function)
    (setq lisp-indent-function 'common-lisp-indent-function)

    ;;; Available styles are: basic, classic, modern and sbcl.
    ;;; All of them are defined in slime-cl-indent.el file,
    ;;; but you can define your own style as well.
    (setq common-lisp-style "sbcl")

    (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
    (setq slime-load-failed-fasl 'never)) ; never load code that failed to compile

  ;; Finally we tell lisp-mode to run our function on startup
  (add-hook 'lisp-mode-hook 'lisp-hook-fn)


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Section VI: Ruby mode behavior                                             ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Doubt this is needed with the auto hooks in the next section
  ;; (setq initial-major-mode 'ruby-mode)

  ;; Apply enh-ruby-mode to files with these extensions
  (add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rake$" . enh-ruby-mode))
  (add-to-list 'auto-mode-alist '("Rakefile$" . enh-ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.gemspec$" . enh-ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.ru$" . enh-ruby-mode))
  (add-to-list 'auto-mode-alist '("Gemfile$" . enh-ruby-mode))
  (add-to-list 'auto-mode-alist '("Guard$" . enh-ruby-mode))
  ;; (add-to-list 'auto-mode-alist
  ;;     '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode))

  ;; Auto-close paired syntax elements like parens, quotes, when in ruby-mode
  ;; (require 'ruby-electric)
  ;; (add-hook 'ruby-mode-hook 'ruby-electric-mode)

  ;; When folding, take these delimiters into consideration
  (add-to-list 'hs-special-modes-alist
               '(enh-ruby-mode
                 "\\(class\\|def\\|do\\|if\\)" "\\(end\\)" "#"
                 (lambda (arg) (ruby-end-of-block)) nil))

  ;; Projectile-rails key bindings:
  ;;
  ;; M-x projectile-rails-find-model         ->  C-c r m
  ;; M-x projectile-rails-find-current-model ->  C-c r M
  ;; M-x projectile-rails-find-current-test  ->  C-c r T
  ;;
  (use-package projectile-rails
    :defer t
    :ensure t
    :init
    (setq projectile-rails-vanilla-command "bin/rails"
          projectile-rails-spring-command "bin/spring"
          projectile-rails-zeus-command "bin/zeus")
    ;; (projectile-rails-global-mode))
    (add-hook 'projectile-mode-hook 'projectile-rails-on))

  (autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)
  (add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode)
  (add-hook 'compilation-filter-hook 'inf-ruby-auto-enter)
  ;; Switch the compilation buffer mode with C-x C-q
  ;; (useful when interacting with a debugger)
  (add-hook 'after-init-hook 'inf-ruby-switch-setup)

  ;; rbenv
  (use-package rbenv
    :defer t
    :ensure t
    :init
    (add-to-list 'load-path "~/emacs.d/vendor/rbenv.el/")
    ;; searches for .ruby-version and activates the corresponding ruby
    (rbenv-use-corresponding)
    (setq rbenv-modeline-function 'rbenv--modeline-plain) ; remove colors
    ;; (global-rbenv-mode)
    (add-hook 'enh-ruby-mode-hook 'global-rbenv-mode))
  ;; (setq rbenv-show-active-ruby-in-modeline nil)
  ;; (rbenv-use-global) ; activates global ruby
  ;; (rbenv-use) ; allows you to choose what ruby version you want to use
  ;; Optional -- if your RBENV installation is located somewhere besides
  ;; ~/.rbenv, you will need to configure this:
  ;;(setq rbenv-installation-dir "/usr/local/rbenv")

  ;; (use-package bundler)
  ;; (add-to-list 'load-path "~/.emacs.d/vendor/bundler.el")
  ;; (add-hook 'ruby-mode-hook 'bundler)

  ;; Minitest key bindings:
  ;;
  ;; M-x minitest-verify     ->  C-c , v
  ;; M-x minitest-verify-all ->  C-c , a
  ;; M-x minitest-rerun      ->  C-c , r
  ;;
  (use-package minitest
    :defer t
    :ensure t
    :init (add-hook 'enh-ruby-mode-hook 'minitest-mode)
    :config (minitest-enable-appropriate-mode))

  ;; Evaluate tests in the current buffer with “C-c C-,”
  ;; (use-package 'ruby-test-mode)
  ;; (add-to-list 'load-path "~/.emacs.d/vendor/ruby-test-mode.el")
  ;; (add-hook 'enh-ruby-mode-hook 'ruby-test-mode)

  ;; Rubocop keybindings:
  ;;
  ;; M-x rubocop-check-project            ->  C-c C-r p
  ;; M-x rubocop-check-current-file       ->  C-c C-r f
  ;;
  ;; M-x rubocop-autocorrect-project      ->  C-c C-r P
  ;; M-x rubocop-autocorrect-current-file ->  C-c C-r F
  ;;
  (use-package rubocop
    :defer t :ensure t :init (add-hook 'enh-ruby-mode-hook 'rubocop-mode))

  ;; This function switches to the new buffer and permits killing it easily with "k":
  (add-hook 'compilation-finish-functions
            (lambda (new-buffer strg)
              (switch-to-buffer-other-window new-buffer)
              (read-only-mode)
              (goto-char (point-max))
              (local-set-key (kbd "k")
                             (lambda () (interactive) (kill-buffer-and-window))))) ; kills it
                     ;; (lambda () (interactive) (quit-restore-window))))) ; closes window

  ;; Functions to help with refactoring
  ;; (require 'ruby-refactor)
  ;; (add-hook 'enh-ruby-mode-hook 'ruby-refactor-mode-launch)

  ;; Easily toggle ruby's hash syntax with "M-x ruby-hash-syntax-toggle"
  (use-package ruby-hash-syntax :defer t :ensure t)

  ;; Ruby rdoc helpers mostly
  (use-package ruby-additional :defer t :ensure t)

  ;; Helpers to deal with strings and symbols
  (use-package ruby-tools :defer t :ensure t)

  ;; Support for YARD
  ;; (require 'yard-mode)
  ;; (add-hook 'ruby-mode-hook 'yard-mode)

  ;; Support for running rspec tests
  ;; (require 'rspec-mode)

  ;; Ruby buffer interaction
  ;; (setq seeing-is-believing-prefix "C-.")
  ;; (add-hook 'ruby-mode-hook 'seeing-is-believing)
  ;; (require 'seeing-is-believing)

  ;; rails-minor-mode
  ;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/rails-minor-mode"))
  ;; (require 'rails)

  (use-package whitespace
    :defer t
    :ensure t
    :init
    (setq whitespace-style
          '(face empty tabs lines-tail trailing)
          whitespace-global-modes
          '(not org-mode web-mode fundamental-mode "Web" emacs-lisp-mode)
          ;; whitespace-line-column 80
          whitespace-display-mappings
          ;; all numbers are Unicode codepoint in decimal. e.g. (insert-char 182 1)
          '(
            (space-mark nil) ; 32 SPACE, 183 MIDDLE DOT
            (newline-mark 10 [172 10]) ; 10 LINE FEED
            (tab-mark 9 [9654 9] [92 9])
            (tab-mark 9 [183 9] [92 9]) ; 9 TAB, MIDDLE DOT
            ))
    (global-whitespace-mode t))
  ;; (require 'whitespace-cleanup-mode)
  ;; (setq whitespace-line-column 80) ;; limit line length
  ;; (setq whitespace-style '(spaces tabs newline space-mark tab-mark newline-mark face lines-tail))
  ;; (setq whitespace-style '(newline newline-mark face lines-tail))
  ;; (setq whitespace-global-modes '(not org-mode web-mode fundamental-mode "Web" emacs-lisp-mode))
  ;; (global-whitespace-mode)

  ;; Disable ruby-mode and enh-ruby-mode auto-adding utf-8 magic comments
  (setq ruby-insert-encoding-magic-comment nil
        enh-ruby-add-encoding-comment-on-save nil)
)

;; No GC while minibuffer is open
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold (* 50 1000 1000)))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; Display GC messages in the mini-buffer
(setq garbage-collection-messages t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (oceanic)))
 '(custom-safe-themes
   (quote
    ("5c9bd73de767fa0d0ea71ee2f3ca6fe77261d931c3d4f7cca0734e2a3282f439" default)))
 '(package-selected-packages
   (quote
    (comment-or-uncomment-sexp haskell-mode rust-mode grizzl enh-ruby-mode popwin ruby-tools rubocop minitest slime better-defaults flx-ido scpaste smex magit whitespace-cleanup-mode select-themes oceanic-theme projectile projectile-rails seeing-is-believing inf-ruby saveplace)))
 '(safe-local-variable-values (quote ((encoding . utf-8)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
