;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; .emacs                                                                     ;;
;; Author: Jon Atack                                                          ;;
;; Licence: MIT                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file configures a basic Emacs setup. Feel free to customize it.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section I: Generic settings                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; No GC while minibuffer is open
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; Sacrifice memory for speed by reducing GC globally:
;; (setq gc-cons-threshold 20000000)

;; Display GC messages
(setq garbage-collection-messages t)

(set-language-environment "UTF-8")
;; (setenv "LANG" "en_US.UTF-8")
(prefer-coding-system 'utf-8) ;; Prefer UTF-8 encoding

;; Resize the window to my screen
;; (setq default-frame-alist '(
;;   (width . 105) ; characters
;;   (height . 56)
;;   (top . 0)
;;   (left . 0) ; move window x characters to the right (308 = centered over keyboard)
;;   ))

(if (display-graphic-p)
    (progn
      (setq initial-frame-alist
            '(
              ; (tool-bar-lines . 0)
              (width . 105) ; chars
              (height . 56) ; lines
              ; (background-color . "honeydew")
              (left . 0)
              (top . 0)))
      (setq default-frame-alist
            '(
              ; (tool-bar-lines . 0)
              (width . 105)
              (height . 56)
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

;; The following 3 lines disable unnecessary GUI elements, in this case the
;; menu bar, the tool bar and the scroll bar. If you wish, you can comment out
;; the menu-bar and keep it, but eventually I recommend you disable it.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(column-number-mode 1)

;; Auto-insert newline at end of file
(setq-default require-final-newline t mode-require-final-newline t )

;; Display filepath in window title bar
(setq-default frame-title-format '((:eval (if (buffer-file-name)
                                              (abbreviate-file-name (buffer-file-name)) "%f"))))

(setq resize-mini-windows nil) ; Do not resize the mini-buffer to keep it to one line.

(setq inhibit-startup-screen t) ; Don’t display the Emacs splash screen

(global-linum-mode 1) ; global-linum-mode adds line numbers to all open windows.
(setq linum-format "%4d ") ;; 4 character and a space for line numbers

(show-paren-mode t) ; Highlight matching parentheses globally.

;; Display “lambda” as “λ”
(global-prettify-symbols-mode t)

;; (setq case-fold-search t) ; Make searches case-insensitive

(setq browse-url-browser-function 'eww-browse-url)

;; (set-face-attribute 'default nil :height 90) ; Make the default font slightly smaller.

(setq-default indent-tabs-mode nil) ; Make indentation use spaces.

;; Use 2 spaces for indentation everywhere
(setq-default tab-width 2)
(setq-default c-basic-offset 2)
(setq css-indent-offset 2)
(setq js-indent-level 2)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-css-indent-offset 2)

(setq-default fill-column 80) ; Sets a 80 character line width
(setq large-file-warning-threshold nil) ; Don’t warn me about opening large files
(setq x-select-enable-clipboard t) ; Enable copy/past-ing from clipboard
(setq system-uses-terminfo nil) ; Fix weird color escape sequences
(fset 'yes-or-no-p 'y-or-n-p) ; Answer with y and n instead of yes and no
;; (setq confirm-kill-emacs 'yes-or-no-p) ; Ask for confirmation before closing emacs
(global-auto-revert-mode t) ; Always reload the file if it changed on disk
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section II: Packages                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; No GC during this memory-heavy init phase
(let ((gc-cons-threshold most-positive-fixnum))

  ;; Elpa, the default package repository for emacs is fairly conservative, so
  ;; we'll add the melpa and marmalade repositories
  (require 'package)

  (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")))

  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives
               '("marmalade" . "https://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives
               '("org" . "https://orgmode.org/elpa/") t)

  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

  ;; Activate all the packages
  (package-initialize)

  ;; Fetch the list of packages available
  (unless package-archive-contents
    (package-refresh-contents))

  ;; Install any missing packages
  (dolist (package package-selected-packages)
    (unless (package-installed-p package)
      (package-refresh-contents)
      (package-install package)))

  (require 'better-defaults)

  ;; Save the cursor position between sessions
  (require 'saveplace)
  (setq-default save-place t)
  (setq save-place-file (expand-file-name ".places" user-emacs-directory))

  (require 'flx-ido)
  (ido-mode t)
  (ido-everywhere t)
  (flx-ido-mode t)
  (setq ido-enable-flex-matching t)
  ;; Disable ido faces to see flx highlights:
  ;; (setq ido-use-faces nil)
  ;; If you don't want to use the flx's highlights you can turn them off like this:
  (setq flx-ido-use-faces nil)

  ;; (add-hook 'clojure-mode-hook #'smartparens-strict-mode)

  ;; (setq ns-pop-up-frames nil)

  ;; SLIME
  ;; (add-to-list 'load-path "~/quicklisp/dists/quicklisp/software/slime-v2.22")
  ;; (load (expand-file-name "~/quicklisp/slime-helper.el"))
  (require 'slime-autoloads)

  (slime-setup '(slime-fancy))
  (setq slime-contribs '(slime-fancy slime-asdf slime-indentation
      slime-banner slime-tramp slime-mdot-fu slime-scratch
      slime-company slime-editing-commands slime-quicklisp))
  ;; (setq slime-contribs '(slime-repl)) ; repl only

  (setq slime-startup-animation nil)

  ;; (setq inferior-lisp-program "/usr/bin/sbcl --noinform")
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

  ;; (global-set-key (kbd "C-c s") 'slime-selector)
  ;; (global-set-key (kbd "C-c h") 'clhs-lookup)
  ;; (global-set-key (kbd "C-c r") 'slime-pop-find-definition-stack)

  ;; Set the enter key to newline-and-indent which inserts a new line and then
  ;; indents according to the major mode. This is very convenient.
  ;; (global-set-key (kbd "RET") 'newline-and-indent)
  ;; (define-key global-map (kbd "RET") 'newline-and-indent)

  ;; By default C-x o is bound to 'other window, but I find I use it much more
  ;; ofther than open-line, which is bound to C-o, so I swap their definitions
  (global-set-key (kbd "C-o") 'other-window)
  (global-set-key (kbd "C-x o") 'open-line)

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
  ;; Section IV: Setup Emacs Lisp mode behavior                                 ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (add-to-list 'auto-mode-alist
      '("\\(?:\\.elisp\\|emacs\\|sbclrc\\|/\\(?:emacs\\|sbclrc\\)\\)\\'"
      . emacs-lisp-mode))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Section V: Setup Slime Lisp mode behavior                                  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun lisp-hook-fn ()
    (interactive)
    (slime-mode) ; start slime mode
    ;; Some useful key-bindings
    (local-set-key [tab] 'slime-complete-symbol)
    (local-set-key (kbd "M-q") 'slime-reindent-defun)
    ;; We set the indent function so common-lisp-indent-function will indent our code correctly
    (set (make-local-variable lisp-indent-function) 'common-lisp-indent-function)
    (setq lisp-indent-function 'common-lisp-indent-function)
    (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
    ;; We tell slime to not load failed compiled code
    (setq slime-load-failed-fasl 'never))

  ;; Finally we tell lisp-mode to run our function on startup
  (add-hook 'lisp-mode-hook 'lisp-hook-fn)


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Section VI: Ruby mode                                                       ;;
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

  ;; The projectile keymap prefix needs to be called before the mode is required/loaded
  ;; (setq projectile-keymap-prefix (kbd "C-c C-p"))
  (setq projectile-project-search-path '("~/projects/" "~/lisp/"))
  (require 'projectile)
  ;; (projectile-global-mode)
  (setq projectile-completion-system 'grizzl)
  (add-hook 'enh-ruby-mode-hook 'projectile-mode)

  (require 'projectile-rails)
  (setq projectile-rails-vanilla-command "bin/rails"
        projectile-rails-spring-command "bin/spring"
        projectile-rails-zeus-command "bin/zeus")
  ;; (projectile-rails-global-mode)
  ;; Start projectile-rails
  (add-hook 'projectile-mode-hook 'projectile-rails-on)

  (autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)
  (add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode)
  (add-hook 'compilation-filter-hook 'inf-ruby-auto-enter)
  ;; Switch the compilation buffer mode with C-x C-q
  ;; (useful when interacting with a debugger)
  (add-hook 'after-init-hook 'inf-ruby-switch-setup)

  ;; rbenv
  (setq rbenv-modeline-function 'rbenv--modeline-plain) ; remove colors
  (add-to-list 'load-path "~/emacs.d/vendor/rbenv.el/")
  (require 'rbenv)
  ;; (global-rbenv-mode)
  (add-hook 'enh-ruby-mode-hook 'global-rbenv-mode)
  ;; (setq rbenv-show-active-ruby-in-modeline nil)
  (rbenv-use-corresponding) ; searches for .ruby-version and activates the corresponding ruby
  ;; (rbenv-use-global) ; activates global ruby
  ;; (rbenv-use) ; allows you to choose what ruby version you want to use
  ;; Optional -- if your RBENV installation is located somewhere besides
  ;; ~/.rbenv, you will need to configure this:
  ;;(setq rbenv-installation-dir "/usr/local/rbenv")

  ;; (add-to-list 'load-path "~/.emacs.d/vendor/bundler.el")
  ;; (require 'bundler)
  ;; (add-hook 'ruby-mode-hook 'bundler)

  (require 'minitest)
  ;; M-x minitest-verify     ->  C-c C-, v
  ;; M-x minitest-verify-all ->  C-c C-, a
  ;; M-x minitest-rerun      ->  C-c C-, r
  (minitest-enable-appropriate-mode)
  (add-hook 'enh-ruby-mode-hook 'minitest-mode)

  ;; Evaluate tests in the current buffer with “C-c C-,”
  ;; (add-to-list 'load-path "~/.emacs.d/vendor/ruby-test-mode.el")
  ;; (require 'ruby-test-mode)
  ;; (add-hook 'enh-ruby-mode-hook 'ruby-test-mode)

  (require 'rubocop)
  ;; M-x rubocop-check-project            ->  C-c C-r p
  ;; M-x rubocop-check-current-file       ->  C-c C-r f
  ;;
  ;; M-x rubocop-autocorrect-project      ->  C-c C-r P
  ;; M-x rubocop-autocorrect-current-file ->  C-c C-r F
  (add-hook 'enh-ruby-mode-hook #'rubocop-mode)

  ;; This function switches to the new buffer and permits killing it easily with "k":
  (add-hook 'compilation-finish-functions
            (lambda (new-buffer strg)
              (switch-to-buffer-other-window new-buffer)
              (read-only-mode)
              (goto-char (point-max))
              (local-set-key (kbd "k")
                             (lambda () (interactive) (kill-buffer-and-window))))) ; kills it
                      ; (lambda () (interactive) (quit-restore-window))))) ; closes window

  ;; Functions to help with refactoring
  ;; (require 'ruby-refactor)
  ;; (add-hook 'enh-ruby-mode-hook 'ruby-refactor-mode-launch)

  ;; Easily toggle ruby's hash syntax with "M-x ruby-hash-syntax-toggle"
  (require 'ruby-hash-syntax)

  ;; Ruby rdoc helpers mostly
  (require 'ruby-additional)

  ;; Helpers to deal with strings and symbols
  (require 'ruby-tools)

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

  (require 'whitespace)
  (setq whitespace-style '(face empty tabs lines-tail trailing))
  (setq whitespace-global-modes '(not org-mode web-mode fundamental-mode "Web" emacs-lisp-mode))
  (global-whitespace-mode t)

  ;; (require 'whitespace)
  ;; (require 'whitespace-cleanup-mode)
  ;; (setq whitespace-line-column 80) ;; limit line length
  ;; (setq whitespace-style '(spaces tabs newline space-mark tab-mark newline-mark face lines-tail))
  ;; (setq whitespace-style '(newline newline-mark face lines-tail))
  (setq whitespace-display-mappings
        ;; all numbers are Unicode codepoint in decimal. e.g. (insert-char 182 1)
        '(
          (space-mark nil) ; 32 SPACE, 183 MIDDLE DOT
          (newline-mark 10 [172 10]) ; 10 LINE FEED
          (tab-mark 9 [9654 9] [92 9])
          (tab-mark 9 [183 9] [92 9]) ; 9 TAB, MIDDLE DOT
          ))
  ;; (setq whitespace-global-modes '(not org-mode web-mode fundamental-mode "Web" emacs-lisp-mode))
  ;; (global-whitespace-mode)

  (setq ruby-insert-encoding-magic-comment nil) ; disable ruby-mode auto-adding utf-8 magic comments

)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (oceanic)))
 '(custom-safe-themes
   (quote
    ("4486ade2acbf630e78658cd6235a5c6801090c2694469a2a2b4b0e12227a64b9" "645599a2aab022fd7677124515a3104a60ba64d2cafdd77a6e7703f8ae97250c" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "eb25c68d3959c31d34021aa722d5ea1c53ea69714580b2b8c150592becf412cf" "5c9bd73de767fa0d0ea71ee2f3ca6fe77261d931c3d4f7cca0734e2a3282f439" default)))
 '(package-selected-packages
   (quote
    (grizzl enh-ruby-mode popwin ruby-tools rubocop minitest slime better-defaults flx-ido scpaste smex magit paredit whitespace-cleanup-mode select-themes oceanic-theme nzenburn-theme solarized-theme projectile projectile-rails seeing-is-believing inf-ruby saveplace))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
