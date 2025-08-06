;;; heavybool-cat-explorer.el --- Emacs configuration for HeavyBool Category Explorer

;;; Commentary:
;; Project-specific Emacs configuration for Scheme development
;; with Geiser, Guile, and Org mode support

;;; Code:

;; Package initialization
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Ensure required packages are installed
(defvar heavybool-required-packages
  '(geiser
    geiser-guile
    paredit
    company
    rainbow-delimiters
    org))

(dolist (package heavybool-required-packages)
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install package)))

;; Load required features
(require 'geiser)
(require 'geiser-guile)
(require 'paredit)
(require 'company)
(require 'rainbow-delimiters)
(require 'org)
(require 'tramp)

;; Project settings
(setq heavybool-project-root (or (getenv "PROJECT_ROOT")
                                  (expand-file-name default-directory)))
(setq heavybool-project-name (or (getenv "PROJECT_NAME")
                                  "heavybool-cat-explorer"))

;; Geiser configuration for Guile
(setq geiser-guile-binary "guile3")
(setq geiser-active-implementations '(guile))
(setq geiser-repl-history-filename 
      (expand-file-name ".geiser-history" heavybool-project-root))
(setq geiser-repl-startup-time 10000)

;; Load path configuration
(add-to-list 'geiser-guile-load-path 
             (expand-file-name "src" heavybool-project-root))
(add-to-list 'geiser-guile-load-path 
             (expand-file-name "examples" heavybool-project-root))
(add-to-list 'geiser-guile-load-path 
             (expand-file-name "tests" heavybool-project-root))

;; Paredit configuration
(add-hook 'scheme-mode-hook 'paredit-mode)
(add-hook 'geiser-repl-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

;; Rainbow delimiters for better parenthesis visibility
(add-hook 'scheme-mode-hook 'rainbow-delimiters-mode)
(add-hook 'geiser-repl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

;; Company mode for auto-completion
(add-hook 'scheme-mode-hook 'company-mode)
(add-hook 'geiser-repl-mode-hook 'company-mode)
(setq company-idle-delay 0.3)
(setq company-minimum-prefix-length 2)

;; Org mode configuration for literate programming
(org-babel-do-load-languages
 'org-babel-load-languages
 '((scheme . t)))

(setq org-confirm-babel-evaluate nil)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

;; TRAMP configuration for remote development
(setq tramp-default-method "ssh")
(setq tramp-verbose 3)

;; Project-specific keybindings
(global-set-key (kbd "C-c g r") 'geiser-mode)
(global-set-key (kbd "C-c g c") 'geiser-connect)
(global-set-key (kbd "C-c g l") 'geiser-load-file)
(global-set-key (kbd "C-c g e") 'geiser-eval-definition)
(global-set-key (kbd "C-c g b") 'geiser-eval-buffer)
(global-set-key (kbd "C-c p f") 
                (lambda () 
                  (interactive)
                  (find-file 
                   (expand-file-name "src/heavybool.scm" heavybool-project-root))))

;; Custom functions for project navigation
(defun heavybool-open-source ()
  "Open a source file in the project."
  (interactive)
  (let ((default-directory (expand-file-name "src" heavybool-project-root)))
    (call-interactively 'find-file)))

(defun heavybool-open-example ()
  "Open an example file in the project."
  (interactive)
  (let ((default-directory (expand-file-name "examples" heavybool-project-root)))
    (call-interactively 'find-file)))

(defun heavybool-run-tests ()
  "Run the project tests."
  (interactive)
  (let ((test-file (expand-file-name "tests/run-tests.scm" heavybool-project-root)))
    (geiser-load-file test-file)))

;; Bind custom functions
(global-set-key (kbd "C-c h s") 'heavybool-open-source)
(global-set-key (kbd "C-c h e") 'heavybool-open-example)
(global-set-key (kbd "C-c h t") 'heavybool-run-tests)

;; Display project information
(message "HeavyBool Category Explorer - Emacs configuration loaded")
(message "Project root: %s" heavybool-project-root)

;; Start Geiser REPL automatically
(geiser-guile)

(provide 'heavybool-cat-explorer)
;;; heavybool-cat-explorer.el ends here