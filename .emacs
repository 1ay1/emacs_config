;
;
;
;
;
;
;
;

; start package.el with emacs
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)
;
;
;
;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("0e219d63550634bc5b0c214aced55eb9528640377daf486e13fb18a32bf39856" default)))
 '(flymake-google-cpplint-command "/usr/bin/cpplint")
 '(package-selected-packages
   (quote
    (powerline cmake-ide company flycheck rtags zenburn-theme irony google-c-style flymake-cursor flymake-google-cpplint iedit auto-complete-c-headers yasnippet auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;
;;
;;;;
;;
;;
;;
;;powerline
;;
;;
;;
;;
;;
(require 'powerline)
(powerline-default-theme)
;;
;;
;;
;;
;;
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
;;
;;
;;
;;
;;
;;
(require 'yasnippet)
   (yas-global-mode 1)
;;
;;
;;
;;
;;
(defun my:ac-c-header-init()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (add-to-list 'achead:include-directories '"/usr/include")
  (add-to-list 'achead:include-directories '"/usr/local/include")
  )
(add-hook 'c++-mode-hook 'my:ac-c-header-init)
(add-hook 'c-mode-hook 'my:ac-c-header-init)

;;
;;
;;
;;
;;fixing iedit bug
(define-key global-map (kbd "C-c ;") 'iedit-mode)
;;
;;
;;
;; flymake google 
;;
(defun my:flymake-google-init()
 (require 'flymake-google-cpplint)
 (custom-set-variables '(flymake-google-cpplint-command "/usr/bin/cpplint"))
 (flymake-google-cpplint-load)
 )
(add-hook 'c-mode-hook 'my:flymake-google-init)
(add-hook 'c++-mode-hook 'my:flymake-google-init)
;;
;;
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)
;;
;;
;;
(semantic-mode 1)
(defun my:add-semantic-to-autocomplete()
 (add-to-list 'ac-sources 'ac-source-semantic)
 )
(add-hook 'c-mode-common-hook 'my:add-semantic-to-autocomplete)
;;
(global-ede-mode 1)
(global-semantic-idle-scheduler-mode 1)
;;
;;
;; lines mode
(global-linum-mode 1)
;;
;;
;;
;;IRONY MODE
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
;;
;;
;;
;;
;; fly check
(global-flycheck-mode)
;;
;;
;;company mode
(add-hook 'after-init-hook 'global-company-mode)
;;
;;
;;
;; Rtags 
(require 'rtags)
(require 'company-rtags)

(setq rtags-completions-enabled t)
(eval-after-load 'company
  '(add-to-list
    'company-backends 'company-rtags))
(setq rtags-autostart-diagnostics t)
(rtags-enable-standard-keybindings)
;;
(setq rtags-use-helm t)
;;
;;
(cmake-ide-setup)
;;
;;

;;
;;
;;
;;SPELLING CHECK 
;;
;;
;;
 ;;
 ;;

 ;; if (aspell installed) { use aspell}
;; else if (hunspell installed) { use hunspell }
;; whatever spell checker I use, I always use English dictionary
;; I prefer use aspell because:
;; 1. aspell is older
;; 2. looks Kevin Atkinson still get some road map for aspell:
;; @see http://lists.gnu.org/archive/html/aspell-announce/2011-09/msg00000.html
(defun flyspell-detect-ispell-args (&optional run-together)
  "if RUN-TOGETHER is true, spell check the CamelCase words."
  (let (args)
    (cond
     ((string-match  "aspell$" ispell-program-name)
      ;; Force the English dictionary for aspell
      ;; Support Camel Case spelling check (tested with aspell 0.6)
      (setq args (list "--sug-mode=ultra" "--lang=en_US"))
      (if run-together
          (setq args (append args '("--run-together" "--run-together-limit=5" "--run-together-min=2")))))
     ((string-match "hunspell$" ispell-program-name)
      ;; Force the English dictionary for hunspell
      (setq args "-d en_US")))
    args))

(cond
 ((executable-find "aspell")
  ;; you may also need `ispell-extra-args'
  (setq ispell-program-name "aspell"))
 ((executable-find "hunspell")
  (setq ispell-program-name "hunspell")

  ;; Please note that `ispell-local-dictionary` itself will be passed to hunspell cli with "-d"
  ;; it's also used as the key to lookup ispell-local-dictionary-alist
  ;; if we use different dictionary
  (setq ispell-local-dictionary "en_US")
  (setq ispell-local-dictionary-alist
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8))))
 (t (setq ispell-program-name nil)))

;; ispell-cmd-args is useless, it's the list of *extra* arguments we will append to the ispell process when "ispell-word" is called.
;; ispell-extra-args is the command arguments which will *always* be used when start ispell process
;; Please note when you use hunspell, ispell-extra-args will NOT be used.
;; Hack ispell-local-dictionary-alist instead.
(setq-default ispell-extra-args (flyspell-detect-ispell-args t))
;; (setq ispell-cmd-args (flyspell-detect-ispell-args))
(defadvice ispell-word (around my-ispell-word activate)
  (let ((old-ispell-extra-args ispell-extra-args))
    (ispell-kill-ispell t)
    (setq ispell-extra-args (flyspell-detect-ispell-args))
    ad-do-it
    (setq ispell-extra-args old-ispell-extra-args)
    (ispell-kill-ispell t)
    ))

(defadvice flyspell-auto-correct-word (around my-flyspell-auto-correct-word activate)
  (let ((old-ispell-extra-args ispell-extra-args))
    (ispell-kill-ispell t)
    ;; use emacs original arguments
    (setq ispell-extra-args (flyspell-detect-ispell-args))
    ad-do-it
    ;; restore our own ispell arguments
    (setq ispell-extra-args old-ispell-extra-args)
    (ispell-kill-ispell t)
    ))

(defun text-mode-hook-setup ()
  ;; Turn off RUN-TOGETHER option when spell check text-mode
  (setq-local ispell-extra-args (flyspell-detect-ispell-args)))
(add-hook 'text-mode-hook 'text-mode-hook-setup)

;;
;;
;;
;;
(defun ispell-word-then-abbrev (p)
  "Call `ispell-word'. Then create an abbrev for the correction made.
With prefix P, create local abbrev. Otherwise it will be global."
  (interactive "P")
  (let ((before (downcase (or (thing-at-point 'word) "")))
        after)
    (call-interactively 'ispell-word)
    (setq after (downcase (or (thing-at-point 'word) "")))
    (unless (string= after before)
      (define-abbrev
        (if p local-abbrev-table global-abbrev-table) before after))
      (message "\"%s\" now expands to \"%s\" %sally."
               before after (if p "loc" "glob"))))

(define-key ctl-x-map (kbd "C-i") 'ispell-word-then-abbrev)
(setq save-abbrevs t)
(setq-default abbrev-mode t)
;;
;;
;;
;;
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
  
;;
;;
;;
;;
(dolist (mode '(emacs-lisp-mode-hook
                inferior-lisp-mode-hook
                clojure-mode-hook
                python-mode-hook
                js-mode-hook
                R-mode-hook))
  (add-hook mode
            '(lambda ()
               (flyspell-prog-mode))))
;;
;;
;;
;;
;;
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
;;
;;
