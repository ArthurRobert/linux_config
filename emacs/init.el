(require 'package)

(add-to-list 'package-archives '("melpa"     . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("gnu"       . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)

(setq user-full-name "Arthur ROBERT"
      user-mail-address "a.arthur.robert@gmail.com")


(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

(tooltip-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)
(global-font-lock-mode 1)
;; (global-hl-line-mode t)                     ; highline the current line 
(set-face-background 'region "gray80")
(column-number-mode 1)
(global-linum-mode t)


(setq auto-save-timeout 120)                   ; Autosave every two minute
(speedbar t)                                   ; Quick file access with bar
(setq make-backup-files nil)                   ; No backup files ~

(setq pop-up-frame t)                          ; Buffers in separate frames

(fset 'yes-or-no-p 'y-or-n-p)

(prefer-coding-system 'utf-8)


(setq custom-theme-directory "~/.emacs.d/themes/")
(add-to-list 'load-path "~/.emacs.d/themes/")

;; (require 'color-theme)
;; (color-theme-initialize)
;; (color-theme-robin-hood)


;;--CMake mode--;
(setq auto-mode-alist
      (append
       '(("CMakeLists\\.txt\\'" . cmake-mode))
       '(("\\.cmake\\'" . cmake-mode))
       auto-mode-alist))




;;------------------------;;
;;------Keys binding------;;
;;------------------------;;

(global-set-key [f3]    'vectra-man-on-word)
(global-set-key (kbd "C-c c") 'comment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)
(global-set-key (kbd "C-c <right>") 'next-buffer)
(global-set-key (kbd "C-c <left>") 'previous-buffer)
(global-set-key (kbd "C-c ;") 'iedit-mode)


;;org mode
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)


;; Transpose stuff with M-t
;; (global-set-key "M-t" nil) ;; which used to be transpose-words
;; (global-set-key "M-t l" 'transpose-lines)
;; (global-set-key "M-t w" 'transpose-words)
;; (global-set-key "M-t t" 'transpose-words)
;; (global-set-key "M-t M-t" 'transpose-words)
;; (
;;  global-set-key "M-t s" 'transpose-sexps)

;;--plug--;;

;;--org mode--;;
;; TODO list
;;(require 'org)
;;(setq org-log-done t)
;;(setq org-agenda-files (list "~/org_files/test.org"))


;;--gccsense--;;
;; code analyzer, completion
(require 'gccsense)


;;--flymake--;
;; show the compilations errors on the fly
(require 'flymake)

(defun flymake-cc-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
		       'flymake-create-temp-inplace))
	 (local-file  (file-relative-name
		       temp-file
		       (file-name-directory buffer-file-name))))

    (list "g++" (list "-Wall" "-Wextra" "-fsyntax-only" local-file))))
(push '("\\.cpp$" flymake-cc-init) flymake-allowed-file-name-masks)

(add-hook 'c++-mode-hook 'flymake-mode)






;;--yasnippet--;;
;; code completion
(require 'yasnippet)
(yas-global-mode 1)

(require 'semantic)
;; code analyzer, completion
(semantic-mode 1)

(defun my:add-semantic-to-autocomplete()
  (add-to-list 'ac-sources 'ac-source-semantic)
  )

(add-hook 'c-mode-common-hook 'my:add-semantic-to-autocomplete)
(setq semantic-c-dependency-system-include-path '("/usr/include"))


;;(global-ede-mode 1)
;;(global-semantic-idle-scheduler-mode 1)


;;--aspell--;;
;; orthography checker
(setq ispell-program-name "aspell")
(setq ispell-dictionary "english")
(setq flyspell-default-dictionary "english")


(add-hook 'c++-mode-hook
	  (lambda() (flyspell-prog-mode)))

;;--autocomplete--;;
;; auto completion
(require 'auto-complete)
(require 'auto-complete-config)

(ac-config-default)

;; configure the autocomplete headers
(defun my:ac-c-header-init() 
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (add-to-list 'achead:include-directories '"/usr/include")
)
(add-hook 'c++-mode-hook 'my:ac-c-header-init)
(add-hook 'c-mode-hook 'my:ac-c-header-init)
(define-key global-map (kbd "C-c ;") 'iedit-mode)





;; (autoload 'octave-mode "octave-mode" nil t)
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))

;; (add-hook 'octave-mode-hook
;;	  (lambda ()
;;	    (abbrev-mode 1)
;;	    (auto-fill-mode 1)
;;	    (if (eq window-system 'x)
;;		(font-lock-mode 1))))
