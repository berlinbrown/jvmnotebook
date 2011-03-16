;;;****************************
;;; Emacs mode configuration
;;;
;;; Updated for Win32
;;; 11/10/2007
;;; Note: you may need to included the
;;; module and libraries below.  Most are programming
;;; language modes.
;;;****************************

;;******************
;; Emacs config:
;; Berlin Brown - 4/17/2008
;; Emacs version = This is GNU Emacs 22.1.1 (i386-mingw-nt5.1.2600)
;;******************

(custom-set-variables
  ;; custom-set-variables was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 '(case-fold-search t)
 '(current-language-environment "UTF-8")
 '(default-input-method "rfc1345")
 '(global-font-lock-mode t nil (font-lock))
 '(show-paren-mode t nil (paren)))
(custom-set-faces
  ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 )

;;------------------
;; Enable columns
;;------------------
(column-number-mode 1)

(setq-default truncate-lines 1)
;;(setq truncate-partial-width-windows nil)
;;(setq overflow-newline-into-fringe 1)

;;------------------
;; Key Bindings
;;------------------
(global-set-key "\C-c\C-g" 'goto-line)
(global-set-key "\C-c\C-c" "\C-q\t")
(global-set-key "\C-c\C-a" 'clipboard-kill-ring-save)
(global-set-key "\C-c\C-v" 'clipboard-yank)

(global-set-key [f4] 'shell)
(global-set-key [f5] "\C-x\h")
(global-set-key [f6] 'clipboard-kill-ring-save)
(global-set-key [f7] 'clipboard-yank)
(global-set-key [f8] 'htmlize-buffer)
;;;(global-set-key [f9] '(lambda () (interactive)
;;;			(if (comint-after-pmark-p)
;;;			    (comint-previous-input 1)
;;;			  (previous-line 1))))

(global-set-key [f9] (lambda () (interactive)			
		       (comint-previous-input 1)))

;;; Description:
;;;     So, to select-all and copy, do 'C-x h' 'C-c C-a'

(global-set-key "\C-z" 'advertised-undo)

;;******************
;;disable backup and autosave
;;******************
(setq backup-inhibited t)
(setq auto-save-default nil)

;;******************
;; Factor Mode
;;******************
(load-file "c:\\projects\\tools\\home\\projects\\aaageneralprojects\\factorbuild\\factor\\misc\\factor.el")
(setq factor-binary "c:\\projects\\tools\\home\\projects\\aaageneralprojects\\factorbuild\\factor\\factor-nt.exe")
(setq factor-image "c:\\projects\\tools\\home\\projects\\aaageneralprojects\\factorbuild\\factor\\factor-nt.image")

;;******************
;; Enable erlang mode
;;******************
;;(setq load-path (cons  "/usr/local/lib/erlang/lib/tools-2.5.5/emacs/"
;;    load-path))
;;(setq erlang-root-dir "/usr/local/lib/erlang")
;;(setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))
;;(require 'erlang-start)

;; ErlEmacsHome=C:\projects\tools\home\opt\erlang\erl5.6\lib\tools-2.6\emacs
(setq load-path (cons  "C:/projects/tools/home/opt/erlang/erl5.6/lib/tools-2.6/emacs"
					   load-path))
(setq erlang-root-dir "C:/projects/tools/home/opt/erlang/erl5.6")
(setq exec-path (cons "C:/projects/tools/home/opt/erlang/erl5.6/lib/tools-2.6/emacs" exec-path))
(require 'erlang-start)

;;*****************************
;; haskell mode
;; Added 11/10/2007
;;*****************************
(load "~/lib/emacs/haskell-mode-2.3/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;;******************
;; Install Speed Project Bar
;;******************
;;(add-to-list 'load-path "~/lib/emacs/speedbar-0.14beta4")
;;(autoload 'speedbar-frame-mode "speedbar" "Popup a speedbar frame" t)
;;(autoload 'speedbar-get-focus "speedbar" "Jump to speedbar frame" t)

;;*****************
;; Enable scala mode
;;*****************
(add-to-list 'load-path "~/lib/emacs/scala")
(require 'scala-mode-auto)

;;******************
;; Ruby Mode
;;*****************
(load-file "~/lib/emacs/ruby-mode.el")
(autoload 'ruby-mode "ruby-mode" "Major mode for editing ruby scripts." t)
(setq auto-mode-alist (cons '(".rb$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '(".rhtml$" . html-mode) auto-mode-alist))

;;******************
;; Python Mode
;;******************
;;(add-to-list 'load-path "~/lib/emacs/python")
;;(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
;;(setq interpreter-mode-alist (cons '("python" . python-mode)
;;                                       interpreter-mode-alist))
;;(autoload 'python-mode "python-mode" "Python editing mode." t)

(load-file "~/lib/emacs/htmlize.el")

;;******************
;; Botlist Emacs Mode
;;******************
(load-file "~/lib/emacs/botlist-mode.el")

;;******************
;; Clojure Emacs Mode
;;******************
(load-file "~/lib/emacs/clojure-mode.el")
(setq inferior-lisp-program
                                        ; Path to java implementation
      (let* ((java-path "java")
                                        ; Extra command-line options
                                        ; to java.
             (java-options "")
                                        ; Base directory to Clojure.
                                        ; Change this accordingly.
             (clojure-path "/cygdrive/c/projects/tools/home/projects/projects_ecl/botclient/botnetclient/clojure/")
                                        ; The character between
                                        ; elements of your classpath.
             (class-path-delimiter ":")
             (class-path (mapconcat (lambda (s) s)
                                        ; Add other paths to this list
                                        ; if you want to have other
                                        ; things in your classpath.
                                    (list (concat clojure-path "target/clojure.jar"))
                                    class-path-delimiter)))
        (concat java-path
                " " java-options
                " -cp " class-path
                " clojure.lang.Repl")))

;; Require clojure-mode to load and associate it to all .clj files.
(require 'clojure-mode)
(setq auto-mode-alist
      (cons '("\\.clj$" . clojure-mode)
            auto-mode-alist))

;; These are extra key defines because I kept typing them.  
;; Within clojure-mode, have Ctrl-x Ctrl-e evaluate the last 
;; expression.
;; Ctrl-c Ctrl-e is also there, because I kept typoing it.
(add-hook 'clojure-mode-hook
          '(lambda ()
             (define-key clojure-mode-map "\C-c\C-e" 'lisp-eval-last-sexp)
             (define-key clojure-mode-map "\C-x\C-e" 'lisp-eval-last-sexp)))

;;******************
;; Set Tab for 4 spaces of indendation
;; Update: 5/5/08, replace all tabs with spaces.
;; Add file specific settings for Makefiles or use Ctrl-q
;;******************
(setq c-basic-offset 4)
(setq default-tab-width 4)

(setq-default indent-tabs-mode nil)

(cd "~/")
(server-start)
