;;; package --- Summary

;;; Commentary:

;;; Code:

;;; loading additional packages
(prelude-require-packages '(multiple-cursors hydra neotree))

(when (window-system)
  (tool-bar-mode 0)               ;; Toolbars were only cool with XEmacs
  (when (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))
  (scroll-bar-mode -1))

;; Newline character at the end of the line upon save
(setq mode-require-final-newline nil)
(setq require-final-newline nil)

(setq auto-mode-alist
      (cons
       '("\\.m$" . octave-mode)
       auto-mode-alist))

;; (use-package fill
;;              :bind ("C-c T f" . auto-fill-mode)
;;              :init (add-hook 'org-mode-hook 'turn-on-auto-fill)
;;              :diminish auto-fill-mode)

;; (use-package multiple-cursors
;;              :ensure t
;;              :config
;;              (global-set-key
;;               (kbd "C-c C-.")
;;               (defhydra hydra-multiple-cursors ()
;;                 "multiple-cursors"
;;                 ("." mc/mark-all-dwim                   "all-dwim")
;;                 ("C-." mc/mark-all-like-this-dwim       "all-like-dwim")
;;                 ("n" mc/mark-next-like-this             "next")
;;                 ("p" mc/mark-previous-like-this         "previous")
;;                 ("a" mc/mark-all-like-this              "mark-all")
;;                 ("N" mc/mark-next-symbol-like-this      "next-symbol")
;;                 ("P" mc/mark-previous-symbol-like-this  "previous-symbol")
;;                 ("A" mc/mark-all-symbols-like-this      "all-symbols")
;;                 ("f" mc/mark-all-like-this-in-defun     "in-func")
;;                 ("l" mc/edit-lines                      "all-lines")
;;                 ("e" mc/edit-ends-of-lines              "end-lines"))))

;; Tramps default protocol
(setq tramp-default-method "ssh")

;;; enabling global hook for scala files to run in ensime mode
;; (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;; Multiple cursors
(require 'multiple-cursors)
;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;; Note: If you want to insert a newline in multiple-cursors-mode, use C-j

(global-set-key
 (kbd "C-c C-.")
 (defhydra hydra-multiple-cursors ()
   "multiple-cursors"
   ("." mc/mark-all-dwim                   "all-dwim")
   ("C-." mc/mark-all-like-this-dwim       "all-like-dwim")
   ("n" mc/mark-next-like-this             "next")
   ("p" mc/mark-previous-like-this         "previous")
   ("a" mc/mark-all-like-this              "mark-all")
   ("N" mc/mark-next-symbol-like-this      "next-symbol")
   ("P" mc/mark-previous-symbol-like-this  "previous-symbol")
   ("A" mc/mark-all-symbols-like-this      "all-symbols")
   ("f" mc/mark-all-like-this-in-defun     "in-func")
   ("l" mc/edit-lines                      "all-lines")
   ("e" mc/edit-ends-of-lines              "end-lines")))

(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-theme 'arrow)

(require 'intero)
(add-hook 'haskell-mode-hook 'intero-mode)

;; Fix helm arrow keys
(define-key helm-map (kbd "<left>") 'helm-previous-source)
(define-key helm-map (kbd "<right>") 'helm-next-source)
;; helm-find-files
(customize-set-variable 'helm-ff-lynx-style-map t)
;; helm-imenu
(customize-set-variable 'helm-imenu-lynx-style-map t)
;; semantic
(customize-set-variable 'helm-semantic-lynx-style-map t)
;; helm-occur
(customize-set-variable 'helm-occur-use-ioccur-style-keys t)
;; for helm-grep
(customize-set-variable 'helm-grep-use-ioccur-style-keys t)

(provide 'init)
;;; init.el ends here
