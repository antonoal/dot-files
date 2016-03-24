;;; package --- Summary

;;; Commentary:

;;; Code:

;;; loading additional packages
(prelude-require-packages '(base16-theme smart-mode-line-powerline-theme highlight-indent-guides))

(load-theme 'base16-ocean-dark)

;; Fonts
(defvar aa/fixed-font-family
  (cond ((x-list-fonts "Inconsolata LGC")   "Inconsolata LGC")
        ((x-list-fonts "M+ 2c")          "M+ 2c"))
  "My fixed width font based on what is installed, `nil' if not defined.")

(when aa/fixed-font-family
  (set-frame-font aa/fixed-font-family)
  (set-face-attribute 'default nil :font aa/fixed-font-family :height 120)
  (set-face-font 'default aa/fixed-font-family))

;; Smart mode line
(require 'smart-mode-line-powerline-theme)
(setq sml/theme 'powerline)
(setq sml/shorten-directory t)
(setq sml/shorten-modes t)

;; Fixes the powerline's arrows on OSX
(setq ns-use-srgb-colorspace nil)

;; Highlight indentation
;; (require 'highlight-indent-guides)
;; (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
;; (set-face-background 'highlight-indent-guides-odd-face "#343d46")
;; (set-face-background 'highlight-indent-guides-even-face "#2b303b")

;; Folds all code on an indentation level greater than the current line
;; (defun aj-toggle-fold ()
;;   "Toggle fold all lines larger than indentation on current line"
;;   (interactive)
;;   (let ((col 1))
;;     (save-excursion
;;       (back-to-indentation)
;;       (setq col (+ 1 (current-column)))
;;       (set-selective-display
;;        (if selective-display nil (or col 1))))))
;; (global-set-key [(M C i)] 'aj-toggle-fold)

(provide 'facelift)
;;; facelift.el ends here
