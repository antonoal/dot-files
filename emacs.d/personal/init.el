;;; package --- Summary

;;; Commentary:

;;; Code:

;;; loading additional packages
(prelude-require-packages '(ensime base16-theme highlight-indent-guides neotree))

;; (setq prelude-theme 'base16-ocean-dark)
(load-theme 'base16-ocean-dark)

;;; enabling global hook for scala files to run in ensime mode
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;;; flycheck overwrote all the C-c !. Rebinding it back for the org-mode
(define-key flycheck-mode-map (kbd "C-c ! !") 'org-time-stamp-inactive)

;;; start week on Monday in Calendar
(setq calendar-week-start-day 1)

;;; deft
;; (require 'deft)
;; (setq deft-extensions '("org" "txt" "tex"))
;; (setq deft-directory "~/Org")
;; (setq deft-recursive t)
;; (global-set-key [f8] 'deft)

;;; org-mode changes
(setq org-directory "~/Org")
(setq org-default-notes-file "~/Org/refile.org")
(setq org-agenda-files (quote ("~/Org"
                               "~/Org/notes"
                               )))

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))

;; (setq org-todo-keyword-faces
;;       (quote (;("TODO" :foreground "red" :weight bold)
;;               ("NEXT" :foreground "blue" :weight bold)
;;               ("DONE" :foreground "forest green" :weight bold)
;;               ;("WAITING" :foreground "orange" :weight bold)
;;               ("HOLD" :foreground "magenta" :weight bold)
;;               ("CANCELLED" :foreground "forest green" :weight bold)
;;               ;("MEETING" :foreground "forest green" :weight bold)
;;               ;("PHONE" :foreground "forest green" :weight bold)
;;               )))

; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes (quote confirm))

;; I use C-c c to start capture mode
(global-set-key (kbd "C-c c") 'org-capture)

;; Capture templates
(setq org-capture-templates
      (quote (("t" "Todo" entry (file "~/Org/refile.org")
               "* TODO %?")
              ("n" "Note" entry (file "~/Org/refile.org")
               "* %?\n  CREATED: %u\n  %c\n")
              ("m" "Mind Dump" entry (file "~/Org/refile.org")
               "* %?   :minddump:\n  CREATED: %u\n  ")
              )))

;; Compact the block agenda view
(setq org-agenda-compact-blocks t)

;; Agenda defaults to one day only
(setq org-agenda-span 'day)

;; Ignore deadline if scheduled
(setq org-agenda-skip-deadline-prewarning-if-scheduled t)

;; Hide leading stars
(setq org-hide-leading-stars t)

;; Custom agenda command definitions
(setq org-agenda-custom-commands
      (quote ((" " "Agenda"
               ((agenda "" nil)
                (tags "minddump"
                      ((org-agenda-overriding-header "Mind Dump")
                       ))
                (tags-todo "-SCHEDULED={.+}-DEADLINE={.+}/!NEXT"
                           ((org-agenda-overriding-header "Next")
                            ))
                (tags-todo "/!HOLD"
                           ((org-agenda-overriding-header "On Hold")
                            ))
                (tags "-minddump+refile"
                      ((org-agenda-overriding-header "Refile")
                       ))
                (tags-todo "-gtd-refile/!"
                           ((org-agenda-overriding-header "Notes Follow Up")
                            ))
                )
               nil)
              ("w" "Week Review"
               ((tags-todo "+gtd-SCHEDULED={.+}-DEADLINE={.+}/!-NEXT-HOLD"
                           ((org-agenda-overriding-header "Someday")
                            ))
                )
               nil)
              )))

(load (expand-file-name "next-spec-day.el" prelude-personal-dir))

;; Highlight indentation
(require 'highlight-indent-guides)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(set-face-background 'highlight-indent-guides-odd-face "#343d46")
(set-face-background 'highlight-indent-guides-even-face "#2b303b")

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


;; Neotree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-smart-open t)

(provide 'init)
;;; init.el ends here
