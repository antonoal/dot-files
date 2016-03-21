;;; package --- Summary

;;; Commentary:

;;; Code:

;;; flycheck overwrote all the C-c !. Rebinding it back for the org-mode
(define-key flycheck-mode-map (kbd "C-c ! !") 'org-time-stamp-inactive)

;;; start week on Monday in Calendar
(setq calendar-week-start-day 1)

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

;; Font coloring in code blocks
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

(load (expand-file-name "next-spec-day.el" prelude-personal-dir))

(provide 'org-mode)
;;; org-mode.el ends here
