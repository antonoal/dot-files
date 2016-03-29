;;; package --- Summary

;;; Commentary:

;;; Code:

(defun mrb/is-project-p ()
  "This function returns true if the entry is considered a project.
   A project is defined to be:
   - does not have a keyword itself;
   - having at least one todo entry, regardless of their state."
  (let ((has-subtask)
        (subtree-end (save-excursion (org-end-of-subtree t)))
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)
                   ))
    (save-excursion
      (forward-line 1)
      (while (and (not has-subtask)
                  (< (point) subtree-end)
                  (re-search-forward "^\*+ " subtree-end t))
        (when (member (org-get-todo-state) org-todo-keywords-1)
          (setq has-subtask t))))
    (and (not is-a-task) has-subtask)
    )
  )

(defun aa/has-active-task (subtree-end)
  "Returns t if current task has got an active task. A task is active if
   - it is a NEXT or HOLD; or
   - it is a TODO with a DEADLINE or SCHEDULED timestamp"
  (let ((result))
    (save-excursion
      (while (and (not result)
                  (< (point) subtree-end)
                  (re-search-forward "^\*+ " subtree-end t))
        (let ((todo-state (org-get-todo-state)))
          (when (or (and (equal "TODO" todo-state)
                         (or (org-get-scheduled-time (point))
                             (org-get-deadline-time (point))))
                    (member todo-state '("NEXT" "HOLD")))
            (setq result t)
           ))
        )
      result))
  )

(defun aa/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
         ;; FIXME this should be done easier - going to the next (sub-)item if any or end-of-subtree
         (next-item (save-excursion
                      (let ((a (re-search-forward "^\*+ " subtree-end t)))
                        (if (eq a subtree-end) subtree-end (re-search-backward "^")))))
         (has-next (save-excursion
                     (forward-line 1)
                     (and (< (point) subtree-end)
                          (aa/has-active-task subtree-end)
                          ))))
    (if (and (mrb/is-project-p) (not has-next))
        nil ; a stuck project, has subtasks but no next task
      next-item)))

(defun mrb/skip-non-projects ()
  "Skip trees that are not projects"
  (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (mrb/is-project-p)
        nil
      subtree-end)))

(defun mrb/skip-projects ()
  "Skip trees that are projects"
  (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (mrb/is-project-p)
        subtree-end
      nil)))


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
               ((agenda ""
                        ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("HOLD")))))
                (tags "minddump"
                      ((org-agenda-overriding-header "Mind Dump")
                       ))
                (tags-todo "-SCHEDULED={.+}-DEADLINE={.+}/!NEXT"
                           ((org-agenda-overriding-header "Next")
                            ))
                ;; (tags-todo "/!NEXT"
                ;;            ((org-agenda-overriding-header "Next")
                ;;             (org-tags-match-list-sublevels t)
                ;;             (org-agenda-todo-ignore-scheduled t)
                ;;             (org-agenda-todo-ignore-deadlines t)
                ;;             ))
                (tags "+gtd-nonproj"
                      ((org-agenda-overriding-header "Stuck Projects")
                       (org-agenda-skip-function 'aa/skip-non-stuck-projects)
                       ; (org-tags-match-list-sublevels t)
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
               ((tags-todo "+gtd/!-NEXT-HOLD"
                           ((org-agenda-overriding-header "Someday")
                            (org-agenda-todo-ignore-scheduled t)
                            (org-agenda-todo-ignore-deadlines t)
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
