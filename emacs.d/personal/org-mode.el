;;; package --- Summary

;;; Commentary:

;;; Code:

(defun mrb/is-project-p ()
  "This function returns true if the entry is considered a project.
   A project is defined to be:
   - does not have a keyword itself;
   - having at least one todo entry, regardless of their state
   - is tagged as :project:"
  (let ((has-subtask)
        (subtree-end (save-excursion (org-end-of-subtree t)))
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1))
        (has-project-tag (member "project" (org-get-tags))))
    (save-excursion
      (forward-line 1)
      (while (and (not has-subtask)
                  (< (point) subtree-end)
                  (re-search-forward "^\*+ " subtree-end t))
        (when (member (org-get-todo-state) org-todo-keywords-1)
          (setq has-subtask t))))
    (and (not is-a-task) has-subtask has-project-tag)
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
(setq org-default-notes-file "~/Org/gtd/refile.org")
(setq org-agenda-files (quote ("~/Org"
                               "~/Org/prog"
                               "~/Org/gtd"
                               )))

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))

(add-to-list 'org-tags-exclude-from-inheritance (quote "project"))

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
              )))

(setq org-agenda-compact-blocks t)
(setq org-agenda-span 'day)
(setq org-agenda-skip-deadline-prewarning-if-scheduled t)
(setq org-hide-leading-stars t)
(setq org-agenda-skip-deadline-if-done t)

;; Breadcrumbs example to display parent entry info
;; Eg: gtd:         [ ACL ] TODO Get IR35 insurance                            :gtd::

;; ("w" "Week Review"
;;                ((tags-todo "+gtd/!TODO"
;;                            ((org-agenda-overriding-header "Someday")
;;                             (org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
;;                                                              (timeline . "  % s")
;;                                                              (todo .
;;                                                                    " %i %-12:c %(concat \"[ \"(org-format-outline-path (org-get-outline-path)) \" ]\") ")
;;                                                              (tags .
;;                                                                    " %i %-12:c %(concat \"[ \"(org-format-outline-path (org-get-outline-path)) \" ]\") ")
;;                                                              (search . " %i %-12:c"))
;;                                                       )
;;                             (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
;;                 )
;;                nil)

;; Example of using a custom func to display an org entry level as an arrow
;; Eg:   ──► TODO Catch up with Kharlamov                                        :gtd::

;; (defun my-agenda-prefix ()
;;   (format "%s" (my-agenda-indent-string (org-current-level))))

;; (defun my-agenda-indent-string (level)
;;   (if (= level 1)
;;       ""
;;     (let ((str ""))
;;       (while (> level 2)
;;         (setq level (1- level)
;;               str (concat str "──")))
;;       (concat str "►"))))

;; (setq org-agenda-custom-commands
;;       '(("c" "My TODOs"
;;          ((tags-todo "mytag"
;;                      ((org-agenda-prefix-format " %e %(my-agenda-prefix) ")
;;                       (org-tags-match-list-sublevels t)))))))

;; Custom agenda command definitions
(setq org-agenda-custom-commands
      (quote ((" " "Agenda"
               ((agenda ""
                        ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("HOLD")))))
                (tags-todo "+gtd-delegated/!NEXT"
                           ((org-agenda-overriding-header "Next")
                            (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))
                            ))
                (tags "+gtd-nonproj"
                      ((org-agenda-overriding-header "Stuck Projects")
                       (org-agenda-skip-function 'aa/skip-non-stuck-projects)
                       ; (org-tags-match-list-sublevels t)
                       ))
                (tags-todo "/!HOLD"
                           ((org-agenda-overriding-header "On Hold")
                            ))
                (tags-todo "+delegated"
                           ((org-agenda-overriding-header "Delegated")
                            ))
                (tags "+refile"
                      ((org-agenda-overriding-header "Refile")
                       ))
                (tags-todo "-gtd-refile/!"
                           ((org-agenda-overriding-header "Notes Follow Up")
                            ))
                )
               nil)
              ("w" "Week Review"
               ((agenda ""
                        ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("HOLD")))
                         (org-agenda-span 10)
                         (org-agenda-start-on-weekday nil)
                         (org-agenda-start-day "-3d")))
                (tags-todo "+gtd/!TODO"
                           ((org-agenda-overriding-header "Someday")
                            (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
                (tags "+gtd-nonproj"
                      ((org-agenda-overriding-header "Stuck Projects")
                       (org-agenda-skip-function 'aa/skip-non-stuck-projects)
                                        ; (org-tags-match-list-sublevels t)
                       ))
                )
               nil)
              )))

;; Font coloring in code blocks
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

(load (expand-file-name "next-spec-day.el" prelude-personal-dir))

;;; Mobile-org

;; Set to the location of your Org files on your local system
;; (setq org-directory "~/Org")
;; ;; Set to the name of the file where new notes will be stored
;; (setq org-mobile-inbox-for-pull "~/org/flagged.org")
;; ;; Set to <your Dropbox root directory>/MobileOrg.
;; (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

(provide 'org-mode)
;;; org-mode.el ends here
