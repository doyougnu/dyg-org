;;; config.el --- custom org Layer Configuration File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Jeffrey Young (doyougnu) <youngjef@oregonstate.edu>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;;;;;;;;;;;;;;;;;;;;;;;;; Org General Config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this is the stuff that would normally be wrapped in a :variables. I like
;; keeping it grouped

(setf org-want-todo-bindings         t)
(setf org-enforce-todo-dependencies  t)
(setf org-clock-persist              'history)
(setf org-clock-idle-time            15)
(setf org-cycle-separator-lines      1)
(setf org-enable-github-support      t)
(setf org-src-tab-acts-natively      t)
(setf org-projectile-file            "project.org")
(setf org-enable-org-journal-support t)
(setf org-journal-enable-cache       t)
(setf org-journal-file-type          'weekly)
(setf org-journal-dir                "~/sync/org/journal")
(setf org-journal-file-format        "%Y/%m/%d.org")
(setf org-columns-default-format     "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")
(setf org-journal-enable-agenda-integration t)
(setf org-journal-carryover-items "TODO=\"NEXT\"|TODO=\"TODO\"|TODO=\"HOLD\"|TODO=\"INPROG\"")
(setf org-confirm-babel-evaluate       nil)
(setf org-edit-src-content-indentation 2)
(setf org-use-fast-todo-selection      t)
(setf org-refile-use-outline-path      t)

;; don't set bookmarks on a capture
(setf org-capture-bookmark             nil)
;; don't fontify the refile created bookmarks
(setf bookmark-fontify                 nil)

; Exclude DONE state tasks from refile targets
(setf org-refile-target-verify-function 'dyg/verify-refile-target)

;; never split headlines
(setf org-M-RET-may-split-line           nil)
(setf org-insert-heading-respect-content 'expert)

;;;;;;;;;;;;;;;;;;;;;;;;; Org Agenda Config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; orgmode organization stuff
(setq-default org-default-todo-file "~/sync/org/refile.org")

(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                          (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "MEETING")))


(setq org-agenda-files '("~/sync/org/research.org"
                         "~/sync/org/meetings.org"
                         "~/sync/org/projects.org"
                         "~/sync/org/refile.org"))


(setq org-tag-alist '(("research"     . ?r)
                      ("job"          . ?j)
                      ("dnd"          . ?d)
                      ("chores"       . ?c)
                      ("side-project" . ?s)
                      ("longterm"     . ?l)))


(setq org-todo-state-tags-triggers
      '(("CANCELLED" ("CANCELLED" . t))
        ("WAITING" ("WAITING" . t))
        ("HOLD" ("WAITING") ("HOLD" . t))
        (done ("WAITING") ("HOLD"))
        ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
        ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
        ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))

(setq org-agenda-compact-blocks t)

;; Custom agenda command definitions
(setq org-agenda-custom-commands
      '((" " "Agenda"
        ((agenda "" nil)
         (tags "REFILE"
               ((org-agenda-overriding-header "Tasks to Refile")
                (org-tags-match-list-sublevels nil)))
         (tags-todo "-CANCELLED/!"
                    ((org-agenda-overriding-header "Stuck Projects")
                     (org-agenda-skip-function     'dyg/skip-non-stuck-projects)
                     (org-agenda-sorting-strategy  '(category-keep))))
         (tags-todo "-HOLD-CANCELLED/!"
                    ((org-agenda-overriding-header "Projects")
                     (org-agenda-skip-function      'dyg/skip-non-projects)
                     (org-tags-match-list-sublevels 'indented)
                     (org-agenda-sorting-strategy   '(category-keep))))
         (tags-todo "-CANCELLED/!NEXT"
                    ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                           (if dyg/hide-scheduled-and-waiting-next-tasks
                                                               ""
                                                             " (including WAITING and SCHEDULED tasks)")))
                     (org-agenda-skip-function 'dyg/skip-projects-and-habits-and-single-tasks)
                     (org-tags-match-list-sublevels t)
                     (org-agenda-todo-ignore-scheduled dyg/hide-scheduled-and-waiting-next-tasks)
                     (org-agenda-todo-ignore-deadlines dyg/hide-scheduled-and-waiting-next-tasks)
                     (org-agenda-todo-ignore-with-date dyg/hide-scheduled-and-waiting-next-tasks)
                     (org-agenda-sorting-strategy      '(todo-state-down effort-up category-keep))))
         (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                    ((org-agenda-overriding-header (concat "Project Subtasks"
                                                           (if dyg/hide-scheduled-and-waiting-next-tasks
                                                               ""
                                                             " (including WAITING and SCHEDULED tasks)")))
                     (org-agenda-skip-function 'dyg/skip-non-project-tasks)
                     (org-agenda-todo-ignore-scheduled dyg/hide-scheduled-and-waiting-next-tasks)
                     (org-agenda-todo-ignore-deadlines dyg/hide-scheduled-and-waiting-next-tasks)
                     (org-agenda-todo-ignore-with-date dyg/hide-scheduled-and-waiting-next-tasks)
                     (org-agenda-sorting-strategy      '(category-keep))))
         (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                    ((org-agenda-overriding-header (concat "Standalone Tasks"
                                                           (if dyg/hide-scheduled-and-waiting-next-tasks
                                                               ""
                                                             " (including WAITING and SCHEDULED tasks)")))
                     (org-agenda-skip-function 'dyg/skip-project-tasks)
                     (org-agenda-todo-ignore-scheduled dyg/hide-scheduled-and-waiting-next-tasks)
                     (org-agenda-todo-ignore-deadlines dyg/hide-scheduled-and-waiting-next-tasks)
                     (org-agenda-todo-ignore-with-date dyg/hide-scheduled-and-waiting-next-tasks)
                     (org-agenda-sorting-strategy      '(category-keep))))
         (tags-todo "-CANCELLED+WAITING|HOLD/!"
                    ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
                                                           (if dyg/hide-scheduled-and-waiting-next-tasks
                                                               ""
                                                             " (including WAITING and SCHEDULED tasks)")))
                     (org-agenda-skip-function      'dyg/skip-non-tasks)
                     (org-tags-match-list-sublevels nil)
                     (org-agenda-todo-ignore-scheduled dyg/hide-scheduled-and-waiting-next-tasks)
                     (org-agenda-todo-ignore-deadlines dyg/hide-scheduled-and-waiting-next-tasks)))
         (tags "-REFILE/"
               ((org-agenda-overriding-header "Tasks to Archive")
                (org-agenda-skip-function 'dyg/skip-non-archivable-tasks)
                (org-tags-match-list-sublevels nil))))
        nil)))

;;;;;;;;;;;;;;;;;;;;;;;;; Org Capture Config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-capture-templates
      '(("t" "todo" entry (file org-default-todo-file)
         "* TODO %?\n - Todo made on %U \\\\ \n" :clock-in t :clock-resume t)
        ("r" "respond" entry (file org-default-todo-file)
         "* NEXT Respond to %^{Prompt}%:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
        ("n" "note" entry (file org-default-todo-file)
         "* %? :NOTE:\n - Note taken on %U \\\\ \n" :clock-in t :clock-resume t)
        ("i" "idea" entry (file org-default-todo-file)
         "* %? :IDEA:\n - Idea taken on %U \\\\ \n" :clock-in t :clock-resume t)
        ("m" "meeting" entry (file org-default-todo-file)
         "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
        ))


(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

(add-hook 'org-clock-out-hook 'dyg/remove-empty-drawer-on-clock-out 'append)


;;;;;;;;;;;;;;;;;;;;;;;;; Org Publish Config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-publish-project-alist
      '(("org-notes"
         :base-directory "~/Programming/blog/orgblog"
         :base-extension "org"
         :publishing-directory "~/Programming/blog/public_html/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4         ; Just the default for this project.
         :auto-preamble t)

        ("org-static"
         :base-directory "~/Programming/blog/orgblog"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "~/Programming/blog/public_html/"
         :recursive t
         :publishing-function org-publish-attachment)

        ("orgblog" :components ("org-notes" "org-static"))))
