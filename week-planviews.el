;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file provides functions that support my weekly planning/reviewing
;;
;; TODO
;; - Add support for copying over my time logging data from spreadsheet to the
;;   PLAN/REVIEW sections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-week-file (&optional week)
  (interactive)
  (if week
      (setq week (number-to-string week))
    (setq week (format-time-string "%W")))
  (setq week-file (concat "~/org/planviews/weeks/" (format-time-string "%Y-w") week ".org"))
  (if (file-exists-p week-file)
      (find-file week-file)
    (find-file week-file)
    (insert (concat (format-time-string "#+TITLE: %Y, Week ") week "\n"))
    (insert-file-contents "~/org/planviews/templates/week.org")))

(defun iso-week-to-date (year week day)
  ;; https://emacs.stackexchange.com/questions/43984/convert-between-iso-week-and-a-normal-date
  (pcase-let ((`(,m ,d ,y)
               (calendar-gregorian-from-absolute
                (calendar-iso-to-absolute (list week day year)))))
    (format-time-string "%F" (encode-time 0 0 0 d m y))))

(defun get-iso-week-journal-files (&optional year week)
  (or year (setq year (string-to-number (format-time-string "%Y"))))
  (or week (setq week (string-to-number (format-time-string "%W"))))
  (setq files (list))
  (dolist (day (number-sequence 1 7))
    (setq file (concat "~/org/journal/" (iso-week-to-date year week day) ".org"))
    (add-to-list 'files file t))
  files)

(defun get-week-file (&optional year week)
  (if year
      (setq year (number-to-string year))
    (setq year (format-time-string "%Y")))
  (if week
      (setq week (number-to-string week))
    (setq week (format-time-string "%W")))
  (concat "~/org/planviews/weeks/" year "-w" week ".org"))

(defun extract-journal-entry-subtree (entry)
  (if (file-exists-p entry)
      (with-temp-buffer
        (insert-file-contents entry)
        (goto-char (point-min))
        (org-mode)
        (org-next-visible-heading 1)
        (org-narrow-to-subtree)
        (org-shiftmetaright)
        (buffer-string))))

(defun copy-journal-entries-to-week-file (&optional year week)
  (interactive)
  (setq week-file (get-week-file year week))
  (setq journal-files (get-iso-week-journal-files year week))
  (set-buffer (find-file-noselect week-file))
  (save-excursion
    (goto-char (point-min))
    (search-forward "* DAILY JOURNAL")
    (org-narrow-to-subtree)
    (setq beg (point))
    (end-of-buffer)
    (delete-region beg (point))
    (newline 2)
    (dolist (entry journal-files)
      (if (file-exists-p entry)
          (insert (extract-journal-entry-subtree entry) "\n")))
    (org-toggle-narrow-to-subtree)
    (save-buffer)))
