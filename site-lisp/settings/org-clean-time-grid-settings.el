;; * Remove time grid lines that are in an appointment
;; The agenda shows lines for the time grid.  Some people think that
;; these lines are a distraction when there are appointments at those
;; times.  You can get rid of the lines which coincide exactly with the
;; beginning of an appointment.  Michael Ekstrand has written a piece of
;; advice that also removes lines that are somewhere inside an
;; appointment:

(defun org-time-to-minutes (time)
  "Convert an HHMM time to minutes"
  (+ (* (/ time 100) 60) (% time 100)))

(defun org-time-from-minutes (minutes)
  "Convert a number of minutes to an HHMM time"
  (+ (* (/ minutes 60) 100) (% minutes 60)))

(defadvice org-agenda-add-time-grid-maybe (around mde-org-agenda-grid-tweakify
                                                  (list ndays todayp))
  (if (member 'remove-match (car org-agenda-time-grid))
      (flet ((extract-window
              (line)
              (let ((start (get-text-property 1 'time-of-day line))
                    (dur (get-text-property 1 'duration line)))
                (cond
                 ((and start dur)
                  (cons start
                        (org-time-from-minutes
                         (+ dur (org-time-to-minutes start)))))
                 (start start)
                 (t nil)))))
        (let* ((windows (delq nil (mapcar 'extract-window list)))
               (org-agenda-time-grid
                (list (car org-agenda-time-grid)
                      (cadr org-agenda-time-grid)
                      (remove-if
                       (lambda (time)
                         (find-if (lambda (w)
                                    (if (numberp w)
                                        (equal w time)
                                      (and (>= time (car w))
                                           (< time (cdr w)))))
                                  windows))
                       (caddr org-agenda-time-grid)))))
          ad-do-it))
    ad-do-it))
(ad-activate 'org-agenda-add-time-grid-maybe)
