;;; dotemacs-notebook.el --- My GNU Emacs configuration
;;
;; Copyright (c) 2012-2015 Mosè Giordano
;;
;; Author: Mosè Giordano

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Notebook related customizations.

;;; License:

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING. If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;; USA.

;;; Code:

(eval-after-load "battery"
  '(progn
     (setq battery-mode-line-format " [Battery: %p%% %b. AC: %L. ETA: %t]"
	   battery-load-low 20)
     (when (and (eq system-type 'gnu/linux)
		(file-readable-p "/sys/")
		(battery--find-linux-sysfs-batteries))
       (defun mg-battery-linux-sysfs ()
	 "Improved version of original `battery-linux-sysfs'.

Get ACPI status information from Linux kernel.
This function works only with the new `/sys/class/power_supply/'
format introduced in Linux version 2.4.25.

The following %-sequences are provided:
%c Current capacity (mAh or mWh)
%r Current rate
%B Battery status (verbose)
%d Temperature (in degrees Celsius)
%b Battery status: empty means high, `-' means low,
   `!' means critical, and `+' means charging
%p Battery load percentage
%L AC line status (verbose)
%m Remaining time (to charge or discharge) in minutes
%h Remaining time (to charge or discharge) in hours
%t Remaining time (to charge or discharge) in the form `h:min'"
	 (let (charging-state rate temperature hours
			      (charge-full 0.0)
			      (charge-now 0.0)
			      (energy-full 0.0)
			      (energy-now 0.0)
			      (capacity 0.0))
	   ;; SysFS provides information about each battery present in the
	   ;; system in a separate subdirectory.  We are going to merge the
	   ;; available information together.
	   (with-temp-buffer
	     (dolist (dir (ignore-errors
			    (directory-files
			     "/sys/class/power_supply/" t
			     battery--linux-sysfs-regexp)))
	       (erase-buffer)
	       (ignore-errors (insert-file-contents
			       (expand-file-name "uevent" dir)))
	       (when (re-search-forward "POWER_SUPPLY_PRESENT=1$" nil t)
		 (goto-char (point-min))
		 (and (re-search-forward "POWER_SUPPLY_STATUS=\\(.*\\)$" nil t)
		      (member charging-state '("Unknown" "Full" nil))
		      (setq charging-state (match-string 1)))
		 (when (re-search-forward
			"POWER_SUPPLY_\\(CURRENT\\|POWER\\)_NOW=\\([0-9]*\\)$"
			nil t)
		   (setq rate (float (string-to-number (match-string 2)))))
		 (when (re-search-forward "POWER_SUPPLY_TEMP=\\([0-9]*\\)$" nil t)
		   (setq temperature (match-string 1)))
		 (let (full-string now-string)
		   ;; Sysfs may list either charge (mAh) or energy (mWh).
		   ;; Keep track of both, and choose which to report later.
		   (cond ((and (re-search-forward
				"POWER_SUPPLY_CHARGE_FULL=\\([0-9]*\\)$" nil t)
			       (setq full-string (match-string 1))
			       (re-search-forward
				"POWER_SUPPLY_CHARGE_NOW=\\([0-9]*\\)$" nil t)
			       (setq now-string (match-string 1)))
			  (setq charge-full (+ charge-full
					       (string-to-number full-string))
				charge-now  (+ charge-now
					       (string-to-number now-string))
				hours (and (not (zerop rate)) (/ charge-now rate))
				capacity (and (> charge-full 0)
					      (> charge-now 0)
					      (/ (* 100 charge-now) charge-full))))
			 ((and (re-search-forward
				"POWER_SUPPLY_ENERGY_FULL=\\([0-9]*\\)$" nil t)
			       (setq full-string (match-string 1))
			       (re-search-forward
				"POWER_SUPPLY_ENERGY_NOW=\\([0-9]*\\)$" nil t)
			       (setq now-string (match-string 1)))
			  (setq energy-full (+ energy-full
					       (string-to-number full-string))
				energy-now  (+ energy-now
					       (string-to-number now-string))
				capacity (and (> energy-full 0)
					      (/ (* 100 energy-now) energy-full)))
			  (goto-char (point-min))
			  (and energy-now rate (not (zerop rate))
			       (re-search-forward
				"POWER_SUPPLY_VOLTAGE_NOW=\\([0-9]*\\)$" nil t)
			       (let ((remaining (if (string= charging-state "Discharging")
						    energy-now
						  (- energy-full energy-now))))
				 (setq hours (/ (/ (* remaining (string-to-number
								 (match-string 1)))
						   rate)
						10000000.0))))))))))

	   (list (cons ?c (cond ((or (> charge-full 0) (> charge-now 0))
				 (number-to-string charge-now))
				((or (> energy-full 0) (> energy-now 0))
				 (number-to-string energy-now))
				(t "N/A")))
		 (cons ?r (if rate (format "%.1f" (/ rate 1000000.0)) "N/A"))
		 (cons ?m (if hours (format "%d" (* hours 60)) "N/A"))
		 (cons ?h (if hours (format "%d" hours) "N/A"))
		 (cons ?t (if hours
			      (format "%d:%02d" hours (* (- hours (floor hours)) 60))
			    "N/A"))
		 (cons ?d (or temperature "N/A"))
		 (cons ?B (or charging-state "N/A"))
		 (cons ?b (or (and (string= charging-state "Charging") "+")
			      (and capacity (< capacity battery-load-critical) "!")
			      (and capacity (< capacity battery-load-low) "-")
			      ""))
		 (cons ?p (if capacity
			      (format "%.1f" capacity)
			    "N/A"))
		 (cons ?L (if (file-readable-p "/sys/class/power_supply/AC/online")
			      (if (battery-search-for-one-match-in-files
				   (list "/sys/class/power_supply/AC/online"
					 "/sys/class/power_supply/ACAD/online")
				   "1" 0)
				  "AC"
				"BAT")
			    "N/A")))))
       (setq battery-status-function 'battery-linux-sysfs))))
(display-battery-mode 1)

;;; dotemacs-notebook.el ends here
