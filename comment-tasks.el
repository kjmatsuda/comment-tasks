;;; comment-tasks.el --- Show tasks in comments in code.

;; Copyright (C) 2018 by Koji Matsuda

;; Author: Koji Matsuda <kjmatsuda1985@gmail.com>
;; URL: https://github.com/kjmatsuda/comment-tasks
;; Version: alpha
;; Package-Requires: ()

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Show tasks in comments in code

;;
;; To use this package, add these lines to your init.el or .emacs file:
;; (require 'comment-tasks)

;;; Code:

(require 'cl)

(defcustom comment-tasks-auto-update t
  "*If non-nil, *Tasks* buffer updated whenever a file is saved."
  :type 'boolean)

(defvar task-comments '())

;; TODO この関数を説明できるようにする
(defun get-comments-in-file (fname)
  (let (already-open
        buf
        start
        comment
        (comments '()))
    (setq already-open (find-buffer-visiting fname)
          buf (find-file-noselect fname t)) ;; 2nd argument no warn
    (with-current-buffer buf
      (goto-char (point-min))
      (while (setq start (text-property-any
                          (point) (point-max)
                          'face 'font-lock-comment-face))
        (goto-char start)
        (goto-char (next-single-char-property-change (point) 'face))
        (setq comment (propertize (buffer-substring-no-properties start (point)) ;; comment text
                                  :path (buffer-file-name) ;; file path
                                  :point start             ;; position of comment 
                                  ))
        (setq comments (append comments (list comment)))))
    (unless already-open (kill-buffer buf))
    comments))

(defun filter-task-comments (comments)
  (let (task-comments '())
    (loop for comment in comments
          do (if (string-match "TODO" comment)
                 (setq task-comments (append task-comments (list comment)))))
    task-comments))

(defun comment-tasks-action-goto-entry (event)
  "Goto the entry that was clicked."
  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event)))
        (tasks-buffer (get-buffer "*Tasks*")))
    (with-current-buffer tasks-buffer
      (goto-char pos)
      (comment-tasks-goto-entry))
    ))

(defun comment-tasks-goto-entry ()
  "Display task comment position."
  (interactive)
  (let* ((entry (comment-tasks-find-entry))
         (fname (get-text-property 0 :path entry))
         (buf (find-buffer-visiting fname)))
    (if buf
        (pop-to-buffer buf)
      (find-file fname))
    (goto-char (get-text-property 0 :point entry))
    ))

(defun comment-tasks-find-entry ()
  "Find the entry in `task-comments' correspond to the current line."
  (nth (1- (line-number-at-pos)) task-comments))


(defun comment-tasks-show ()
  (interactive)
  ;; TODO 指定したフォルダ以下の指定した拡張子の全ファイルに対して、タスクを探す処理を実行
  (save-excursion
    (setq comments (get-comments-in-file "comment-tasks.el"))
    (setq task-comments (filter-task-comments comments))
    (with-current-buffer (get-buffer-create "*Tasks*")
      (setq-local inhibit-message t)
      (read-only-mode -1) ;; make it writable
      (erase-buffer)
      (loop for task in task-comments
            do (insert-button (format "%s" task)
                     'action #'comment-tasks-action-goto-entry))
      (read-only-mode 1) ;; make it read only
      )
    )
  )

;; TODO autoloadにどういう意味があるか調べる
;;;###autoload
(define-minor-mode comment-tasks-mode ()
  "Enable comment-tasks"
  :init-value nil
  :global     nil
  (if comment-tasks-mode
      (when comment-tasks-auto-update
        (add-hook 'after-save-hook 'comment-tasks-show nil t))
    (when comment-tasks-auto-update
      (remove-hook 'after-save-hook 'comment-tasks-show t))))

(provide 'comment-tasks)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; comment-tasks.el ends here
