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

(defconst comment-tasks-buffer-name "*Tasks*"
  "Name of the buffer which display comment task entries.")

(defvar comment-tasks-list '())

(defgroup comment-tasks nil
  "Variables for `comment-tasks' package."
  :group 'programming)

(defcustom comment-tasks-keyword-list '("TODO")
  "Keyword list of which matching comments are shown on buffer."
  )

(defcustom comment-tasks-auto-update t
  "*If non-nil, comment-tasks buffer updated whenever a file is saved."
  :type 'boolean)
;;REF test

(defcustom comment-tasks-list-size 0.3
  "Size (height or width) for the comment-tasks buffer."
  :group 'comment-tasks
  :type 'number)

(defcustom comment-tasks-list-position 'below
  "Position of the comment-tasks buffer.
Either 'right, 'left, 'above or 'below. This value is passed directly to `split-window'."
  :group 'comment-tasks
  :type '(choice (const above)
                 (const below)
                 (const left)
                 (const right)))

(defface comment-tasks-entry-face
  '((t))
  "Basic face for comment-tasks entries in comment-tasks buffer."
  :group 'comment-tasks)

(defcustom comment-tasks-focus-after-activation nil
  "Non-nil to select the comment-tasks window automatically when
`comment-tasks-minor-mode' is activated."
  :group 'comment-tasks
  :type 'boolean)

;; TODO この関数を説明できるようにする
(defun get-comments-in-file (fname)
  (let (already-open
        buf
        comment
        (comments '()))
    (setq already-open (find-buffer-visiting fname)
          buf (find-file-noselect fname t)) ;; 2nd argument no warn
    (with-current-buffer buf
      (goto-char (point-min))
      (while (comment-search-forward (point-max) t)
        (setq comment (propertize (concat (buffer-substring-no-properties (point) (line-end-position)) "\n") ;; comment text
                                  :path (buffer-file-name) ;; file path
                                  :point (point)             ;; position of comment
                                  ))
        (setq comments (append comments (list comment)))))
    (unless already-open (kill-buffer buf))
    comments))

(defun comment-is-matched-with-keyword-list (comment)
  (let ((matched nil))
    (loop for keyword in comment-tasks-keyword-list
          do (if (string-match keyword comment)
                 (setq matched t))
          )
    matched))

(defun filter-task-comments (comments)
  (let (comment-tasks '())
    (loop for comment in comments
          do (if (comment-is-matched-with-keyword-list comment)
                 (setq comment-tasks (append comment-tasks (list comment)))))
    comment-tasks))

(defun comment-tasks-action-goto-entry (event)
  "Goto the entry that was clicked."
  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event)))
        (tasks-buffer (get-buffer comment-tasks-buffer-name)))
    (with-current-buffer tasks-buffer
      (goto-char pos)
      (comment-tasks-goto-entry))))

(defun comment-tasks-goto-entry ()
  "Display task comment position."
  (interactive)
  (let* ((entry (comment-tasks-find-entry))
         (fname (get-text-property 0 :path entry))
         (buf (find-buffer-visiting fname)))
    (other-window 1)
    (if buf
        (switch-to-buffer buf)
      (switch-to-buffer (find-file-noselect fname t)))
    (goto-char (get-text-property 0 :point entry))))

(defun  comment-tasks-display-entry ()
  (interactive)
  (save-selected-window
    (comment-tasks-goto-entry)))

(defun comment-tasks-find-entry ()
  "Find the entry in `comment-tasks-list' correspond to the current line."
  (nth (1- (line-number-at-pos)) comment-tasks-list))

(defun comment-tasks-split-size ()
  "Convert `comment-tasks-list-size' to proper argument for `split-window'."
  (let ((frame-size (if (member comment-tasks-list-position '(left right))
                        (frame-width)
                      (frame-height))))
    (cond ((integerp comment-tasks-list-size) (- comment-tasks-list-size))
          (t (- (round (* frame-size comment-tasks-list-size)))))))

(defun comment-tasks-display-buffer (buffer alist)
  "Display the comment-tasks buffer at the specified position."
  (or (get-buffer-window buffer)
      (let ((window (ignore-errors (split-window (frame-root-window) (comment-tasks-split-size) comment-tasks-list-position))))
        (when window
          (window--display-buffer buffer window 'window alist t)
          window))))

(defun comment-tasks-install-display-buffer ()
  "Install comment-tasks display settings to `display-buffer-alist'."
  (cl-pushnew `(,(concat "^" (regexp-quote comment-tasks-buffer-name) "$")
                comment-tasks-display-buffer)
              display-buffer-alist
              :test #'equal))

(comment-tasks-install-display-buffer)

(defun comment-tasks-show-select ()
  "Show the comment-tasks buffer, and select it"
  (interactive)
  (pop-to-buffer comment-tasks-buffer-name))

(defun comment-tasks-show-noselect ()
  "Show the comment-tasks buffer, but don't select it.
If the comment-tasks buffer doesn't exist, create it."
  (interactive)
  (display-buffer comment-tasks-buffer-name))

(defun comment-tasks-search-tasks-recursively (dir)
  (let ((files (directory-files dir t)))
    (dolist (file files)
      ;; TODO ループのcontinueの構文を調べる
      (if (not (string= (file-name-nondirectory file) "."))
          (if (not (string= (file-name-nondirectory file) ".."))
              (if (not (string= (file-name-nondirectory file) ".git"))
                  (if (f-directory? file)
                      (comment-tasks-search-tasks-recursively file)
                    (progn
                      (setq comments (get-comments-in-file file))
                      (setq comment-tasks-list (append comment-tasks-list (filter-task-comments comments)))))))))))

;; I want to make this function enable, but it easily costs too much time.
;; So currently I don't recommend this function.
(defun comment-tasks-make-list-recursively ()
  (let ((dir (locate-dominating-file default-directory ".git")))
    (if (not dir)
        ;; root dir not found
        (setq dir default-directory)
      )
    (comment-tasks-search-tasks-recursively dir)))

(defun comment-tasks-make-list ()
  (setq comment-tasks-list (append comment-tasks-list
                                   (filter-task-comments (get-comments-in-file (buffer-file-name))))))

;; TODO comment-tasks-showとの重複を除く
(defun comment-tasks-update-buffer ()
  (interactive)
  (save-excursion
    (setq comment-tasks-list '())
    (comment-tasks-make-list)
    (with-current-buffer (get-buffer-create comment-tasks-buffer-name)
      (comment-tasks-major-mode)
      (setq-local inhibit-message t)  ;; view-modeに入ったときのメッセージを抑止する
      (read-only-mode -1) ;; make it writable
      (erase-buffer)
      (loop for task in comment-tasks-list
            do (insert-button (format "%s" task)
                     'face 'comment-tasks-entry-face
                     'action #'comment-tasks-action-goto-entry))
      (goto-char (point-min))
      (read-only-mode 1) ;; make it read only
      (comment-tasks-show-noselect))))


(defun comment-tasks-show ()
  (interactive)
  (save-excursion
    (setq comment-tasks-list '())
    (comment-tasks-make-list)
    (with-current-buffer (get-buffer-create comment-tasks-buffer-name)
      (comment-tasks-major-mode)
      (setq-local inhibit-message t)  ;; view-modeに入ったときのメッセージを抑止する
      (read-only-mode -1) ;; make it writable
      (erase-buffer)
      (loop for task in comment-tasks-list
            do (insert-button (format "%s" task)
                     'face 'comment-tasks-entry-face
                     'action #'comment-tasks-action-goto-entry))
      (goto-char (point-min))
      (read-only-mode 1) ;; make it read only
      (if comment-tasks-focus-after-activation
          (comment-tasks-show-select)
        (comment-tasks-show-noselect)))))

(defun comment-tasks-quit-window ()
  "Disable `comment-tasks-minor-mode' and hide the comment-tasks buffer.
If `comment-tasks-minor-mode' is already disabled, just call `quit-window'."
  (interactive)
  (if comment-tasks-minor-mode
      ;; disabling `comment-tasks-minor-mode' also quits the window
      (comment-tasks-minor-mode -1)
    (quit-window)))

(defvar comment-tasks-major-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'comment-tasks-goto-entry)
    (define-key map (kbd "C-j") #'comment-tasks-display-entry)
    (define-key map (kbd "j") #'next-line)
    (define-key map (kbd "k") #'previous-line)
    (define-key map (kbd "q") #'comment-tasks-quit-window)
    map))

(define-derived-mode comment-tasks-major-mode special-mode "Tasks")

;; TODO autoloadにどういう意味があるか調べる
;;;###autoload
(define-minor-mode comment-tasks-minor-mode
  nil :global nil
  (if comment-tasks-minor-mode
      (progn
        (when comment-tasks-auto-update
          (add-hook 'after-save-hook 'comment-tasks-update-buffer nil t))
        (comment-tasks-show))
    (when comment-tasks-auto-update
      (remove-hook 'after-save-hook 'comment-tasks-update-buffer t))
    (ignore-errors (quit-windows-on comment-tasks-buffer-name))
    (when (get-buffer comment-tasks-buffer-name)
      (bury-buffer (get-buffer comment-tasks-buffer-name)))))

(provide 'comment-tasks)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; comment-tasks.el ends here
