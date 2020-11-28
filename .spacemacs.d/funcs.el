;; this module contains only definitions--no stateful actions.

;; TODO: make version of nics-counsel-load-theme for fonts. use ivy-read to select a font. select from (x-list-fonts _regex nil (selected-frame)) constrained to names containing normal-normal-normal, mono, and nerd. use set-frame-font to set the font.

;;; utils

(defun %chg (a b) (* 100 (/ (- b a) a)))

(defmacro cmd (&rest c) "transform a function into a command by prefixing its body with `interactive'. note that this command will not have a name; it'll just say \"lambda\"" `(lambda () (interactive) ,@c))

;;; timer

(defun timer-bell () (call-process
                      (car (file-expand-wildcards "/nix/store/*-user-environment/bin/cvlc"))
                      nil nil nil "--play-and-exit" "/home/nic/programming/op_finished.wav"))

(defun toggle-timer-bell nil
  (interactive)
  (if (boundp 'org-timer-done-hook)
      (if (memql 'timer-bell org-timer-done-hook)
          (progn (message "removed \"timer done\" bell") (remove-hook 'org-timer-done-hook 'timer-bell))
        (progn (message "added \"timer done\" bell") (add-hook 'org-timer-done-hook 'timer-bell)))
    (error "org-timer-done-hook does not exist. you need to start a timer first")))

(defun toggle-cursor-blink nil
  (interactive)
  (setq blink-cursor-mode (if (equal 0 (blink-cursor-mode)) t 0)))

;;; other hooks & toggles

(defun line-numbers-on () (interactive) (display-line-numbers-mode))

;;; commands

;; better version of counsel-load-theme: shows only themes from dotspacemacs-themes
(defun nics-counsel-load-theme ()
  "variant of `counsel-load-theme'. uses dotspacemacs-themes instead of (custom-available-themes)"
  (interactive)
  (ivy-read "Load one of nic's favorite themes: "
            (mapcar 'symbol-name dotspacemacs-themes)
            :action #'counsel-load-theme-action
            :caller 'nics-counsel-load-theme))

;;; functions to be used in the buffer

(defun try-theme () ;; nb see http://ergoemacs.org/emacs/elisp_buffer_string.html for similar functions
  (interactive)
  (counsel-load-theme-action (current-word)))

(defun save-modified-and-close-buffer ()
  (interactive)
  (when (buffer-modified-p) (save-buffer))
  (funcall 'spacemacs/kill-this-buffer))

;; NB: word boundary regex may need some tweaking
(evil-define-motion vile-goto-word-by-first-letter (count char)
  "Move to the next COUNT'th occurrence of CHAR.
Movement is restricted to the current line unless `evil-cross-lines' is non-nil.
Slightly modified version of evil-find-char."
  :type inclusive
  (interactive "<c><C>")
  (setq count (or count 1))
  (let ((fwd (> count 0))
        (visual (and evil-respect-visual-line-mode
                     visual-line-mode)))
    (setq evil-last-find (list #'vile-goto-word-by-first-letter char fwd))
    (when fwd (forward-char))
    (let ((case-fold-search nil))
      (unless (prog1
                  (re-search-forward (concat "[^[:alnum:]]" (char-to-string char)) ; modified this line
                                     (cond (evil-cross-lines
                                            nil)
                                           ((and fwd visual)
                                            (save-excursion
                                              (end-of-visual-line)
                                              (point)))
                                           (fwd
                                            (line-end-position))
                                           (visual
                                            (save-excursion
                                              (beginning-of-visual-line)
                                              (point)))
                                           (t
                                            (line-beginning-position)))
                                     t count)
                (when fwd (backward-char)))
        (user-error "Can't find %c" char)))))

;; backwards version
(evil-define-motion vile-goto-word-by-first-letter-backward (count char)
  "Move to the previous COUNT'th occurrence of CHAR."
  :type exclusive
  (interactive "<c><C>")
  (vile-goto-word-by-first-letter (- (or count 1)) char)
  (forward-char 1))

;;; specific functions

(defun open-typed-racket-docs () ;; NOTE: this URL is incorrect on nixos
  (commandp)
  (eww-browse-url "file:///usr/share/doc/racket/ts-reference/index.html"))

(defun keymap+ (&rest bindings)
  (if (stringp (car bindings))
      (progn (setq k (pop bindings) f (pop bindings))
             (while k
               (global-set-key (kbd k) f)
               (setq k (pop bindings) f (pop bindings))))
    (let ((m (pop bindings)))
      (setq k (pop bindings) f (pop bindings))
      (while k
        (define-key m (kbd k) f)
        (setq k (pop bindings) f (pop bindings))))))

(provide 'funcs)
