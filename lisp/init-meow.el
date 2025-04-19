;;; init-meow.el -*- lexical-binding: t; -*-

;;; Log:
;; 2025-03-31:
;;   + format code
;;   + add wrap-keymap for insert quote/bracket/brace/parenthesis pair in normal state
;; 2025-04-06:
;;   + add `emt' and `meow-mark-word-or-chinese'

;;; Code:

;; emt: Emacs MacOS Tokenizer
(use-package emt
  :straight (:host github :repo "roife/emt"
                   :files ("*.el" "module/*" "module"))
  :hook (after-init . emt-mode))

;; https://github.com/LuciusChen/.emacs.d/blob/61241953d3cf1e5b4d1ca0559717d8a55b12543c/lib/lib-meow.el#L5-L41
(defun meow-mark-word-or-chinese (n)
  "Mark current word under cursor, handling both English and Chinese text.

This function uses EMT's segmentation for Chinese and default behavior for English.
The selection will be expandable with `meow-next-word' and `meow-back-word'.
The selected word will be added to `regexp-search-ring' and highlighted.

Use a negative argument to create a backward selection."
  (interactive "p")
  ;; Ensure that EMT is loaded
  (emt-ensure)
  (let* ((direction (if (< n 0) 'backward 'forward))
         (bounds (emt--get-bounds-at-point
                  (emt--move-by-word-decide-bounds-direction direction)))
         (beg (car bounds))
         (end (cdr bounds)))
    (if (eq beg end)
        ;; Use default Meow for English words
        (meow-mark-thing meow-word-thing 'word (< n 0) "\\<%s\\>")
      ;; Use EMT segmentation for Chinese
      (let* ((text (buffer-substring-no-properties beg end))
             (segments (append (emt-split text) nil))
             (pos (- (point) beg))
             (segment-bounds (car segments)))
        ;; Find the correct segment
        (dolist (bound segments)
          (when (and (>= pos (car bound)) (< pos (cdr bound)))
            (setq segment-bounds bound)))
        (when segment-bounds
          (let* ((seg-beg (+ beg (car segment-bounds)))
                 (seg-end (+ beg (cdr segment-bounds)))
                 (segment-text (buffer-substring-no-properties seg-beg seg-end))
                 (regexp (regexp-quote segment-text)))
            (let ((selection (meow--make-selection (cons 'expand 'word) seg-beg seg-end)))
              (meow--select selection (< n 0))
              (meow--push-search regexp)
              (meow--highlight-regexp-in-buffer regexp))))))))

;; meow
(use-package meow
  :ensure t
  ;; :demand t
  :config
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state.
     '("j" . "H-j")
     '("k" . "H-k")
     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     ;; '("w" . meow-mark-word)
     (if *is-a-mac*
	 '("w" . meow-mark-word-or-chinese)
       '("w" . meow-mark-word))
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<escape>" . ignore)))
  (setq wrap-keymap
	(let ((map (make-keymap)))
          (suppress-keymap map)
          (dolist (k '("(" "[" "{" "<" "\""))
            (define-key map k #'insert-pair))
          map))
  (meow-normal-define-key (cons "\\" wrap-keymap))
  (meow-setup)
  (meow-global-mode 1))

(provide 'init-meow)

;;; init-meow.el ends here
