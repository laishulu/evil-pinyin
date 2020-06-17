;;; evil-pinyin.el --- Evil search Chinese characters by pinyin -*- lexical-binding: t; -*-

;; URL: https://github.com/laishulu/evil-pinyin
;; Created: June 17th, 2020
;; Keywords: extensions
;; Package-Requires: ((emacs "25") (names "0.5") (evil "1") (pinyinlib "0.1.1"))
;; Version: 1.0

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; This package provide modes to extend evil with ability to search or find
;; Chinese characters by pinyin. For more information see the README in the
;; GitHub repo.

;;; Code:

;; `define-namespace' is autoloaded, so there's no need to require `names'.
;; However, requiring it here means it will also work for people who don't
;; install through package.el.
(eval-when-compile (require 'names) (require 'evil) (require 'pinyinlib))

(define-namespace evil-pinyin-

(defvar with-find-char t
  "Enable the /find char/ feature.")

(defvar with-search t
  "Enable the /search/ feature.")

(defvar with-traditional nil
  "Include traditional Chinese.")

(defvar with-punctuation t
  "Include Chinese punctuation.")

(defun -build-regexp (thing)
  "Build regexp form THING for search."
  (cond

   ((integerp thing)
    (pinyinlib-build-regexp-char
     thing (not with-punctuation) with-traditional))

   ((stringp thing)
    (pinyinlib-build-regexp-string
     thing (not with-punctuation) with-traditional))))

(evil-define-motion find-char (count char)
  "Move to the next COUNT'th occurrence of CHAR."
  :type inclusive
  (interactive "<c><C>")
  (setq count (or count 1))
  (let ((fwd (> count 0)))
    (setq evil-last-find (list #'evil-pinyin-find-char char fwd))
    (when fwd (forward-char))
    (let ((case-fold-search nil))
      (unless
          (prog1
              (search-forward-regexp
               (-build-regexp char)
               (unless evil-cross-lines
                 (if fwd
                     (line-end-position)
                   (line-beginning-position)))
               t count)
            (when fwd (backward-char)))
        (user-error "Can't find %c" char)))))

(evil-define-motion find-char-backward (count char)
  "Move to the previous COUNT'th occurrence of CHAR."
  :type exclusive
  (interactive "<c><C>")
  (find-char (- (or count 1)) char))

(evil-define-motion find-char-to (count char)
  "Move before the next COUNT'th occurrence of CHAR."
  :type inclusive
  (interactive "<c><C>")
  (unwind-protect
      (progn
        (find-char count char)
        (if (> (or count 1) 0)
            (backward-char)
          (forward-char)))
    (setcar evil-last-find #'evil-pinyin-find-char-to)))

(evil-define-motion find-char-to-backward (count char)
  "Move before the previous COUNT'th occurrence of CHAR."
  :type exclusive
  (interactive "<c><C>")
  (find-char-to (- (or count 1)) char))

(evil-define-motion repeat-find-char (count)
  "Repeat the last find COUNT times."
  :type inclusive
  (setq count (or count 1))
  (if evil-last-find
      (let ((cmd (car evil-last-find))
            (char (nth 1 evil-last-find))
            (fwd (nth 2 evil-last-find))
            evil-last-find)
        ;; ensure count is non-negative
        (when (< count 0)
          (setq count (- count)
                fwd (not fwd)))
        ;; skip next character when repeating t or T
        (and (eq cmd #'evil-pinyin-find-char-to)
             evil-repeat-find-to-skip-next
             (= count 1)
             (or (and fwd (or (= (char-after (1+ (point))) char)
                              (string-match-p
                               (-build-regexp char)
                               (string (char-after (1+ (point)))))))
                 (and (not fwd) (or (= (char-before) char)
                                    (string-match-p
                                     (-build-regexp char)
                                     (string (char-before))))))
             (setq count (1+ count)))
        (funcall cmd (if fwd count (- count)) char)
        (unless (nth 2 evil-last-find)
          (setq evil-this-type 'exclusive)))
    (user-error "No previous search")))

(evil-define-motion repeat-find-char-reverse (count)
  "Repeat the last find COUNT times in the opposite direction."
  :type inclusive
  (repeat-find-char (- (or count 1))))

;; ---------------------- ;;
;; evil-snipe integration ;;
;; ---------------------- ;;
(defvar -snipe-def nil)

(defun -snipe-process-key (key)
  "Process the snipe KEY."
  (let ((regex-p (assoc key evil-snipe-aliases))
        (keystr (char-to-string key)))
    (cons keystr
          (if regex-p (elt regex-p 1)
            (-build-regexp keystr)))))


(defun -advice (fn &rest args)
  "Advice for FN with ARGS."
  (-build-regexp (apply fn args)))

;;;###autoload
(define-minor-mode mode
  "Global mode to extend evil with ability to search or find Chinese characters by pinyin."
  :global t
  :init-value nil

  (when with-find-char
    (if mode
        (progn
          (define-key evil-motion-state-map
            [remap evil-find-char]
            'evil-pinyin-find-char)
          (define-key evil-motion-state-map
            [remap evil-find-char-backward]
            'evil-pinyin-find-char-backward)
          (define-key evil-motion-state-map
            [remap evil-find-char-to]
            'evil-pinyin-find-char-to)
          (define-key evil-motion-state-map
            [remap evil-find-char-to-backward]
            'evil-pinyin-find-char-to-backward)
          (define-key evil-motion-state-map
            [remap evil-repeat-find-char]
            'evil-pinyin-repeat-find-char)
          (define-key evil-motion-state-map
            [remap evil-repeat-find-char-reverse]
            'evil-pinyin-repeat-find-char-reverse)
          (when (featurep 'evil-snipe)
            (unless -snipe-def
              (setq -snipe-def (symbol-function 'evil-snipe--process-key)))
            (fset 'evil-snipe--process-key 'evil-pinyin--snipe-process-key))))
    (define-key evil-motion-state-map [remap evil-find-char] nil)
    (define-key evil-motion-state-map [remap evil-find-char-backward] nil)
    (define-key evil-motion-state-map [remap evil-find-char-to] nil)
    (define-key evil-motion-state-map [remap evil-find-char-to-backward] nil)
    (define-key evil-motion-state-map [remap evil-repeat-find-char] nil)
    (define-key evil-motion-state-map [remap evil-repeat-find-char-reverse] nil)
    (when (and (featurep 'evil-snipe) -snipe-def)
      (fset 'evil-snipe--process-key -snipe-def)))

  (when with-search
    (if mode
        (advice-add 'evil-ex-pattern-regex :around
                    #'evil-pinyin--advice)
      (advice-remove 'evil-ex-pattern-regex
                     #'evil-pinyin--advice))))
;; end of namespace
)

(provide 'evil-pinyin)
;;; evil-pinyin.el ends here
