;;; evil-pinyin.el --- Evil search Chinese characters by pinyin -*- lexical-binding: t; -*-

;; URL: https://github.com/laishulu/evil-pinyin
;; Created: June 17th, 2020
;; Keywords: extensions
;; Package-Requires: ((emacs "25") (names "0.5") (evil "1"))
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

(require 'evil)

(defvar evil-pinyin-with-search-rule 'custom
  "Enable the /search/ feature.

Possible values:
- 'always: always enable pinyin search.
- 'never: never enable pinyin search.
- 'custom: enable pinyin search when pattern started with, default `:'.")
(make-variable-buffer-local 'evil-pinyin-with-search-rule)

(defvar evil-pinyin-start-pattern ":"
  "`evil-pinyin' start pattern.")
(make-variable-buffer-local 'evil-pinyin-start-pattern)

(defvar evil-pinyin-with-punctuation t
  "Include Chinese punctuation.")
(make-variable-buffer-local 'evil-pinyin-with-punctuation)

(defvar evil-pinyin-char-table nil
  "User specified char table.")
(make-variable-buffer-local 'evil-pinyin-char-table)

(defvar evil-pinyin-scheme 'simplified-quanpin-all
  "Pinyin scheme.

Possible values:
- nil: user specified char table.
- 'simplified-quanpin-all: quanpin for all simplified characters.
- 'simplified-quanpin-common: quanpin of common used 3500 simplified characters.
- 'simplified-xiaohe-all: xiaohe shuangpin for all simplified characters.
- 'simplified-pinyinjiajia-all: pinyinjiajia shuangpin for all simplified characters.
- 'simplified-ziranma-all: ziranma shuangpin for all simplified characters.
- 'simplified-weiruan-all: weiruan shuangpin for all simplified characters.
- 'traditional-quanpin-all: quanpin for all traditional characters.
- 'simplified-traditional-quanpin-all: quanpin for all simplified and traditional characters.")
(make-variable-buffer-local 'evil-pinyin-scheme)

;; variable defined by evil-snipe
(defvar ::evil-snipe-aliases)

;; dynamically loaded scheme
(defvar evil-pinyin--simplified-quanpin-all)
(defvar evil-pinyin--simplified-quanpin-common)
(defvar evil-pinyin--simplified-xiaohe-all)
(defvar evil-pinyin--traditional-quanpin-all)
(defvar evil-pinyin--simplified-traditional-quanpin-all)
(defvar evil-pinyin--punctuation-alist)

(defconst evil-pinyin--this-file load-file-name
  "This file name.")

(defun evil-pinyin--load-char-table-file (char-table-file)
  "Load char table file CHAR-TABLE-FILE."
  (load (expand-file-name char-table-file (file-name-directory
                                            evil-pinyin--this-file))))

(defun evil-pinyin--get-char-table ()
  "Get char table for simplified Chinese."
  (cond
   (; use simplified quanpin
    (eq evil-pinyin-scheme 'simplified-quanpin-all)
    (unless (boundp 'evil-pinyin--simplified-quanpin-all)
      (evil-pinyin--load-char-table-file "simplified-quanpin-all"))
    evil-pinyin--simplified-quanpin-all)
   (; use simplified common
    (eq evil-pinyin-scheme 'simplified-quanpin-common)
    (unless (boundp 'evil-pinyin--simplified-quanpin-common)
      (evil-pinyin--load-char-table-file "simplified-quanpin-common"))
    evil-pinyin--simplified-quanpin-common)
   (; use simplified xiaohe
    (memq evil-pinyin-scheme (list 'simplified-xiaohe-all
                       'simplified-pinyinjiajia-all
                       'simplified-ziranma-all
                       'simplified-weiruan-all))
    (unless (boundp 'evil-pinyin--simplified-xiaohe-all)
      (evil-pinyin--load-char-table-file "simplified-xiaohe-all"))
    evil-pinyin--simplified-xiaohe-all)
   (; use tradtional quanpin
    (eq evil-pinyin-scheme 'traditional-quanpin-all)
    (unless (boundp 'evil-pinyin--traditional-quanpin-all)
      (evil-pinyin--load-char-table-file "traditional-quanpin-all"))
    evil-pinyin--traditional-quanpin-all)
   (; use simplified and tradtional quanpin
    (eq evil-pinyin-scheme 'simplified-traditional-quanpin-all)
    (unless (boundp 'evil-pinyin--simplified-traditional-quanpin-all)
      (evil-pinyin--load-char-table-file "simplified-traditional-quanpin-all"))
    evil-pinyin--simplified-traditional-quanpin-all)
   (; user specified char table
    (not evil-pinyin-scheme)
    evil-pinyin-char-table)))

(defun evil-pinyin--get-punctuation-alist()
  "Get punctuation alist."
  (unless (boundp 'evil-pinyin--punctuation-alist)
    (evil-pinyin--load-char-table-file "punctuation"))
  evil-pinyin--punctuation-alist)

(defun evil-pinyin--build-regexp-char
    (char &optional no-punc-p only-chinese-p)
  "Build regexp for a character CHAR.

NO-PUNC-P: punctuations are not included.
ONLY-CHINESE-P: English characters are not included."
  (let ((diff (- char ?a))
        regexp)
    (if (or (>= diff 26) (< diff 0))
        (or (and (not no-punc-p)
                 (assoc-default
                  char
                  (evil-pinyin--get-punctuation-alist)))
            (regexp-quote (string char)))
      (setq regexp (nth diff (evil-pinyin--get-char-table)))
      (if only-chinese-p
          (if (string= regexp "")
              regexp
            (format "[%s]" regexp))
        (format "[%c%s]" char
                regexp)))))

(defun evil-pinyin--build-regexp-string
    (str &optional no-punc-p only-chinese-p)
  "Build regexp for a string STR.

NO-PUNC-P: punctuations are not included.
ONLY-CHINESE-P: English characters are not included."
  (mapconcat
   (lambda (c) (evil-pinyin--build-regexp-char c no-punc-p only-chinese-p))
   str
   ""))


(defun evil-pinyin--build-regexp (thing)
  "Build regexp form THING for search."
  (cond

   ((integerp thing)
    (evil-pinyin--build-regexp-char
     thing (not evil-pinyin-with-punctuation)))

   ((stringp thing)
    (evil-pinyin--build-regexp-string
     thing (not evil-pinyin-with-punctuation)))))

(evil-define-motion find-char (count char)
  "Move to the next COUNT'th occurrence of CHAR."
  :type inclusive
  (interactive "<c><C>")
  (setq count (or count 1))
  (let ((fwd (> count 0)))
    (setq evil-last-find (list #'find-char char fwd))
    (when fwd (forward-char))
    (let ((case-fold-search nil))
      (unless
          (prog1
              (search-forward-regexp
               (evil-pinyin--build-regexp char)
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
    (setcar evil-last-find #'find-char-to)))

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
        (and (eq cmd #'find-char-to)
             evil-repeat-find-to-skip-next
             (= count 1)
             (or (and fwd (or (= (char-after (1+ (point))) char)
                              (string-match-p
                               (evil-pinyin--build-regexp char)
                               (string (char-after (1+ (point)))))))
                 (and (not fwd) (or (= (char-before) char)
                                    (string-match-p
                                     (evil-pinyin--build-regexp char)
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

:autoload
(define-minor-mode mode
  "Evil search or find Chinese characters by pinyin."
  :init-value nil
  (advice-add 'evil-ex-pattern-regex :around
              #'evil-ex-pattern-regex-advice)
  (when (featurep 'evil-snipe)
    (advice-add 'evil-snipe--process-key :around
                #'evil-snipe--process-key-advice))
  (if (and mode evil-motion-state-local-map)
      (progn
        (define-key evil-motion-state-local-map
          [remap evil-find-char]
          #'find-char)
        (define-key evil-motion-state-local-map
          [remap evil-find-char-backward]
          #'find-char-backward)
        (define-key evil-motion-state-local-map
          [remap evil-find-char-to]
          #'find-char-to)
        (define-key evil-motion-state-local-map
          [remap evil-find-char-to-backward]
          #'find-char-to-backward)
        (define-key evil-motion-state-local-map
          [remap evil-repeat-find-char]
          #'repeat-find-char)
        (define-key evil-motion-state-local-map
          [remap evil-repeat-find-char-reverse]
          #'repeat-find-char-reverse))

    (when evil-motion-state-local-map
      (define-key evil-motion-state-local-map
        [remap evil-find-char] nil)
      (define-key evil-motion-state-local-map
        [remap evil-find-char-backward] nil)
      (define-key evil-motion-state-local-map
        [remap evil-find-char-to] nil)
      (define-key evil-motion-state-local-map
        [remap evil-find-char-to-backward] nil)
      (define-key evil-motion-state-local-map
        [remap evil-repeat-find-char] nil)
      (define-key evil-motion-state-local-map
        [remap evil-repeat-find-char-reverse] nil))))

;; ---------------------- ;;
;; evil-snipe integration ;;
;; ---------------------- ;;
(defun evil-pinyin--snipe-process-key (key)
  "Process the snipe KEY."
  (when (featurep 'evil-snipe)
    (let ((regex-p (assoc key evil-snipe-aliases))
          (keystr (char-to-string key)))
      (cons keystr
            (if regex-p (elt regex-p 1)
              (if mode
                  (evil-pinyin--build-regexp keystr)
                keystr))))))

(defun evil-snipe--process-key-advice (fn key)
  "Advice for FN `evil-snipe--process-key' with KEY."
  (if mode
      (funcall #'evil-pinyin--snipe-process-key key)
    (funcall fn key)))

(defun evil-ex-pattern-regex-advice (fn &rest args)
  "Advice for FN `evil-ex-pattern-regex' with ARGS args."
  (let ((re (apply fn args)))
    (if (and mode re mode
             (cond (; always
                    (eq evil-pinyin-with-search-rule 'always) t)
                   (; never
                    (eq evil-pinyin-with-search-rule 'never) nil)
                   (; custom
                    (eq evil-pinyin-with-search-rule 'custom)
                    (and re (= (string-to-char re)
                               (string-to-char evil-pinyin-start-pattern)))))
             (not (string-match-p "\[.*+?[\\$]" re)))
        (evil-pinyin--build-regexp
          (if (eq evil-pinyin-with-search-rule 'custom) (substring re 1) re))
      re)))

(defun evil-pinyin-clear()
  "Clear all pollutions."
  (advice-remove 'evil-ex-pattern-regex #'evil-ex-pattern-regex-advice)
  (when (featurep 'evil-snipe)
    (advice-remove 'evil-snipe--process-key #'evil-snipe--process-key-advice)))

:autoload
(define-globalized-minor-mode
  global-evil-pinyin-mode
  evil-pinyin-mode
  evil-pinyin-mode)

(provide 'evil-pinyin)
;;; evil-pinyin.el ends here
