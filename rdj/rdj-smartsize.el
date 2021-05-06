;; rdj-smartsize.el --- Ryan D Johnson's Emacs configuration

;; Copyright (C) 1998-2015 Ryan D Johnson

;; Author: Ryan D Johnson <ryandjohnson@gmail.com>

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Emacs; see the file COPYING, or type `C-h C-c'. If not,
;; write to the Free Software Foundation at this address:

;;   Free Software Foundation
;;   51 Franklin Street, Fifth Floor
;;   Boston, MA 02110-1301
;;   USA

;; Iosevka https://github.com/be5invis/Iosevka
;; v-g-singlestorey v-asterisk-low v-paragraph-low v-m-shortleg v-at-long v-numbersign-slanted
;;
;; Input font available here: http://input.fontbureau.com/download/
;;
;; Emacs will give an autocomplete list of fontspec strings in response to
;;   M-x set-default-font TAB
;;
;; You can use a function like this to get emacs to render a string in
;; all the fonts it knows:
;;
;;   (let ((str "The quick brown fox jumps over the lazy dog ´`''\"\"1lI|¦!Ø0Oo{[()]}.,:; ")
;;         (font-families (cl-remove-duplicates
;;                (sort (font-family-list)
;;                  (lambda(x y) (string< (upcase x) (upcase y))))
;;                :test 'string=)))
;;     (dolist (ff font-families)
;;       (insert
;;        (propertize str 'font-lock-face `(:family ,ff))               ff "\n")
;;       ))

(defun rdj-small-font () "-*-Iosevka-light-*-*-*-15-*-*-*-*-*-*-*")
(defun rdj-big-font   () "-*-Iosevka-light-*-*-*-18-*-*-*-*-*-*-*")
(defun rdj-huge-font   () "-*-Iosevka-light-*-*-*-18-*-*-*-*-*-*-*")

(defun rdj-change-font (fontspec) "Change the current and default frame font"
  (interactive)
  (set-frame-font fontspec)
  (aput 'default-frame-alist 'font fontspec)
)

(defun rdj-setup-frame (xpos ypos width height font)
  (rdj-change-font font)
  (set-frame-position (selected-frame) xpos ypos)
  (set-frame-size (selected-frame) width height)
)

(defun rdj-smartsize-frame-for (display-width display-height)
  (apply 'rdj-setup-frame (cond    ; +X  +Y   WxH  FONT
    ((= display-width 1440)   (list   0  22 177 20 (rdj-big-font)))
    ((= display-width 1680)   (list   1  22 207 52 (rdj-small-font)))
    ((= display-width 1920)   (list 175  50 225 72 (rdj-small-font)))
    ((= display-width 2560)   (list 240  22 250 70 (rdj-big-font)))
    ((< display-width 1920)   (list   0  22 180 50 (rdj-small-font)))
    ((< display-height 1600)  (list 261  50 260 76 (rdj-big-font)))
    ((>= display-height 1920) (list 261 125 260 80 (rdj-big-font)))
  ))
)

(defun rdj-smartsize-frame () "Set frame size, position, font to something reasonable"
  (interactive)
  ;; In 23.1, x-display-pixel-width is the width of the current
  ;; screen, not the entire display including all screens. However,
  ;; set-frame-position takes bounds which are across all screens. So
  ;; moving the frame back to (0,0) first lets us query the leftmost
  ;; topmost screen's size. Which - at least at this point - is what I
  ;; care about. I'll learn to live with the unsightly flash.
  (rdj-setup-frame 0 0 180 50 (rdj-small-font))
  (rdj-smartsize-frame-for (x-display-pixel-width) (x-display-pixel-height))
)

(provide 'rdj-smartsize)
