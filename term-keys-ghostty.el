;;; term-keys-ghostty.el --- term-keys support for Ghostty

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file contains supplementary code for aiding in the
;; configuration of the Ghostty terminal emulator to interoperate with
;; the term-keys package.

;; For more information, please see the accompanying README.md file.

;;; Code:

;; TODO: Finish initial write
(require 'term-keys)


(defgroup term-keys/ghostty nil
  "`term-keys' options for GHOSTTY-based terminal emulators."
  :group 'term-keys)

(define-widget 'term-keys/ghostty-modifier 'lazy
  "Choice for Ghostty key modifier state flags."
  :type '(choice (const :tag "Shift" "shift+")
		 (const :tag "Ctrl" "ctrl+")
		 (const :tag "Super" "super+")
		 (const :tag "Alt" "alt+")
		 (const :tag "(none)" nil)))


(defcustom term-keys/ghostty-modifier-map ["shift+" "ctrl+" "alt+" "super+" nil nil]
  "Map of Ghostty modifiers to Emacs modifiers.

This should be a vector of 6 elements, with each element being a
string indicating the name of the Ghostty modifier name
corresponding to the Emacs modifiers Shift, Control, Meta, Super,
Hyper and Alt respectively.  nil indicates that there is no
mapping for this modifier."
  :type '(vector
	  (term-keys/ghostty-modifier :tag "Shift")
	  (term-keys/ghostty-modifier :tag "Control")
	  (term-keys/ghostty-modifier :tag "Meta")
	  (term-keys/ghostty-modifier :tag "Super")
	  (term-keys/ghostty-modifier :tag "Hyper")
	  (term-keys/ghostty-modifier :tag "Alt"))
  :group 'term-keys/ghostty)


(defun term-keys/ghostty-mods-representable (mods)
  "Return non-nil if the given MODS vector is representable in GHOSTTY."
  (cl-reduce (lambda (x y) (and x y)) ; all
	     (mapcar (lambda (n)
		       (or (not (elt mods n)) ; inactive modifier
			   (elt term-keys/ghostty-modifier-map n))) ; mapped
		     (number-sequence 0 (1- (length mods)))))) ; 0..5

(defun term-keys/ghostty-format-key (keymap mods)
  "Format key and modifiers in kitty syntax.

Returns the kitty key combination string corresponding to the
KEYMAP and modifier state MODS."
  (concat
   (cl-loop for modflag across mods
	    for index from 0
	    if modflag
	    concat
	    (elt term-keys/ghostty-modifier-map index))
   (elt keymap 15)))

(defun term-keys/ghostty-format-mods (mods)
  "Format MODS in Ghostty syntax."
  (if (cl-reduce (lambda (x y) (or x y)) mods)
      (concat
       ", mods: "
       (mapconcat
	(lambda (n)
	  (elt term-keys/ghostty-modifier-map n))
	(cl-remove-if-not (lambda (n) (elt mods n))
			  (number-sequence 0 (1- (length mods))))
	"|"))
    ""))


(defun term-keys/ghostty-config ()
  "Construct Ghostty configuration (config fragment).

This function returns, as a string, a ghostty config file fragment
necessary to configure Ghostty to encode term-keys key
sequences, according to the term-keys configuration."
  (apply #'concat
	 (term-keys/iterate-keys
	  (lambda (index keymap mods)
	    (format "keybind = all:%s=text:%s\n"
		    (term-keys/ghostty-format-key keymap mods)
		    (mapconcat
		     (lambda (c) (format "\\x%02x" c))
		     (append
		      term-keys/prefix
		      (term-keys/encode-key index mods)
		      term-keys/suffix
		      nil)
		     ""))))))


(provide 'term-keys-ghostty)
;;; term-keys-ghostty.el ends here
