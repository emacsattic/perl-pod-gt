;;; perl-pod-gt.el --- helpers for Perl POD <> markup

;; Copyright 2007, 2008 Kevin Ryde

;; Author: Kevin Ryde <user42@zip.com.au>
;; Version: 4
;; Keywords: wp
;; URL: http://www.geocities.com/user42_kevin/perl-pod-gt/index.html
;; EmacsWiki: PerlPodGt

;; perl-pod-gt.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; perl-pod-gt.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses>.


;;; Commentary:
;;
;; This spot of code helps when writing Perl POD markup forms like C<...>,
;; B<...>, etc.  `perl-pod-gt-enable' offers smart E<gt> insertion, line
;; break suppression when filling, and some font lock
;; warnings. `perl-pod-gt-double' converts C<..> to C<<...>> when you need
;; it and are ready to assume perl 5.6.  See the docstrings for details.

;;; Install
;;
;; Put perl-pod-gt.el somewhere in your load-path and add to your .emacs
;;
;;     (autoload 'perl-pod-gt-enable "perl-pod-gt")
;;     (add-hook 'perl-mode-hook 'perl-pod-gt-enable)
;;
;; And for cperl-mode or other similar mode
;;
;;     (add-hook 'cperl-mode-hook 'perl-pod-gt-enable)
;;
;; There's autoload cookies for the functions and the customize options on
;; the hooks if you use `update-file-autoloads' and friends.

;;; History:
;;
;; Version 1 - the first version
;; Version 2 - new `perl-pod-gt-double'
;; Version 3 - warning face done with overlays
;; Version 4 - don't line break after C<!>


;;; Code:

(require 'overlay) ;; for xemacs21


(defun perl-pod-gt-find-markup ()
  "Find Perl POD C<...> markup surrounding point.
This might change in the future, but for now it works like this:

If point is within a C<...>, B<<...>> etc markup form then point
is moved to the starting C, B, etc, and the return is the symbol
`E' if also within an E<> in that markup, or t if in ordinary
text.

If point is not within a markup at all the return is nil and
point is moved somewhere unspecified.

The strategy is to search from the start of the paragraph forward
for a C<, C<<, C<<< etc which extends across point.  This means
nested apparent forms like C<< B<x> >> give the outermost (as the
formatters will of course do).  Looking only from the start of
the paragraph keeps down the amount parsed."

  (save-restriction
    (narrow-to-region (save-excursion (backward-paragraph) (point))
                      (point)) ;; start of paragraph through to point
    (let ((case-fold-search nil)
          (ret nil))
      (goto-char (point-min))
      (while (and (not ret)
                  (or (and (looking-at "[IBCLFSX]<+")
                           (goto-char (match-end 0)))
                      (re-search-forward "[IBCLFSX]<+" nil t)))

        (let* ((beg    (match-beginning 0))
               (angles (- (match-end 0) (match-beginning 0) 1))
               (re     (concat (make-string angles ?>)
                               "\\|E<[^>]*\\(\\(>\\)\\|$\\)"))
               (found  nil))
          (while (and (setq found (re-search-forward re nil t))
                      (match-beginning 2))) ;; matching a whole E
          (cond ((not found) ;; no end, so within markup
                 (goto-char beg)
                 (setq ret t))
                ((match-beginning 1) ;; last was a partial E<
                 (goto-char beg)
                 (setq ret 'E)))))
      ret)))

;;-----------------------------------------------------------------------------
;; paragraph breaking

;;;###autoload
(defun perl-pod-gt-nobreak-p ()
  "Don't break in certain Perl POD markup forms.
This function is for use in `fill-nobreak-predicate'.  It avoids
a line break in the following circumstances,

* Anywhere within a non-breaking S<...> form.
* Immediately after a perl 5.6 style opening C<< or B<<< etc.
* Immediately before a perl 5.6 style closing >> or >>> etc.
* After a C<!>.

S<> forms tell the formatters to avoid line breaks in the
contents, and it's good to do the same in the source.

Perl 5.6 multi-angle C<< >> etc forms require whitespace after
the open and before the close.  A newline is fine for the
formatters, but it's easier to read the source if the markup is
on the same line as its content.  (This is a little like what
`fill-french-nobreak-p' does for double angle quote marks.)

C<!> is likely to be talking about the ! operator rather than an
exclamation to end a sentence.  Making sure its not at the end of
a line stops nroff/troff putting two spaces after it (at least
with what pod2man of perl 5.10 generates)."

  (or (save-excursion
 	(skip-chars-backward " \t")
        (goto-char (- (point) 4))
        (looking-at "C<!>"))

      (let ((orig-point (point)))
        (save-excursion
          (and (perl-pod-gt-find-markup)
               (or
                ;; no break anywhere at all within an S<>
                (= ?S (char-after))

                ;; no break immediately after a C<, B<<, etc
                (progn
                  (looking-at ".\\(<+\\) *")
                  (>= (match-end 0) orig-point))

                ;; no break before terminating >>
                (let ((angles (- (match-end 1) (match-beginning 1))))
                  (goto-char orig-point)
                  (looking-at (concat " *" (make-string angles ?>))))))))))


;;-----------------------------------------------------------------------------
;; interactive commands

;;;###autoload
(defun perl-pod-gt-insert ()
  "Insert either \">\" or \"E<gt>\" as needed for Perl POD.
When inside a C<>, B<> etc form and you type \"->\", \"=>\" or a
space and \">\", the \">\" is inserted as \"E<gt>\".

It's easy to forget to escape the > when typing arrows or a
greater than expression.  `perl-pod-gt-insert' tries to help you
get the common cases right, but it also tries to keep out of your
hair by only acting when it's pretty clear E<gt> is right.

Perl 5.6 style doubled \"C<<...>>\" etc is recognised and it
doesn't need escaping so `perl-pod-gt-insert' just inserts a
plain \">\" in that case."

  (interactive)
  (if (and (memq (char-before) '(?- ?= ? ))
           (save-excursion
             (and (eq t (perl-pod-gt-find-markup)) ;; inside and not in E<
                  (not (looking-at ".<<"))))) ;; not for 2 or more <<
      (insert "E<gt>")
    (insert ">")))

;;;###autoload
(defun perl-pod-gt-double ()
  "Convert C<> style markup at point to C<< >>.
Isolated E<gt> forms in the markup are converted to plain > since
such an escape is no longer needed in a C<< >>.  But repeated
E<gt>E<gt> doesn't become >>, since that could wrongly look like
an end marker."
  (interactive)
  (save-excursion
    (let ((orig-point (point)))
      (when (not (perl-pod-gt-find-markup))
        ;; not on the inside of a C<> forms, but possible on the C or <
        (goto-char orig-point)
        (or (looking-at "[IBCLFSX]<[^<]")
            (goto-char (max (1- (point)) (point-min))))))

    (or (looking-at "[IBCLFSX]\\(<\\)[^<]") ;; single C<... form
        (error "Not in a C<...> etc single angles form"))

    ;; "C<" becomes "C<< "
    (replace-match "<< " t t nil 1)

    ;; ">" becomes "E<gt>", through to final ">" becoming " >>" (if present)
    (while (and (re-search-forward "\\(E<gt>\\)+\\|>" nil t)
                (let ((len (- (match-end 0) (match-beginning 0))))
                  (cond ((match-beginning 1) ;; E<gt>
                         (if (= len 5)
                             (replace-match ">" t t))
                         t)
                        (t ;; final >
                         (replace-match " >>" t t)
                         nil)))))))

;;-----------------------------------------------------------------------------
;; warning overlays

(defface perl-pod-gt-warn
  '((((class color))
     (:background "red"))
    (t
     (:inverse-video t)))
  "Face for warning of bad Perl pod bits.
The default is the same as `trailing-whitespace' face, namely red
background on a colour screen, or inverse video for black and
white."
  :group 'faces ;; faces group in absense of a better place
  :link  '(url-link :tag "perl-pod-gt.el home page"
                    "http://www.geocities.com/user42_kevin/perl-pod-gt/index.html"))

(defun perl-pod-gt-warn-after-change (beg end prev-len)
  "Put a warning face on tabs between BEG and END.
This function is meant for use from `after-change-functions'."

  (save-excursion
    ;; Extend to whole lines.
    ;;
    (goto-char beg)
    (setq beg (point-at-bol))
    (goto-char end)
    (setq end (point-at-eol))

    ;; lose existing overlays in case offending bits are now ok; or
    ;; offending bits have been deleted leaving a zero-length overlay; and
    ;; so as not to add multiple overlays onto unchanged bits
    ;; emacs21 and xemacs21 don't have `remove-overlay', it's new in emacs22
    (dolist (overlay (overlays-in beg end))
      (if (eq 'perl-pod-gt-warn (overlay-get overlay 'face))
          (delete-overlay overlay)))

    (dolist (re '(
;; "->" in the middle of C<> etc, but only when followed by a symbol char so
;; C<$var--> is ok but C<$var->method> is not
"[IBCLFSX]<\\(?:[^<>]\\|E+<[a-z]+>\\)\\(?:[^>]\\|E+<[a-z]+>\\)*\\(->\\)[A-Za-z_]"

;; "=>" in the middle of C<> etc, like C<abc=>def>
"[IBCLFSX]<\\(?:[^<>]\\|E+<[a-z]+>\\)\\(?:[^>]\\|E+<[a-z]+>\\)*\\(=>\\)"

;; no space after C<<, or C<<<, etc
"\\([IBCLFSX]<<+\\)[^< \t\r\n]"

;; no space before >> in a C<<...>>
"[IBCLFSX]<<[^<]\\(?:[^>]\\|>[^>]\\)*?[^ \t\r\n]\\(>>+\\)"

;; no space before >>> in a C<<<...>>>
"[IBCLFSX]<<<[^<]\\(?:[^>]\\|>\\(?:[^>]\\|>[^>]\\)\\)*?[^ \t\r\n]\\(>>>+\\)"
                  ))
      (goto-char beg)
      (while (re-search-forward re end t)
        (let ((overlay (make-overlay (match-beginning 1) (match-end 1)
                                     (current-buffer) nil nil)))
          (overlay-put overlay 'face 'perl-pod-gt-warn))))))

(defun perl-pod-gt-warn-enable ()
  "Enable perl-pod-gt warning overlays in the current major mode.
This is done by the main `perl-pod-gt-enable' but can be used
alone."
  (perl-pod-gt-warn-after-change (point-min) (point-max) 0) ;; initial
  (add-hook 'after-change-functions
            'perl-pod-gt-warn-after-change
            t   ;; append
            t)) ;; buffer-local


;;-----------------------------------------------------------------------------

;;;###autoload
(defun perl-pod-gt-enable ()
  "Enable the features of `perl-pod-gt' in the current major mode.
This is designed for use from a hook like `perl-mode-hook' or
`cperl-mode-hook'.  It does the following,

* Binds the > key is to `perl-pod-gt-insert' in the major mode
  map.

* Adds `perl-pod-gt-nobreak-p' to `fill-nobreak-predicate' for
  paragraph filling (buffer-local).

* Sets up warning face overlay on certain -> and similar probable
  errors within C<>, B<> etc markup (`perl-pod-gt-warn-enable').

XEmacs 21 doesn't have `fill-nobreak-predicate', so that's
skipped there.

No binding is made for `perl-pod-gt-double', you can do that
yourself if `M-x perl-pod-gt-double' is too much."

  (interactive)
  (define-key (current-local-map) ">" 'perl-pod-gt-insert)
  (perl-pod-gt-warn-enable)

  (cond ((not (boundp 'fill-nobreak-predicate))
         ;; no such feature at all in xemacs 21
         )

        ;; emacs22 fill-nobreak-predicate is a hook, add to it buffer-local
        ((get 'fill-nobreak-predicate 'custom-type)
         (add-hook 'fill-nobreak-predicate 'perl-pod-gt-nobreak-p nil t))

        ;; emacs21 fill-nobreak-predicate is a variable holding a function
        ;; if no existing value then plonk our function in
        ((not fill-nobreak-predicate)
         (set (make-local-variable 'fill-nobreak-predicate)
              'perl-pod-gt-nobreak-p))
        ;; if an existing value then append to it by making a lambda -- this
        ;; is fairly nasty
        (t
         (set (make-local-variable 'fill-nobreak-predicate)
              `(lambda ()
                 (or (perl-pod-gt-nobreak-p)
                     (,fill-nobreak-predicate)))))))

;; In principle could add perl-pod-gt-nobreak-p as a customize option for
;; fill-nobreak-predicate (in emacs 22 where that variable is a hook).  But
;; it's fairly perl specific and so unlikely to be wanted globally.

;;;###autoload
(custom-add-option 'perl-mode-hook  'perl-pod-gt-enable)
;;;###autoload
(custom-add-option 'cperl-mode-hook 'perl-pod-gt-enable)
;;;###autoload
(custom-add-option 'pod-mode-hook   'perl-pod-gt-enable)

(provide 'perl-pod-gt)

;;; perl-pod-gt.el ends here
