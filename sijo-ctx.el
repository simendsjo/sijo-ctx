;;; sijo-ctx.el --- Switching between profiles in contexts -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Simen Endsjø
;;
;; Author: Simen Endsjø <simendsjo@gmail.com>
;; Maintainer: Simen Endsjø <simendsjo@gmail.com>
;; Created: July 13, 2023
;; Modified: July 13, 2023
;; Version: 0.0.1
;; Keywords: convenience
;; Homepage: https://github.com/simendsjo/sijo-ctx
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Switching between contexts. Heavily inspired by the `contextual' package.
;;  The reason I created this package was to make it easier to have activation
;;  and deactivation functions for each profile, and more granular hooks.
;;
;;  Several contexts are supported, and optional on-activate and on-deactivate
;;  functions for when activating/deactivating a profile. Within a given
;;  context, only a single profile can be active at a time.
;;
;;  Usage:
;;
;;  ;; Set a suitable default context name to avoid the generic name
;;  (setf sijo-ctx-default-context 'profiles)
;; 
;;  ;; Add a profile. Defining the activate and deactivate functions by
;;  ;; themselves and referring them by the symbol (not #'), makes it easier to
;;  ;; change them while developing.
;;  
;;  (defun profile1-on-activate ()
;;    (message "Profile 1 activated"))
;; 
;;  (defun profile1-on-deactivate ()
;;    (message "Profile 1 deactivated"))
;;
;;  ;; Use ' rather than #' for referring to the functions so you can replace
;;  ;; them without having to add the profile again.
;;  (add-profile 'profile1
;;               'profile1-on-activate
;;               'profile1-on-deactivate)
;;
;;  ;; The last argument in the context.
;;  (add-profile 'work nil nil 'email)
;;  (add-profile 'private nil nil 'email)
;;
;;  ;; When switching to a profile, use \\[sijo-ctx-switch-to-profile] for
;;  ;; profiles in the `sijo-ctx-default-context' and with
;;  ;; \\[universal-argument] to select the context.
;;; Code:

(require 'cl-lib)
(require 'cl-extra)

(defvar sijo-ctx--debug nil
  "Whether to print debug messages.")

(defun sijo-ctx--debug (message &rest args)
  "Call `message' with MESSAGE and ARGS when `sijo-ctx--debug' is non-nil."
  (when sijo-ctx--debug
    (apply #'message (cl-concatenate 'string "sijo-ctx: " message) args)))

(defgroup sijo-ctx '()
  "Switching between contexts."
  :group 'convenience
  :prefix "sijo-ctx-")

(defcustom sijo-ctx-default-context 'sijo-ctx--default-context
  "The default context."
  :group 'sijo-ctx
  :type 'symbol)

(defcustom sijo-ctx-before-switch '()
  "Hook called before switching profile.

At this point, the following is set:
- `sijo-ctx-switching-previous-profile' is 'nil'
- `sijo-ctx-switching-current-profile' is currently active profile, or 'nil' if
  no profile is active.
- `sijo-ctx-switching-next-profile' is the profile which will be loaded."
  :group 'sijo-ctx
  :type 'hook)

(defcustom sijo-ctx-before-deactivate '()
  "Hook called before currently active profile is deactivated.

At this point, the following is set:
- `sijo-ctx-switching-previous-profile' is 'nil'
- `sijo-ctx-switching-current-profile' is currently active profile, or 'nil' if
  no profile is active.
- `sijo-ctx-switching-next-profile' is the profile which will be loaded."
  :group 'sijo-ctx
  :type 'hook)

(defcustom sijo-ctx-after-deactivate '()
  "Hook called after currently active profile is deactivated.

At this point, the following is set:
- `sijo-ctx-switching-previous-profile' is the profile we are switching from.
- `sijo-ctx-switching-current-profile' is 'nil'
- `sijo-ctx-switching-next-profile' is the profile which will be loaded."
  :group 'sijo-ctx
  :type 'hook)

(defcustom sijo-ctx-before-activate '()
  "Hook called before new profile is activated.

At this point, the following is set:
- `sijo-ctx-switching-previous-profile' is the profile we are switching from.
- `sijo-ctx-switching-current-profile' is 'nil'
- `sijo-ctx-switching-next-profile' is the profile which will be loaded."
  :group 'sijo-ctx
  :type 'hook)

(defcustom sijo-ctx-after-activate '()
  "Hook called after new profile is activated.

At this point, the following is set:
- `sijo-ctx-switching-previous-profile' is the profile we are switching from.
- `sijo-ctx-switching-current-profile' is the profile we switched to.
- `sijo-ctx-switching-next-profile' is 'nil'"
  :group 'sijo-ctx
  :type 'hook)

(defcustom sijo-ctx-after-switch '()
  "Hook called after switching profile.

At this point, the following is set:
- `sijo-ctx-switching-previous-profile' is the profile we are switching from.
- `sijo-ctx-switching-current-profile' is the profile we switched to.
- `sijo-ctx-switching-next-profile' is 'nil'"
  :group 'sijo-ctx
  :type 'hook)

(defvar sijo-ctx-current-context nil
  "The current context.")

(defmacro sijo-ctx--with-context (context &rest body)
  "Use CONTEXT as the current context when evaluating BODY.

The exact context is determined by the first non-nil value of CONTEXT,
`sijo-ctx-current-context' and `'sijo-ctx--default-context'."
  (declare (indent defun))
  (let ((old-context (gensym "slijo-ctx--with-context-")))
    `(let ((,old-context sijo-ctx-current-context)
           (sijo-ctx-current-context (or ,context sijo-ctx-current-context sijo-ctx-default-context)))
       (unless (eq ,old-context sijo-ctx-current-context)
         (sijo-ctx--debug "Switching context from %s to %s" ,old-context sijo-ctx-current-context))
       ,@body)))

(defvar sijo-ctx--contexts '()
  "All defined contexts.")

(defun sijo-ctx--define-context (&optional context)
  "Create an empty CONTEXT."
  (sijo-ctx--with-context context
    (unless (get sijo-ctx-current-context 'sijo-ctx--created)
      (sijo-ctx--debug "Defining context %s" sijo-ctx-current-context)
      (put sijo-ctx-current-context 'sijo-ctx--created t)
      (put sijo-ctx-current-context 'sijo-ctx--profiles '())
      (put sijo-ctx-current-context 'sijo-ctx-active-profile nil)
      (cl-pushnew sijo-ctx-current-context sijo-ctx--contexts))))

(defun sijo-ctx--clear-context (&optional context)
  "Clear CONTEXT."
  (sijo-ctx--with-context context
    (sijo-ctx--debug "Clearing context %s" sijo-ctx-current-context)
    ;; Don't know how I actually remove entries, so I rather empty them
    (put sijo-ctx-current-context 'sijo-ctx--created nil)
    (put sijo-ctx-current-context 'sijo-ctx--profiles '())
    (put sijo-ctx-current-context 'sijo-ctx-active-profile nil)
    (delq sijo-ctx-current-context sijo-ctx--contexts)))

(defun sijo-ctx--clear-all-contexts ()
  "Delete all contexts."
  (dolist (context (cl-copy-list sijo-ctx--contexts))
    (sijo-ctx--clear-context context)))

(defun sijo-ctx--profiles (&optional context)
  "Return all profiles in CONTEXT."
  (sijo-ctx--with-context context
    (get sijo-ctx-current-context 'sijo-ctx--profiles)))

;;;###autoload
(defun sijo-ctx-active-profile (&optional context)
  "Return the active profile in CONTEXT."
  (sijo-ctx--with-context context
    (get sijo-ctx-current-context 'sijo-ctx-active-profile)))

(defun sijo-ctx--set-active-profile (profile &optional context)
  "Set the active profile in CONTEXT to PROFILE."
  (sijo-ctx--with-context context
    (setf (get sijo-ctx-current-context 'sijo-ctx-active-profile) profile)))

;;;###autoload
(defun sijo-ctx-add-profile (profile on-activate on-deactivate &optional context)
  "Add profile with symbol PROFILE to CONTEXT.

ON-ACTIVATE and ON-DEACTIVATE are functions which is called when switching to
and from the PROFILE respectively. If they are 'nil', the function 'ignore' is
used instead.

The function might be called several times, and PROFILE is replaced with the new
values.

Example:
  (defun profile1-on-activate ()
    (message \"Activated profile1\"))

  (defun profile1-on-deactivate ()
    (message \"Deactivated profile1\"))

  (sijo-ctx-add-profile 'profile1
                        'profile1-on-activate
                        'profile1-on-deactivate)"

  (sijo-ctx--with-context context
    (sijo-ctx--define-context)
    (setf (alist-get profile (get sijo-ctx-current-context 'sijo-ctx--profiles))
          (list (or on-activate 'ignore)
                (or on-deactivate 'ignore)))))

(defvar sijo-ctx-switching-previous-profile nil
  "The previous profile in while switching profiles.

Starts as 'nil', and transitions to the previous profile after it has been
deactivated.")

(defvar sijo-ctx-switching-current-profile nil
  "The current active profile while switching profiles.

Starts as the currently active profile, transitions to 'nil' as it's
deactivated, and then to the new profile as it's activated.")

(defvar sijo-ctx-switching-next-profile nil
  "The next profile to be loaded while switching profiles.

Starts as the new profile, and transitions to 'nil' as the new profile is
loaded.")

(defun sijo-ctx--activate-profile (profile &optional context)
  "Activate PROFILE in CONTEXT."
  (sijo-ctx--with-context context
    (let* ((sijo-ctx-switching-previous-profile nil)
           (sijo-ctx-switching-current-profile (sijo-ctx-active-profile))
           (sijo-ctx-switching-next-profile profile)
           (profiles (sijo-ctx--profiles))
           (current (assoc sijo-ctx-switching-current-profile profiles))
           (current-on-deactivate (or (cl-third current) 'ignore))
           (next (assoc sijo-ctx-switching-next-profile profiles))
           (next-on-activate (cl-second next)))
      (message "sijo-ctx: Switching profile from '%s to '%s in context '%s" sijo-ctx-switching-current-profile sijo-ctx-switching-next-profile sijo-ctx-current-context)
      (sijo-ctx--debug "Current profile: %s" current)
      (sijo-ctx--debug "Next profile: %s" next)
      (run-hooks 'sijo-ctx-before-switch)
      ;; Deactivate
      (run-hooks 'sijo-ctx-before-deactivate)
      (sijo-ctx--debug "Running function #'%s for deactivating profile %s" current-on-deactivate sijo-ctx-switching-current-profile)
      (funcall current-on-deactivate)
      (setf sijo-ctx-switching-previous-profile sijo-ctx-switching-current-profile)
      (sijo-ctx--set-active-profile nil)
      (setf sijo-ctx-switching-current-profile nil)
      (run-hooks 'sijo-ctx-after-deactivate)
      ;; Activate
      (run-hooks 'sijo-ctx-before-activate)
      (sijo-ctx--debug "Running function #'%s for activating profile %s" next-on-activate sijo-ctx-switching-next-profile)
      (funcall next-on-activate)
      (sijo-ctx--set-active-profile sijo-ctx-switching-next-profile)
      (setf sijo-ctx-switching-current-profile sijo-ctx-switching-next-profile)
      (setf sijo-ctx-switching-next-profile nil)
      (run-hooks 'sijo-ctx-after-activate)
      (run-hooks 'sijo-ctx-after-switch))))

;;;###autoload
(defun sijo-ctx-switch-profile (&optional context)
  "Switch to another profile in CONTEXT.

When called with \\[universal-argument], the user is prompted for the CONTEXT.

See documentation for hooks for more information. They are called in the
following order:
- `sijo-ctx-before-switch'
- `sijo-ctx-before-deactivate'
- `sijo-ctx-after-deactivate'
- `sijo-ctx-before-activate'
- `sijo-ctx-after-activate'
- `sijo-ctx-after-switch'

The function also sets some dynamic variables in the process which can be used
to inspect the current state:
- `sijo-ctx-switching-previous-profile'
- `sijo-ctx-switching-current-profile'
- `sijo-ctx-switching-next-profile'"
  (interactive "P")
  (let ((context (if context (car (read-from-string (completing-read "Context: " sijo-ctx--contexts))) context)))
    (sijo-ctx--with-context context
      (let ((profile (completing-read (format "Profile (context:%s active:%s): " sijo-ctx-current-context (sijo-ctx-active-profile))
                                      (mapcar #'car (sijo-ctx--profiles)))))
        (sijo-ctx--activate-profile (car (read-from-string profile)))))))

(provide 'sijo-ctx)
;;; sijo-ctx.el ends here
