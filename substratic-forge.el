;;; substratic-forge.el --- Tools for live-hacking Substratic Engine games. -*- lexical-binding: t -*-

;; Author: David Wilson <david@daviwil.com>
;; Maintainer: David Wilson <david@daviwil.com>
;; Version: 0.1.0
;; Package-Requires: ((cl-lib))
;; Keywords: substratic, gamedev, game-development, scheme
;; URL: https://github.com/substratic/forge
;;
;; This file is NOT part of GNU Emacs.

;;; License:

;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Forge - https://github.com/substratic/forge
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

;;; Commentary:

;; Substratic Forge is a package that facilitates live-hacking of Substratic
;; Engine games.  It provides a custom REPL and separate RPC channel for
;; enabling richer integration with Emacs.

;;; Code:

;; Emacs-side RPC design:
;;
;; Currently there's no need to round-trip game objects.  Requests sent
;; from Emacs need to be careful to encode certain values correctly
;; like #t and #f (use '\#t, etc to format them correctly).  Emacs will
;; also need to regex-replace #t and #f to the escaped versions before
;; passing the form through 'read'.  This should relieve the RPC server
;; from having to process incoming or outgoing messages.

(require 'cl-lib)

(defvar substratic-rpc-client nil)
(defvar substratic-rpc-next-request-id 0)
(defvar substratic-rpc-pending-requests '()) ;; An alist of (id . callback)

(defun substratic-rpc-message-data (key message)
  "Retrieves data named KEY from the RPC MESSAGE."
  (alist-get key (caddr message)))

(defun substratic-rpc-process-filter (process string)
  "Handles a STRING that came from PROCESS."
  ;; (message "RECEIVED: %s" string)
  (let ((message (read string)))
    (pcase (car message)
     ('event
      (message "Got an event! %s" (cadr message)))
     ('response
      (let ((request-callback (alist-get (substratic-rpc-message-data 'request-id message) substratic-rpc-pending-requests)))
        (apply request-callback (list message)))))))

(defun substratic-rpc-process-sentinel (process event)
  "Handles an EVENT that came from PROCESS."
  (pcase event
    ("open\n" (message "Connected to RPC server!"))
    ("connection broken by remote peer\n" (message "Game exited."))
    (_ (message "Unexpected sentinel event: %s" event))))

(defun substratic-connect-rpc (port)
  "Connects to a Substratic RPC server at PORT."
  (setq substratic-rpc-client (make-network-process :name "substratic-rpc"
                                              :filter #'substratic-rpc-process-filter
                                              :sentinel #'substratic-rpc-process-sentinel
                                              :host "127.0.0.1"
                                              :service port
                                              :nowait t)))

(defun substratic-rpc-send (message)
  "Sends an RPC MESSAGE to the current server."
  ;; TODO: Track requests and assign IDs
  (if substratic-rpc-client
      (process-send-string substratic-rpc-client (format "%s" message))
      (message "substratic-rpc-send: Not connected to RPC server.")))

(defun substratic-rpc-request (type callback &optional alist)
  "Sends an RPC message of type TYPE and invokes CALLBACK when \
the engine responds.  Parameters to the RPC command can be provided
in the optional ALIST parameter."
  (let* ((request-id (incf substratic-rpc-next-request-id))
         (message `(request ,type
                            ,(cons `(request-id . ,request-id) (or alist '())))))
    (setq substratic-rpc-pending-requests (cons `(,request-id . ,callback)
                                                substratic-rpc-pending-requests))
    (substratic-rpc-send message)))

;;;###autoload
(defun substratic-reload-module (&optional buffer)
  "Reloads the module referenced by BUFFER or the current buffer."
  (interactive)
  (let ((file-name (buffer-file-name buffer)))
    ;; TODO: Check if it's a .scm or .sld file
    (if file-name
        (substratic-rpc-request 'forge/reload-module
                                (lambda (response) (message "Reloaded module!"))
                                `((module-path . ,file-name)))
        (message "This buffer does not have an associated file."))))

;; DESIGN:
;; - Figure out how to use transient and magit-section for UI
;; - Use tabulated-list-mode for object list: https://www.gnu.org/software/emacs/manual/html_node/elisp/Tabulated-List-Mode.html

;;;###autoload
(defun substratic-connect ()
  "Initiates a connection to a Substratic Engine game."
  (interactive)

  ;; Connect to RPC server
  (substratic-connect-rpc 44311))

(provide 'substratic-forge)

;;; substratic-forge.el ends here
