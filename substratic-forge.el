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

;; This file is part of Substratic Forge.
;;
;; Substratic Forge is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Substratic Forge is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Substratic Forge.  If not, see <http://www.gnu.org/licenses/>.

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
    (setq substratic-rpc-pending-requests (cons `(,request-id . ,callback))
                                          substratic-rpc-pending-requests)
    (substratic-rpc-send message)))

;; DESIGN:
;; - Figure out how to use transient and magit-section for UI
;; - Use tabulated-list-mode for object list: https://www.gnu.org/software/emacs/manual/html_node/elisp/Tabulated-List-Mode.html

;;;###autoload
(defun substratic-connect ()
  "Initiates a connection to a Substratic Engine game."
  (interactive)

  ;; Switch to the REPL buffer first before starting
  (switch-to-buffer (get-buffer-create "* Gambit REPL *"))
  (goto-char (point-max))

  (run-at-time "1 sec" nil (lambda ()
                             ;; Connect to RPC server
                             (substratic-connect-rpc 44311)

                             ;; Connect to REPL
                             (geiser-connect 'gambit "localhost" 44555))))

(provide 'substratic-forge)

;;; substratic-forge.el ends here