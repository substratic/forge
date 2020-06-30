;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Forge - https://github.com/substratic/forge
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(define-library (substratic forge)
  (import (gambit)
          (github.com/substratic engine rpc))

  (export start-forge)

  (begin

    (define (start-forge event-sink #!key (repl-port 44555) (rpc-port 44311))
      ;; Start Gambit's REPL
      (##start-repl-server (string-append "localhost:" (number->string repl-port)))

      ;; Start the RPC server
      (start-rpc-server rpc-port event-sink)

      ;; Invoke emacsclient to initiate the connection
      ;; TODO: Error handling?
      (shell-command "emacsclient -e \"(substratic-connect)\"" #t))))
