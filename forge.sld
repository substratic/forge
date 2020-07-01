;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Forge - https://github.com/substratic/forge
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(define-library (substratic forge)
  (import (gambit)
          (substratic engine rpc)
          (substratic engine string)
          (substratic engine events))

  (export start-forge)

  (begin

    (define (reload-module module-ref module-path)
      (let ((modref (string->symbol (##modref->string module-ref)))
            (resolved (##search-module module-ref)))
        ;; (println "modref: " modref)
        ;; (pp resolved)

        ;; TODO: Catch errors and surface to editor
        (println "### Reloading module: " modref)
        (load module-path)
        (##remove-registered-module modref)
        (##eval `(##let ()
                    (##demand-module ,modref)
                    (println " -> Reloaded.")))))

    (define (reload-module-at-path module-path)
      ;; Is this file under userlib path?  Make it fully-qualified
      (let ((source (read (open-file module-path))))
        (if (not (equal? (car source) 'define-library))
            (println "Can't reload non-module file: " module-path)
            (let* ((module-name (string-join
                                 (cadr source)
                                 delimiter: "/"
                                 formatter: symbol->string))
                   (module-ref (##parse-module-ref module-name)))
              (if module-ref
                  (reload-module module-ref module-path)
                  #f)))))

    (define (make-forge-event-handler event-sink)
      (lambda (event)
        (case (event-type event)
         ((forge/reload-module)
          ;; TODO: Return result of reload operation
          (reload-module-at-path (symbol->string (event-data event 'module-path)))
          ((event-data event 'callback) '((success . t))))
         (else (event-sink event)))))

    (define (start-forge event-sink #!key (repl-port 44555) (rpc-port 44311))
      ;; Start Gambit's REPL
      (##start-repl-server (string-append "localhost:" (number->string repl-port)))

      ;; Start the RPC server
      (start-rpc-server rpc-port (make-forge-event-handler event-sink))

      ;; Invoke emacsclient to initiate the connection
      ;; TODO: Error handling?
      (shell-command "emacsclient -e \"(substratic-connect)\"" #t))))
