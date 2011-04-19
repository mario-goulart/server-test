(module server-test
(test-server-port start-test-server stop-test-server connect-procedure)

;; Code heavily based on the sendfile egg test infrastructure

(import chicken scheme posix data-structures utils ports files extras)
(use tcp)

(define test-server-port (make-parameter 8080))
(define connect-procedure (make-parameter tcp-connect))

(define (notify fmt . args)
  (apply printf fmt args)
  (flush-output))

;; tests if server is already up
;; thanks to Peter Bex
(define (can-connect?)
  (handle-exceptions exn
    #f
    (receive (in out)
        ((connect-procedure) "localhost" 8080)
      (close-input-port in)
      (close-output-port out)
      #t)))

(define (wait-for-server times)
  (if (zero? times)
      #f
      (begin
        (sleep 1)
        (or (can-connect?)
            (wait-for-server (sub1 times))))))

(define (start-test-server thunk)
  (newline)
  (notify "starting test-server on port ~a" (test-server-port))
  (let ((pid (process-fork thunk)))
    (unless (wait-for-server 3)
      (notify "could not start test server!")
      (exit 0))
    (newline)
    (notify "standby")
    (flush-output)
    (sleep 4)
    pid))

(define (stop-test-server pid)
  (notify "shutting down")
  (process-signal pid)
  (newline)
  (notify "sent SIGTERM to server. Please make sure the server isn't running anymore!"))

) ; end module