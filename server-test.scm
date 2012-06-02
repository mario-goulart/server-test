(module server-test
(test-server-port start-test-server stop-test-server connect-procedure with-test-server)

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
        ((connect-procedure) "localhost" (test-server-port))
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
  (notify "Starting test-server on port ~a" (test-server-port))
  (let ((pid (process-fork thunk)))
    (unless (wait-for-server 3)
      (notify "Could not start test server!")
      (exit 0))
    (newline)
    (notify "Standby...")
    (flush-output)
    (sleep 4)
    pid))

(define (stop-test-server pid)
  (notify "Shutting down")
  (process-signal pid)
  (newline)
  (notify "Sent SIGTERM to server. Please make sure the server isn't running anymore!\n"))

(define (with-test-server server-thunk tests-thunk)
  (let ((pid (start-test-server server-thunk)))
    (handle-exceptions exn
      (stop-test-server pid)
      (begin
        (tests-thunk)
        (stop-test-server pid)))))

) ; end module
