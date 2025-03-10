(module server-test
  (;; Parameters
   test-server-port
   connect-procedure
   standby-time
   max-attempts-to-connect
   time-between-attempts-to-connect

   ;; Procedures
   start-test-server
   stop-test-server
   with-test-server

   )

;; Code heavily based on the sendfile egg test infrastructure

(import scheme)
(cond-expand
  (chicken-4
   (import chicken posix data-structures utils ports files extras)
   (use tcp))
  (chicken-5
   (import (chicken base)
           (chicken condition)
           (chicken file)
           (chicken format)
           (chicken port)
           (chicken process)
           (chicken process-context)
           (chicken tcp)))
  (chicken-6
   (import (scheme base)
           (chicken base)
           (chicken condition)
           (chicken file)
           (chicken format)
           (chicken port)
           (chicken process)
           (chicken process-context)
           (chicken tcp)))
  (else (error "Unsupported CHICKEN version.")))

;;; Parameters
(define test-server-port (make-parameter 8080))

(define connect-procedure (make-parameter tcp-connect))

 ;; Time to wait for the server to start serving after it has started
 ;; accepting connections
(define standby-time (make-parameter 2))

;; Maximum number of attempts to connect to the server
(define max-attempts-to-connect (make-parameter 3))

(define time-between-attempts-to-connect (make-parameter 1))



;;; Procedures
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
        (sleep (time-between-attempts-to-connect))
        (or (can-connect?)
            (wait-for-server (sub1 times))))))

(define (start-test-server thunk)
  (newline)
  (notify "Starting test-server on port ~a" (test-server-port))
  (let ((pid (process-fork thunk)))
    (unless (wait-for-server (max-attempts-to-connect))
      (notify "Could not start test server!")
      (exit 0))
    (newline)
    (notify "Standby...")
    (flush-output)
    (sleep (standby-time))
    pid))

(define (stop-test-server pid)
  (notify "Shutting down")
  (process-signal pid)
  (newline)
  (notify "Sent SIGTERM to server. Please make sure the server isn't running anymore!\n"))

(define (with-test-server server-thunk tests-thunk)
  (let ((pid (start-test-server server-thunk)))
    (handle-exceptions exn
      (begin
        (stop-test-server pid)
        (abort exn))
      (begin
        (tests-thunk)
        (stop-test-server pid)))))

) ; end module
