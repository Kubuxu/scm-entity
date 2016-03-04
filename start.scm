#!/usr/bin/csi -s
(require-extension sandbox irc posix symbol-utils srfi-1 srfi-13)

(define-syntax try
  (syntax-rules (catch)
                ((_ body (catch catcher))
                 (call-with-current-continuation
                   (lambda (exit)
                     (with-exception-handler
                       (lambda (condition)
                         catcher
                         (exit condition))
                       (lambda () body)))))))
(define con
  (irc:connection server: "localhost" nick: "Entity") )

(define (irc:message-resp msg)
  (define target (car (irc:message-parameters msg)))
  (if (char=? #\# (string-ref target 0))
    target
    (car (irc:message-prefix msg)) ) )
(define (irc:message-msg msg)
  (cadr (irc:message-parameters msg)) )

(define (string-truncate s len)
  (string-take s (min (string-length s) len)) )

(define exec-env 
  (make-safe-environment parent: default-safe-environment mutable: #t extendable: #t) )

(define (save-define data)
  (display (list 'saveing data)) (newline)
  (call-with-output-file
    "dataset"
    (lambda (port)
      (write data port)
      (newline port)
      ) #:append
    ) )
(define (load-defines)
  (define (work port)
    (define x (read port))
    (if (eof-object? x)
      '()
      (begin
        (safe-eval x environment: exec-env)
        (work port)
        ) ) )
  (if (file-exists? "dataset")
    (call-with-input-file "dataset" work) ) )

(define (publish-to-IPFS)
  (call-with-values
    (lambda () (process "ipfs add -q dataset"))
    (lambda (in out pid) (format "Dataset avalible at fs:/ipfs/~a" (read-line in))) ) ) 

(load-defines)
(define (exec-safe str)
  (define res
    (try 
      (begin
        (define input-data (call-with-input-string str read)) 
        (define output-data (safe-eval input-data environment: exec-env))
        (if (and (pair? input-data) (eq? (car input-data) 'define))
          (save-define input-data) )
        output-data)
      (catch #f) )
    )
  (if (condition? res)
    (get-condition-property res 'exn 'message "frak: no message supplied")
    res) ) 

(define (repl msg)
  (define resp (exec-safe (string-drop (irc:message-msg msg) 1)))
  (if (not (unspecified? resp))
    (irc:say
      con
      (string-truncate
        (format "~a: ~s" (car (irc:message-prefix msg))  resp)
        512)
      (irc:message-resp msg)) ) )

(irc:connect con)

(for-each (lambda (name) (safe-environment-set! exec-env name (eval name)))
          (list 'fold 'fold-right 'reduce 'format 'publish-to-IPFS) )

(irc:join con "#V")
(irc:join con "#cjdns")

(irc:add-message-handler!
  con repl
  command: "PRIVMSG"
  body: (lambda (msg)
          (define body (irc:message-msg msg))
          (and (string? body) (string=? "?" (string-take body 1)))
          ) )

(irc:run-message-loop con debug: #t)
;; vim: set expandtab ts=2 sw=2:
