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
(define (exec-safe str)
  (define res
    (try 
      (being
        (define input-data (call-with-input-string str read)) 
        (safe-eval input-data environment: exec-env))
      (catch #f)) ) 
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
          (list 'fold 'fold-right 'reduce 'format) )
(safe-environment-set! exec-env 'source 
                       "P9C-372, nah, just kidding.https://github.com/Kubuxu/scm-entity")

(irc:join con "#V")
;;(irc:join con "#cjdns")

(irc:add-message-handler!
  con repl
  command: "PRIVMSG"
  body: (lambda (msg)
          (define body (irc:message-msg msg))
          (and (string? body) (string=? "?" (string-take body 1)))
          ) )

(irc:run-message-loop con debug: #t)
;; vim: set expandtab ts=2 sw=2:
