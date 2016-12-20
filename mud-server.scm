(require-library defstruct tcp posix)

(module mud-server

  (; Start the server
   start

   ; Clients handling
   clients
   client?
   client-data-set!
   client-data
   write-to-client
   disconnect-client)

  (import scheme chicken)
  (use data-structures extras srfi-1 defstruct tcp posix)

  ;
  ; Client structure
  ;
  (defstruct client

    input-port    ; Input port
    output-port   ; Output port
    output-buffer ; Stores all the output that was not sent yet

    state         ; Client state. It is a symbol.
                  ; Can be: 'connected or 'disconnect

    (data #f)     ; Any external information that can be freely set
                  ; from the event handler
    )

  ;
  ; The tcp listener
  ;
  (define listener #f)

  ;
  ; A list of CLIENTs
  ;
  (define clients (list))

  ;
  ; Number of milliseconds between each pulse
  ;
  (define pulse-milliseconds 100)

  ;
  ; Holds an external procedure that will handle events
  ;
  ; EVENT-HANDLER should be a function that accepts any number of parameters,
  ; like this: (define (event-handler . params) ...)
  ;
  ; The first parameters is a symbol, containing the event name.
  ;
  ; Possible events are:
  ;     - (on-connect CLIENT)
  ;       The client CLIENT connected to the server.
  ;
  ;     - (on-read-input INPUT-STRING CLIENT)
  ;       This event is triggered everytime a client sends data.
  ;       INPUT-STRING is a string containing the raw input that was read.
  ;
  ;     - (on-pulse)
  ;       Called at every pulse.
  ;
  ;     - (on-connection-lost client)
  ;       Called when the client connection is broken.
  ;
  (define event-handler #f)

  ;
  ; Exported procedures
  ;

  ; Add STRING to the field OUTPUT-BUFFER
  (define (write-to-client string client)
    (client-output-buffer-set!
      client
      (string-append (client-output-buffer client) string) ))

  ; Flag a client to the state 'disconnect', however he won't be disconnected
  ; instantly, the server loop will take care of this.
  (define (disconnect-client client)
    (client-state-set! client 'disconnect) )

  ;
  ; Procedures used by the server loop
  ;

  ; Register all pending connections in the CLIENTS list
  ; The event ON-CONNECT will be issued for every new client added.
  (define (process-new-clients)

    (define new-clients

      (let register-new-client ((clients (list)))

        (if (tcp-accept-ready? listener)

          (begin
            (define-values (input output) (tcp-accept listener))
            (define client (make-client input-port: input
                                        output-port: output
                                        output-buffer: ""
                                        state: 'connected ))
            (register-new-client (cons client clients)) )

          clients )))

    (set! clients (append new-clients clients))
    (for-each (lambda (c) (event-handler 'on-connect c)) new-clients) )

  ; Read and parse input from CLIENT
  ; The event ON-READ-INPUT is always triggered, sending the empty string in
  ; case no input was read.
  (define (read-client-input client)
    (define input (client-input-port client))

    (define new-input-string
      (let read-loop ((input-string ""))

        (cond
          ((char-ready? input)
            (define char (read-byte input))

            (cond
              ((not (eof-object? char))
                (read-loop (string-append input-string
                                          (string (integer->char char)))) )
              (else
                input-string )))

          (else
            input-string ))))

    (event-handler 'on-read-input new-input-string client) )


  ; read from the field OUTPUT-BUFFER, reset it and send its value to the client.
  (define (send-client-output client)
    (define output (client-output-buffer client))

    (if (not (string=? output ""))

      (begin
        (condition-case

          (write-string output #f (client-output-port client))

          ((exn i/o net)
           (disconnect-client client)
           (event-handler 'on-connection-lost client) ))

        (client-output-buffer-set! client "") )))

  ; disconnect client if flagged with DISCONNECT in the field STATE
  ; returns #t if client was disconnected, #f otherwise
  (define (disconnect-client-if-flagged client)

    (if (eq? (client-state client) 'disconnect)
      (begin
        (close-input-port (client-input-port client))
        (close-output-port (client-output-port client))
        #t )
      #f ))

  ;
  ; Start the server loop.
  ;
  (define (start port host backlog handler)

    ; FIXME: is this bad?
    (define (m-sleep milliseconds) (file-select #f #f (/ milliseconds 1000)))

    ; Define a listener
    (set! listener (tcp-listen port backlog host))

    (if (procedure? handler)
      (set! event-handler handler)
      (error start "HANDLER is not a procedure") )

    ;
    ; Server loop
    ;
    (let loop ([last-pulse (current-milliseconds)])

      ; adding new connections
      (process-new-clients)

      ; processing clients input
      (for-each read-client-input clients)

      ; disconnect and remove flagged clients from the CLIENTS list
      (set! clients (remove! disconnect-client-if-flagged clients))

      ;
      ; Perform one pulse each <PULSE-MILLISECONDS>ms.
      ;
      (define pulse-diff (- (current-milliseconds) last-pulse))
      (define num-pulses-to-perform
        (inexact->exact (round (/ pulse-diff pulse-milliseconds))))

      (let perform-pulses ([num-pulses num-pulses-to-perform])
        (cond
          ((not (eq? num-pulses 0))
           (event-handler 'on-pulse)
           (perform-pulses (sub1 num-pulses)) )))

      ; sending clients output
      (for-each send-client-output clients)

      (define pulse-mod (modulo pulse-diff pulse-milliseconds))
      (m-sleep (- pulse-milliseconds pulse-mod))

      (loop (+ last-pulse pulse-diff (- pulse-mod))) ))
)

