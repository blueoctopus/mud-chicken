(require-library mud-server telnet)

(module mud-main

  (run
   players)

  (import scheme chicken)
  (use data-structures srfi-1 defstruct matchable miscmacros regex
       (prefix mud-server mud-server:) (prefix telnet telnet:))

  ;
  ; Player structure
  ;
  (defstruct player
    (client         #f) ; the CLIENT object, handled by mud-server
    (input-buffer   "") ; input that was not processed yet
    (commands-queue #f) ; commands queue
    (state          #f) ; player-state
    (character      #f) ; CHARACTER object
    )

  (defstruct character
    (name #f)
    )

  (define-enum player-state->int int->player-state
    PST:welcome
    PST:get-name
    PST:password
    PST:get-password
    PST:playing
    )

  (define players (list)) ; A list of PLAYER objects.

  (define pulse-count 0)

  ;
  ; Client procedures
  ;

  ; Replace "\n" with "\r\n" in STR.
  ;
  ; This is a quick procedure to make able strings where lines are delimited with
  ; "\n" to be delimited with "\r\n", respecting the Telnet protocol line break
  ; format.
  ;
  ; It is not expected that STR will already contain the sequence "\r\n", if it
  ; does, this sequence will be translated to "\r\r\n", which is clearly wrong.
  (define (fix-eol str) (string-translate* str '(("\n" . "\r\n"))))

  ; Wrapper to MUD-SERVER:WRITE-TO-CLIENT, treating STR correctly.
  (define (write-to-client str client)
    (mud-server:write-to-client (telnet:string-to-telnet (fix-eol str)) client) )

  ; Run WRITE-TO-CLIENT for all clients
  (define (write-to-all-clients string)
    (for-each
      (lambda (c) (write-to-client string c))
      mud-server:clients ))

  ; Run WRITE-TO-CLIENT for all clients but CLIENT
  (define (write-to-all-clients-but string client)
    (for-each
      (lambda (c) (if (not (eqv? client c)) (write-to-client string c)))
      mud-server:clients ))

  ; Just wrappers to the DATA field in the CLIENT structure. Since we just store
  ; the PLAYER object there, it's convenient using these wrapper procedures for
  ; clarity.
  (define client-player-set! mud-server:client-data-set!)
  (define client-player      mud-server:client-data)

  ;
  ; Handler procedure
  ;
  (define handler (match-lambda*

    (  ('on-connect client)

       (define player (make-player client:         client
                                   commands-queue: (make-queue) ))
       (client-player-set! client player)

       ; add client to a list of players
       (set! players (cons player players))

       (enter-state player PST:welcome) )

    (  ('on-read-input input client)

       (define player (client-player client))

       (process-input-to-player input player) )

    (  ('on-connection-lost client)

       ; unset client from the player field.
       (player-client-set! (client-player client) #f)

       (write-to-all-clients "Someone lost his connection.\n") )

    (  ('on-pulse)

       (for-each (lambda (p)
                   (define q (player-commands-queue p))
                   (if (not (queue-empty? q))
                     (process-player-state p (queue-remove! q) )))

                 players )

       ; XXX Example of how to deal with multiples of pulse:
       #;(set! pulse-count (+ pulse-count 1))
       #;(if (= (modulo pulse-count 10) 0)
         (write-to-all-clients (conc "Pulse: " pulse-count " mud-server: " (current-milliseconds) "\n")) ))

    (  other-event
       (print (conc "Unhandled event: " other-event)) )))

  ;
  ; Process INPUT, separate it in commands and add each command to the player
  ; commands queue.
  ;
  (define (process-input-to-player input player)
    (define q (player-commands-queue player))

    ; list of telnet tokens: player's input buffer plus newly received input
    (define telnet-list
            (telnet:telnet-to-list (string-append (player-input-buffer player)
                                                  input )))

    (define client (player-client player))

    ;
    ; Adding commands to queue
    ;
    (let process-input-list ((next #f)
                             (rest telnet-list) )

      (cond

        ((eq? next #f)
         #t )

        ((string? next)
         ; TODO: convert string from player's charset to UTF-8

         ; split string on \r and \n
         ; TODO: make sure each command finishes at a line break.
         (define commands (string-split next "\r\n"))

         ; add strings to the player's commands queue
         (for-each (lambda (cmd) (queue-add! q cmd ))
                   commands ))

        ((telnet:command? next)
         ; TODO process interesting Telnet commands here.
         (define str (conc "[Telnet command -"
                                " type: " (telnet:command-type next)
                                " code: " (char->integer (telnet:command-code next))
                                " option: " (if* (telnet:command-option next) (char->integer it))
                                "]\n" ) )
         (write-to-client str client)
         (print str) )

        ((telnet:incomplete-command? next)

         ; append incomplete command to player's input buffer
         (write-to-client "[Telnet incomplete command]\n" client)
         (print (conc "[Telnet incomplete command from " client "]"))

         (player-input-buffer-set! player
                                   (string-append (player-input-buffer player)
                                                  (telnet:incomplete-command-raw-data next) )))

        (else
         (error process-input-list "Unexpected value in TELNET-LIST" next) ))

      (if (pair? rest)
          (if* (telnet:get-next-string rest)
               (process-input-list (first it) (second it))
               (process-input-list (car rest) (cdr rest)) ))))

  ; A convenient way of setting a player state and handling it.
  (define (enter-state player state)
    (player-state-set! player state)
    (process-player-state player "") )

  ;
  ; Process current player state.
  ;
  ; PLAYER
  ;
  ;     The player for which its current state will be processed.
  ;
  ; COMMAND
  ;
  ;     The command that will be processed in the current state, can be
  ;     anything. For instance, in the welcome screen COMMAND will probably
  ;     contain the character name of which the player wishes to login.
  ;
  (define (process-player-state player command)
    (define client (player-client player))

    (select (player-state player)

      ((PST:welcome)
       (define welcome-message #<<WELCOME-MESSAGE
                           Welcome to this MUD!

Please type the name of your character: 
WELCOME-MESSAGE
       )

       (write-to-client welcome-message client)
       (player-state-set! player PST:get-name) )

      ((PST:get-name)
       (cond
         (  (not (valid-name command))
            (write-to-client
              "Invalid name. Please pick a pronounceable name.\nName: "
              client ))

         (  else
            (player-character-set! player (make-character name: command))
            (enter-state player PST:password) )))

      ((PST:password)
       (write-to-client "Password: " client)
       (player-state-set! player PST:get-password) )

      ((PST:get-password)
       (cond
         (  (not (valid-password command))
            (write-to-client
              "Invalid password. Please try again.\nPassword: "
              client ))

         (  else
            (write-to-client #<#MSG

Welcome, #(character-name (player-character player))!


MSG
                             client )
            (player-state-set! player PST:playing) )))

      ((PST:playing)
       (cond
         (  (string-ci=? "quit" command)
            (write-to-client "Bye!\n" client)
            (mud-server:disconnect-client client)
            (write-to-all-clients-but
              (conc (character-name (player-character player))
                    " has quit.\n" )
              client ))

         (  else
            (write-to-client (string-append "You said, \"" command "\"\n")
                             client)
            (write-to-all-clients-but
              (conc (character-name (player-character player))
                    " said, \"" command "\"\n" )
              client ))))))

  (define (valid-name str)
     ;
     ; Name validation
     ;
     ; No more than 3 vowels or 3 consonants in sequence
     ; Must have at least one vowel and one consonant (no vvv or ccc)
     ; At least 3 letters
     ;
     (define vowel '("aeiouy"))
     (define consonant '("bcdfghjklmnpqrstvwxz"))
     (not (string-match
            `(or (: (* any) (>= 4   ,vowel    ) (* any))
                 (: (* any) (>= 4   ,consonant) (* any))
                 (: bos     (** 0 2 any       ) eos    )
                 (: bos     (*      ,vowel    ) eos    )
                 (: bos     (*      ,consonant) eos    ) )
            str )) )

  (define (valid-password str)
    (equal? str "xxx") )

  ;
  ; Run
  ;
  (define (run port host)
    (print (conc "Listening on " host ":" port))

    ; TODO: load database

    (print (mud-server:start port host 4 handler)) )

)

