       * network_utils.cpy
       * SET-NONBLOCKING sets the file descriptor in WS-FD to non-blocking mode.
       SET-NONBLOCKING.
           *> Call fcntl(WS-FD, F_GETFL, 0) to get current flags.
           CALL "fcntl" USING 
               *> BY VALUE WS-FD (file descriptor)
               BY VALUE WS-FD
               *> BY VALUE 3 (F_GETFL)
               BY VALUE 3
               *> BY VALUE 0 (unused parameter)
               BY VALUE 0
               *> RETURNING WS-FLAGS
               RETURNING WS-FLAGS.
           IF WS-FLAGS < 0
               EXIT PERFORM
           END-IF.
           *> Add the O_NONBLOCK flag (2048).
           ADD 2048 TO WS-FLAGS.
           *> Set new flags: fcntl(WS-FD, F_SETFL, WS-FLAGS)
           CALL "fcntl" USING 
               *> BY VALUE WS-FD (file descriptor)
               BY VALUE WS-FD
               *> BY VALUE 4 (F_SETFL)
               BY VALUE 4
               *> BY VALUE WS-FLAGS (updated flags)
               BY VALUE WS-FLAGS
               *> RETURNING WS-RET
               RETURNING WS-RET.
           EXIT.
