       * new_client_handler.cpy
       * HANDLE-NEW-CLIENT accepts a new connection on the server socket,
       * sets the client socket to non-blocking mode, and adds it to epoll.
       HANDLE-NEW-CLIENT.
           *> Accept a new connection: accept(WS-SERVER-SOCKET, WS-CLIENT-ADDR, WS-CLIENT-ADDR-LEN)
           CALL "accept" USING 
               *> BY VALUE WS-SERVER-SOCKET (server socket)
               BY VALUE WS-SERVER-SOCKET
               *> BY REFERENCE WS-CLIENT-ADDR (client address structure)
               BY REFERENCE WS-CLIENT-ADDR
               *> BY REFERENCE WS-CLIENT-ADDR-LEN (address length)
               BY REFERENCE WS-CLIENT-ADDR-LEN
               *> RETURNING WS-CLIENT-FD (new client socket)
               RETURNING WS-CLIENT-FD.
           IF WS-CLIENT-FD < 0
               DISPLAY "Erreur accept dans HANDLE-NEW-CLIENT"
               EXIT PERFORM
           END-IF.
           DISPLAY "Nouvelle connexion acceptée, fd: " WS-CLIENT-FD.
           *> Set the client socket to non-blocking:
           MOVE WS-CLIENT-FD TO WS-FD.
           PERFORM SET-NONBLOCKING.
           *> Prepare the epoll event for the client socket:
           MOVE WS-CLIENT-FD TO WS-DATA-FIELD OF WS-EPOLL-EVENT-STRUCT.
           *> (WS-EVENTS-FIELD remains EPOLLIN)
           *> Add the client socket to epoll:
           MOVE WS-CLIENT-FD TO WS-FD.
           PERFORM ADD-TO-EPOLL.
           EXIT.
