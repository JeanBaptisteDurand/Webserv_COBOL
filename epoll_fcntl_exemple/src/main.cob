       IDENTIFICATION DIVISION.
       PROGRAM-ID. WEBSERVER.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       *--------------------------------------------------------------
       * Socket descriptors and status variables.
       77 WS-SERVER-SOCKET    PIC S9(9) COMP.
       77 WS-CLIENT-FD        PIC S9(9) COMP.
       77 WS-EPOLL-FD         PIC S9(9) COMP.
       77 WS-BIND-STATUS      PIC S9(9) COMP.
       77 WS-LISTEN-STATUS    PIC S9(9) COMP.
       77 WS-EPOLL-READY      PIC S9(9) COMP.
       77 WS-INDEX            PIC S9(9) COMP.

       * Temporary variable for passing file descriptors to routines.
       77 WS-FD               PIC S9(9) COMP.
       77 WS-FLAGS            PIC S9(9) COMP.
       77 WS-RET              PIC S9(9) COMP.

       *--------------------------------------------------------------
       * sockaddr_in structure (16 bytes):
       *  sin_family (2 bytes): AF_INET = X"0200"
       *  sin_port   (2 bytes): 8080 = X"1F90" (network order)
       *  sin_addr   (4 bytes): INADDR_ANY = X"00000000"
       *  sin_zero   (8 bytes): Padding = X"0000000000000000"
       01 WS-SOCK-ADDR-STRUCT PIC X(16)
            VALUE X"0200"  *> AF_INET
                  & X"1F90" *> Port 8080
                  & X"00000000"  *> INADDR_ANY
                  & X"0000000000000000".  *> Padding

       *--------------------------------------------------------------
       * Client address structure (used by accept)
       01 WS-CLIENT-ADDR      PIC X(16) VALUE SPACES.
       77 WS-CLIENT-ADDR-LEN   PIC S9(9) COMP VALUE 16.

       *--------------------------------------------------------------
       * Buffers for HTTP request and response.
       77 WS-BUFFER           PIC X(1024) VALUE SPACES.
       77 WS-RESPONSE         PIC X(512) VALUE SPACES.
       77 WS-LEN              PIC S9(9) COMP.
       01 WS-PATH             PIC X(256) VALUE SPACES.

       *--------------------------------------------------------------
       * Array of epoll events (simulate struct epoll_event, 12 bytes per event):
       *  - WS-EVENTS: first 4 bytes (PIC X(4)) for events.
       *  - WS-DATA:   next 8 bytes (we now use PIC X(8)) for user data.
       01 WS-EPOLL-EVENTS-ARRAY.
           05 WS-EPOLL-EVENT OCCURS 10 TIMES.
              10 WS-EVENTS   PIC X(4).
              10 WS-DATA     PIC X(8).
       77 MAX-EVENTS PIC S9(9) COMP VALUE 10.

       *--------------------------------------------------------------
       * Template epoll_event structure (12 bytes total):
       * We model the structure as a group:
       *   WS-EVENTS-FIELD: 4 bytes for the event mask.
       *   WS-DATA-FIELD:   8 bytes for user data.
       * For EPOLLIN (0x001) in little-endian, the events field is stored as X"01 00 00 00"
       * and initially, we set the data field to 0.
       01 WS-EPOLL-EVENT-STRUCT.
          05 WS-EVENTS-FIELD   PIC X(4) VALUE X"01000000".
          05 WS-DATA-FIELD     PIC S9(18) COMP VALUE 0.
       *--------------------------------------------------------------

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           *> Create a TCP socket: socket(AF_INET, SOCK_STREAM, IPPROTO_TCP)
           CALL "socket" USING 
               *> BY VALUE 2       (AF_INET)
               BY VALUE 2
               *> BY VALUE 1       (SOCK_STREAM)
               BY VALUE 1
               *> BY VALUE 0       (IPPROTO_TCP)
               BY VALUE 0
               *> RETURNING WS-SERVER-SOCKET
               RETURNING WS-SERVER-SOCKET.
           IF WS-SERVER-SOCKET < 0
               DISPLAY "Erreur lors de la création de la socket"
               STOP RUN.
           DISPLAY "Socket créée, valeur : " WS-SERVER-SOCKET.

           *> Set the server socket to non-blocking.
           MOVE WS-SERVER-SOCKET TO WS-FD.
           PERFORM SET-NONBLOCKING.

           *> Bind the server socket: bind(WS-SERVER-SOCKET, WS-SOCK-ADDR-STRUCT, 16)
           CALL "bind" USING 
               *> BY VALUE WS-SERVER-SOCKET (server socket FD)
               BY VALUE WS-SERVER-SOCKET
               *> BY REFERENCE WS-SOCK-ADDR-STRUCT (address structure)
               BY REFERENCE WS-SOCK-ADDR-STRUCT
               *> BY VALUE 16 (size of structure)
               BY VALUE 16
               *> RETURNING WS-BIND-STATUS
               RETURNING WS-BIND-STATUS.
           IF WS-BIND-STATUS < 0
               DISPLAY "Erreur bind"
               STOP RUN.
           DISPLAY "Bind OK, statut : " WS-BIND-STATUS.

           *> Listen on the server socket: listen(WS-SERVER-SOCKET, 5)
           CALL "listen" USING 
               *> BY VALUE WS-SERVER-SOCKET (server socket FD)
               BY VALUE WS-SERVER-SOCKET
               *> BY VALUE 5 (backlog)
               BY VALUE 5
               *> RETURNING WS-LISTEN-STATUS
               RETURNING WS-LISTEN-STATUS.
           IF WS-LISTEN-STATUS < 0
               DISPLAY "Erreur listen"
               STOP RUN.
           DISPLAY "Listen OK, statut : " WS-LISTEN-STATUS.

           *> Create an epoll instance: epoll_create1(0)
           CALL "epoll_create1" USING 
               *> BY VALUE 0 (flags)
               BY VALUE 0
               *> RETURNING WS-EPOLL-FD
               RETURNING WS-EPOLL-FD.
           IF WS-EPOLL-FD < 0
               DISPLAY "Erreur epoll_create1"
               STOP RUN.
           DISPLAY "Epoll FD : " WS-EPOLL-FD.

           *> Prepare the epoll event for the server socket:
           *> Set events to EPOLLIN and store the server FD in WS-DATA-FIELD.
           MOVE WS-SERVER-SOCKET TO WS-DATA-FIELD OF WS-EPOLL-EVENT-STRUCT.
           *> (WS-EVENTS-FIELD already holds EPOLLIN in little-endian)

           *> Add the server socket to the epoll instance.
           MOVE WS-SERVER-SOCKET TO WS-FD.
           PERFORM ADD-TO-EPOLL.

           DISPLAY "Serveur en écoute sur le port 8080".

           PERFORM UNTIL FALSE
               *> Wait for events: epoll_wait(WS-EPOLL-FD, WS-EPOLL-EVENTS-ARRAY, MAX-EVENTS, -1)
               CALL "epoll_wait" USING 
                   *> BY VALUE WS-EPOLL-FD (epoll instance)
                   BY VALUE WS-EPOLL-FD
                   *> BY REFERENCE WS-EPOLL-EVENTS-ARRAY (array for events)
                   BY REFERENCE WS-EPOLL-EVENTS-ARRAY
                   *> BY VALUE MAX-EVENTS (max events)
                   BY VALUE MAX-EVENTS
                   *> BY VALUE -1 (timeout: block indefinitely)
                   BY VALUE -1
                   *> RETURNING WS-EPOLL-READY (number of events)
                   RETURNING WS-EPOLL-READY.
               IF WS-EPOLL-READY < 0
                   DISPLAY "Erreur epoll_wait"
               ELSE
                   PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > WS-EPOLL-READY
                       *> Check the data field in the event:
                       *> If WS-DATA equals WS-SERVER-SOCKET then it's a new connection.
                       IF FUNCTION NUMVAL (WS-DATA (WS-INDEX)) = WS-SERVER-SOCKET
                           PERFORM HANDLE-NEW-CLIENT
                       ELSE
                           MOVE FUNCTION NUMVAL (WS-DATA (WS-INDEX)) TO WS-CLIENT-FD.
                           PERFORM HANDLE-CLIENT
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM.
           STOP RUN.

       *> Include routines via COPY
       COPY "network_utils.cpy".
       COPY "epoll_utils.cpy".
       COPY "new_client_handler.cpy".
       COPY "client_handler.cpy".
