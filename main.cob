       IDENTIFICATION DIVISION.
       PROGRAM-ID. WEBSERVER.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       *> Socket handles and status codes
       77 WS-SOCKET          PIC S9(9) COMP.
       77 WS-NEW-SOCKET      PIC S9(9) COMP.
       77 WS-BIND-STATUS     PIC S9(9) COMP.
       77 WS-LISTEN-STATUS   PIC S9(9) COMP.
       77 WS-ACCEPT-STATUS   PIC S9(9) COMP.
       77 WS-RECV-STATUS     PIC S9(9) COMP.
       77 WS-SEND-STATUS     PIC S9(9) COMP.

       *> Define a contiguous 16-byte structure for sockaddr_in.
       01 WS-SOCK-ADDR-STRUCT PIC X(16)
            VALUE 
                X"0002"      &  *> sin_family: AF_INET (2 bytes)
                X"1F90"      &  *> sin_port: 8080 in network order (2 bytes)
                X"00000000"  &  *> sin_addr: INADDR_ANY (4 bytes)
                X"0000000000000000".  *> sin_zero: padding (8 bytes)

       *> Buffers for receiving request and sending response
       77 WS-BUFFER   PIC X(1024) VALUE SPACES.
       77 WS-RESPONSE PIC X(512) VALUE 
           "HTTP/1.1 200 OK" & X"0D0A" &
           "Content-Type: text/html" & X"0D0A" &
           "Content-Length: 49" & X"0D0A" &
           X"0D0A" &
           "<html><body><h1>Hello COBOL Webserver!</h1></body></html>".

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           *> Create a TCP socket (AF_INET, SOCK_STREAM, IPPROTO_TCP)
           CALL "socket" USING 
               BY VALUE 2       *> AF_INET
               BY VALUE 1       *> SOCK_STREAM
               BY VALUE 0       *> IPPROTO_TCP
               RETURNING WS-SOCKET.
           DISPLAY "Socket created, value: " WS-SOCKET.
           IF WS-SOCKET < 0
               DISPLAY "Erreur création socket"
               STOP RUN.

           DISPLAY "Socket Address Structure (hex): " WS-SOCK-ADDR-STRUCT.

           *> Bind the socket using the 16-byte structure.
           CALL "bind" USING 
               BY VALUE WS-SOCKET 
               BY REFERENCE WS-SOCK-ADDR-STRUCT 
               BY VALUE 16   *> Size of sockaddr_in
               RETURNING WS-BIND-STATUS.
           DISPLAY "Bind status: " WS-BIND-STATUS.
           IF WS-BIND-STATUS < 0
               DISPLAY "Erreur bind"
               STOP RUN.

           *> Put the socket into listening mode.
           CALL "listen" USING 
               BY VALUE WS-SOCKET 
               BY VALUE 5 
               RETURNING WS-LISTEN-STATUS.
           DISPLAY "Listen status: " WS-LISTEN-STATUS.
           IF WS-LISTEN-STATUS < 0
               DISPLAY "Erreur listen"
               STOP RUN.

           DISPLAY "Serveur en écoute sur le port 8080".

       ACCEPT-LOOP.
           *> Accept an incoming connection.
           CALL "accept" USING 
               BY VALUE WS-SOCKET 
               BY REFERENCE WS-SOCK-ADDR-STRUCT 
               BY VALUE 16
               RETURNING WS-NEW-SOCKET.
           DISPLAY "New socket from accept: " WS-NEW-SOCKET.
           IF WS-NEW-SOCKET < 0
               DISPLAY "Erreur accept"
               STOP RUN.

           *> Receive the HTTP request.
           CALL "recv" USING 
               BY VALUE WS-NEW-SOCKET 
               BY REFERENCE WS-BUFFER 
               BY VALUE 1024 
               BY VALUE 0 
               RETURNING WS-RECV-STATUS.
           DISPLAY "Receive status: " WS-RECV-STATUS.
           IF WS-RECV-STATUS > 0
               DISPLAY "Requête reçue: " WS-BUFFER.

           *> Send the HTTP response.
           CALL "send" USING 
               BY VALUE WS-NEW-SOCKET 
               BY REFERENCE WS-RESPONSE 
               BY VALUE FUNCTION LENGTH(WS-RESPONSE) 
               BY VALUE 0 
               RETURNING WS-SEND-STATUS.
           DISPLAY "Send status: " WS-SEND-STATUS.

           *> Close the client connection.
           CALL "close" USING BY VALUE WS-NEW-SOCKET.
           DISPLAY "Closed connection on socket: " WS-NEW-SOCKET.

           GO TO ACCEPT-LOOP.

       STOP RUN.
