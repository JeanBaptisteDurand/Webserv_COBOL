       IDENTIFICATION DIVISION.
       PROGRAM-ID. WEBSERVER.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       *> Constantes et variables
       77  WS-SOCKET            PIC S9(9) COMP.
       77  WS-NEW-SOCKET        PIC S9(9) COMP.
       77  WS-BIND-STATUS       PIC S9(9) COMP.
       77  WS-LISTEN-STATUS     PIC S9(9) COMP.
       77  WS-ACCEPT-STATUS     PIC S9(9) COMP.
       77  WS-RECV-STATUS       PIC S9(9) COMP.
       77  WS-SEND-STATUS       PIC S9(9) COMP.
       
       *> Adresse du serveur
       01  WS-SOCK-ADDR-STRUCT.
           05 WS-SOCK-FAMILY      PIC 9(4) COMP VALUE 2. *> AF_INET
           05 WS-SOCK-PORT        PIC 9(4) COMP.
           05 WS-SOCK-ADDR        PIC X(4). *> INADDR_ANY (0.0.0.0)
           05 WS-SOCK-ZERO        PIC X(8) VALUE SPACES.
           
       *> Buffer de réception et envoi
       77  WS-BUFFER             PIC X(1024) VALUE SPACES.
       77  WS-RESPONSE           PIC X(512) VALUE 
           "HTTP/1.1 200 OK" & X"0D0A" &
           "Content-Type: text/html" & X"0D0A" &
           "Content-Length: 49" & X"0D0A" &
           X"0D0A" &
           "<html><body><h1>Hello COBOL Webserver!</h1></body></html>".

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           *> Créer la socket
           CALL "socket" USING 
               BY VALUE 2       *> AF_INET
               BY VALUE 1       *> SOCK_STREAM
               BY VALUE 0       *> IPPROTO_TCP
               RETURNING WS-SOCKET.

           IF WS-SOCKET < 0
               DISPLAY "Erreur création socket"
               STOP RUN.

           *> Définir le port (8080 en Big Endian)
           MOVE FUNCTION REVERSE("1F90") TO WS-SOCK-PORT.

           *> Lier la socket à l'adresse et au port
           CALL "bind" USING 
               BY VALUE WS-SOCKET 
               BY REFERENCE WS-SOCK-ADDR-STRUCT 
               BY VALUE 16 
               RETURNING WS-BIND-STATUS.

           IF WS-BIND-STATUS < 0
               DISPLAY "Erreur bind"
               STOP RUN.

           *> Mettre en écoute
           CALL "listen" USING 
               BY VALUE WS-SOCKET 
               BY VALUE 5 
               RETURNING WS-LISTEN-STATUS.

           IF WS-LISTEN-STATUS < 0
               DISPLAY "Erreur listen"
               STOP RUN.

           DISPLAY "Serveur en écoute sur le port 8080".

       ACCEPT-LOOP.
           *> Accepter une connexion entrante
           CALL "accept" USING 
               BY VALUE WS-SOCKET 
               BY REFERENCE WS-SOCK-ADDR-STRUCT 
               BY VALUE 16 
               RETURNING WS-NEW-SOCKET.

           IF WS-NEW-SOCKET < 0
               DISPLAY "Erreur accept"
               STOP RUN.

           *> Lire la requête HTTP
           CALL "recv" USING 
               BY VALUE WS-NEW-SOCKET 
               BY REFERENCE WS-BUFFER 
               BY VALUE 1024 
               BY VALUE 0 
               RETURNING WS-RECV-STATUS.

           IF WS-RECV-STATUS > 0
               DISPLAY "Requête reçue: "
               DISPLAY WS-BUFFER.

           *> Envoyer la réponse HTTP
           CALL "send" USING 
               BY VALUE WS-NEW-SOCKET 
               BY REFERENCE WS-RESPONSE 
               BY VALUE FUNCTION LENGTH(WS-RESPONSE) 
               BY VALUE 0 
               RETURNING WS-SEND-STATUS.

           *> Fermer la connexion
           CALL "close" USING BY VALUE WS-NEW-SOCKET.

           GO TO ACCEPT-LOOP.

       STOP RUN.
