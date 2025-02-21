       IDENTIFICATION DIVISION.
       PROGRAM-ID. WEBSERVER.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       *> Gestion des sockets et codes de retour
       77 WS-SOCKET          PIC S9(9) COMP.
       77 WSNSOCK            PIC S9(9) COMP.
       77 WS-BIND-STATUS     PIC S9(9) COMP.
       77 WS-LISTEN-STATUS   PIC S9(9) COMP.
       77 WS-RECV-STATUS     PIC S9(9) COMP.
       77 WS-SEND-STATUS     PIC S9(9) COMP.

       *> Définition d'une structure contiguë de 16 octets pour sockaddr_in (utilisée pour bind)
       01 WS-SOCK-ADDR-STRUCT PIC X(16)
            VALUE 
                X"0200"      &  *> sin_family : AF_INET en little-endian (0x02 suivi de 0x00)
                X"1F90"      &  *> sin_port   : 8080 en ordre réseau (2 octets)
                X"00000000"  &  *> sin_addr   : INADDR_ANY (4 octets)
                X"0000000000000000".  *> sin_zero   : remplissage (8 octets)

       *> Structure pour l'adresse du client (utilisée par accept)
       01 WS-CLIENT-ADDR      PIC X(16) VALUE SPACES.
       77 WS-CLIENT-ADDR-LEN   PIC S9(9) COMP VALUE 16.

       *> Buffers pour la réception des requêtes et l'envoi de réponses
       77 WS-BUFFER   PIC X(1024) VALUE SPACES.
       77 WS-RESPONSE PIC X(512) VALUE 
           "HTTP/1.1 200 OK" & X"0D0A" &
           "Content-Type: text/html" & X"0D0A" &
           "Content-Length: 49" & X"0D0A" &
           X"0D0A" &
           "<html><body><h1>Hello COBOL Webserver!</h1></body></html>".

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           *> Création d'une socket TCP (AF_INET, SOCK_STREAM, IPPROTO_TCP)
           CALL "socket" USING 
               BY VALUE 2       *> AF_INET
               BY VALUE 1       *> SOCK_STREAM
               BY VALUE 0       *> IPPROTO_TCP
               RETURNING WS-SOCKET.
           DISPLAY "Socket créée, valeur : " WS-SOCKET.
           IF WS-SOCKET < 0
               DISPLAY "Erreur lors de la création de la socket"
               STOP RUN.

           DISPLAY "Structure d'adresse (hex) : " WS-SOCK-ADDR-STRUCT.

           *> Appel de bind en utilisant la structure de 16 octets
           CALL "bind" USING 
               BY VALUE WS-SOCKET 
               BY REFERENCE WS-SOCK-ADDR-STRUCT 
               BY VALUE 16   *> Taille de sockaddr_in (16 octets)
               RETURNING WS-BIND-STATUS.
           DISPLAY "Statut de bind : " WS-BIND-STATUS.
           IF WS-BIND-STATUS < 0
               DISPLAY "Erreur bind"
               STOP RUN.

           *> Passage de la socket en mode écoute
           CALL "listen" USING 
               BY VALUE WS-SOCKET 
               BY VALUE 5 
               RETURNING WS-LISTEN-STATUS.
           DISPLAY "Statut de listen : " WS-LISTEN-STATUS.
           IF WS-LISTEN-STATUS < 0
               DISPLAY "Erreur listen"
               STOP RUN.

           DISPLAY "Serveur en écoute sur le port 8080".

       ACCEPT-LOOP.
           *> Réinitialiser la longueur de l'adresse du client pour accept
           MOVE 16 TO WS-CLIENT-ADDR-LEN.
           *> Acceptation d'une connexion entrante
           CALL "accept" USING 
               BY VALUE WS-SOCKET 
               BY REFERENCE WS-CLIENT-ADDR 
               BY REFERENCE WS-CLIENT-ADDR-LEN
               RETURNING WSNSOCK.
           DISPLAY "Nouvelle socket acceptée : " WSNSOCK.
           IF WSNSOCK < 0
               DISPLAY "Erreur accept"
               STOP RUN.

           *> Réception de la requête HTTP
           CALL "recv" USING 
               BY VALUE WSNSOCK 
               BY REFERENCE WS-BUFFER 
               BY VALUE 1024 
               BY VALUE 0 
               RETURNING WS-RECV-STATUS.
           DISPLAY "Statut de recv : " WS-RECV-STATUS.
           IF WS-RECV-STATUS > 0
               DISPLAY "Requête reçue : " WS-BUFFER.

           *> Envoi de la réponse HTTP
           CALL "send" USING 
               BY VALUE WSNSOCK 
               BY REFERENCE WS-RESPONSE 
               BY VALUE FUNCTION LENGTH(WS-RESPONSE)
               BY VALUE 0 
               RETURNING WS-SEND-STATUS.
           DISPLAY "Statut de send : " WS-SEND-STATUS.

           *> Fermeture de la connexion client
           CALL "close" USING BY VALUE WSNSOCK.
           DISPLAY "Fermeture de la connexion sur la socket : " WSNSOCK.

           GO TO ACCEPT-LOOP.

       STOP RUN.
