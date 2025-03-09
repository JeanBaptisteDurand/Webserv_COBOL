       * client_handler.cpy
       * HANDLE-CLIENT processes an established client connection:
       * it receives data, sends an HTTP response based on the request path,
       * and then closes the connection.
       HANDLE-CLIENT.
           *> Receive data: recv(WS-CLIENT-FD, WS-BUFFER, 1024, 0)
           CALL "recv" USING 
               *> BY VALUE WS-CLIENT-FD (client socket)
               BY VALUE WS-CLIENT-FD
               *> BY REFERENCE WS-BUFFER (buffer for receiving)
               BY REFERENCE WS-BUFFER
               *> BY VALUE 1024 (buffer size)
               BY VALUE 1024
               *> BY VALUE 0 (flags)
               BY VALUE 0
               *> RETURNING WS-RECV-STATUS
               RETURNING WS-RECV-STATUS.
           DISPLAY "Requête reçue sur fd " WS-CLIENT-FD ": " WS-BUFFER.
           IF WS-RECV-STATUS <= 0
               CALL "close" USING 
                   *> BY VALUE WS-CLIENT-FD (close client socket)
                   BY VALUE WS-CLIENT-FD.
               EXIT PERFORM
           END-IF.
           *> Extract the path from the request (simplified: starting at character 5)
           MOVE WS-BUFFER(5:256) TO WS-PATH.
           IF WS-PATH = "/"
               MOVE "HTTP/1.1 200 OK" & X"0D0A" &
                    "Content-Type: text/html" & X"0D0A" &
                    "Content-Length: 49" & X"0D0A" &
                    X"0D0A" &
                    "<html><body><h1>Hello COBOL Webserver!</h1></body></html>" 
                   TO WS-RESPONSE
           ELSE IF WS-PATH = "/api"
               MOVE "HTTP/1.1 200 OK" & X"0D0A" &
                    "Content-Type: text/plain" & X"0D0A" &
                    "Content-Length: 13" & X"0D0A" &
                    X"0D0A" &
                    "API Response" 
                   TO WS-RESPONSE
           ELSE
               MOVE "HTTP/1.1 404 Not Found" & X"0D0A" &
                    "Content-Type: text/plain" & X"0D0A" &
                    "Content-Length: 13" & X"0D0A" &
                    X"0D0A" &
                    "404 Not Found" 
                   TO WS-RESPONSE
           END-IF.
           MOVE FUNCTION LENGTH(WS-RESPONSE) TO WS-LEN.
           *> Send the response: send(WS-CLIENT-FD, WS-RESPONSE, WS-LEN, 0)
           CALL "send" USING 
               *> BY VALUE WS-CLIENT-FD (client socket)
               BY VALUE WS-CLIENT-FD
               *> BY REFERENCE WS-RESPONSE (response buffer)
               BY REFERENCE WS-RESPONSE
               *> BY VALUE WS-LEN (response length)
               BY VALUE WS-LEN
               *> BY VALUE 0 (flags)
               BY VALUE 0
               *> RETURNING WS-SEND-STATUS
               RETURNING WS-SEND-STATUS.
           DISPLAY "Réponse envoyée sur fd: " WS-CLIENT-FD.
           CALL "close" USING 
               *> BY VALUE WS-CLIENT-FD (close client socket)
               BY VALUE WS-CLIENT-FD.
           EXIT.
