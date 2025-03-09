       * epoll_utils.cpy
       * ADD-TO-EPOLL adds the file descriptor in WS-FD to the epoll instance.
       ADD-TO-EPOLL.
           *> Call epoll_ctl(WS-EPOLL-FD, EPOLL_CTL_ADD, WS-FD, WS-EPOLL-EVENT-STRUCT)
           CALL "epoll_ctl" USING 
               *> BY VALUE WS-EPOLL-FD (epoll instance FD)
               BY VALUE WS-EPOLL-FD
               *> BY VALUE 1 (EPOLL_CTL_ADD opcode)
               BY VALUE 1
               *> BY VALUE WS-FD (file descriptor to add)
               BY VALUE WS-FD
               *> BY REFERENCE WS-EPOLL-EVENT-STRUCT (pointer to event structure)
               BY REFERENCE WS-EPOLL-EVENT-STRUCT
               *> RETURNING WS-RET
               RETURNING WS-RET.
           EXIT.
