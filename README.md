# Webserv_COBOL

Creation of a basic webserv using cobol

making call to the c/linux function to manage the socket

amelio possible : comment gerer une monter en charge, fork, thread, asynch

how to debug : strace -e trace=bind ./webserver


pour le probleme de la struct de socket pour letape bind

2. Problème d'Endian et ordre des octets
Dans votre cas, l'erreur indiquée par strace montre que le champ sa_family est lu comme 0x200 (512) alors qu'il devrait être 2 (AF_INET).
Sur un système little-endian, l'ordre en mémoire d'un entier 16 bits ayant la valeur 2 doit être :

Octet 1 : 0x02
Octet 2 : 0x00
Or, si vous définissez ce champ avec le littéral hexadécimal X"0002", en mémoire il sera stocké sous la forme :

Octet 1 : 0x00
Octet 2 : 0x02
Lorsque ce couple d’octets est interprété en little-endian, il donne la valeur 0x0200 (512).
La solution consiste donc à inverser ces octets lors de la définition, c’est-à-dire utiliser X"0200". Ainsi, en mémoire, le premier octet sera 0x02 et le deuxième 0x00, ce qui correspond bien à la valeur 2 attendue par la fonction bind.

ancienne struct :


       *> Define a contiguous 16-byte structure for sockaddr_in.
       01 WS-SOCK-ADDR-STRUCT PIC X(16)
            VALUE 
                X"0002"      &  *> sin_family: AF_INET (2 bytes)
                X"1F90"      &  *> sin_port: 8080 in network order (2 bytes)
                X"00000000"  &  *> sin_addr: INADDR_ANY (4 bytes)
                X"0000000000000000".  *> sin_zero: padding (8 bytes)



