# Webserv_COBOL

Creation of a basic webserv using cobol

making call to the c/linux function to manage the socket

amelio possible : comment gerer une monter en charge, fork, thread, asynch

how to debug : strace -e trace=bind ./webserver


"In this project, we encountered a critical issue with our socket address structure during the bind call. The problem arose from the definition of the sin_family field: we originally used the hexadecimal literal X"0002" to represent AF_INET (which should be 2). However, on a little-endian system, a 16-bit integer with the value 2 must be stored as 0x02 followed by 0x00. When using X"0002", the bytes are stored as 0x00 then 0x02, which is interpreted in little-endian order as 0x0200 (512) instead of 2. This discrepancy led to the bind call failing with the error EAFNOSUPPORT. The solution was to reverse the byte order in our structure definition by using X"0200" for the sin_family field, ensuring it is stored as 0x02 0x00 in memory—exactly as required by the system. This fix resolved the bind error by correctly specifying the address family."

```
# BEFORE:
       *> Define a contiguous 16-byte structure for sockaddr_in.
       01 WS-SOCK-ADDR-STRUCT PIC X(16)
            VALUE 
                X"0002"      &  *> sin_family: AF_INET (2 bytes)
                X"1F90"      &  *> sin_port: 8080 in network order (2 bytes)
                X"00000000"  &  *> sin_addr: INADDR_ANY (4 bytes)
                X"0000000000000000".  *> sin_zero: padding (8 bytes)

Memory Layout (Avant):
    Byte 1: 0x00    (sin_family: high byte)
    Byte 2: 0x02    (sin_family: low byte)
    Byte 3: 0x1F    (sin_port: high byte)
    Byte 4: 0x90    (sin_port: low byte)
    Byte 5-8: 0x00  (sin_addr)
    Byte 9-16: 0x00 (sin_zero)

Interpretation on a little-endian system:
    The two bytes 0x00 0x02 are read as 0x0200, which equals 512 instead of 2.

# AFTER:
       *> Define a contiguous 16-byte structure for sockaddr_in.
       01 WS-SOCK-ADDR-STRUCT PIC X(16)
            VALUE 
                X"0200"      &  *> sin_family: AF_INET (2 bytes, correctly stored)
                X"1F90"      &  *> sin_port: 8080 in network order (2 bytes)
                X"00000000"  &  *> sin_addr: INADDR_ANY (4 bytes)
                X"0000000000000000".  *> sin_zero: padding (8 bytes)

Memory Layout (Après):
    Byte 1: 0x02    (sin_family: low byte)
    Byte 2: 0x00    (sin_family: high byte)
    Byte 3: 0x1F    (sin_port: high byte)
    Byte 4: 0x90    (sin_port: low byte)
    Byte 5-8: 0x00  (sin_addr)
    Byte 9-16: 0x00 (sin_zero)

Interpretation on a little-endian system:
    The two bytes 0x02 0x00 are read as 0x0002, which equals 2 (AF_INET) as expected.
```



