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

# Code c dexemple pour le projet

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <sys/epoll.h>

// Configure le descripteur fd en mode non bloquant.
// Retourne 0 en cas de succès, -1 en cas d'erreur.
int set_nonblocking(int fd) {
    int flags = fcntl(fd, F_GETFL, 0);
    if (flags == -1)
        return -1;
    flags |= O_NONBLOCK;
    return fcntl(fd, F_SETFL, flags);
}

// Ajoute le descripteur fd à l'instance epoll identifiée par epoll_fd
// avec les événements spécifiés. Retourne 0 en cas de succès, -1 en cas d'erreur.
int add_to_epoll(int epoll_fd, int fd, uint32_t events) {
    struct epoll_event event;
    event.events = events;
    event.data.fd = fd;
    return epoll_ctl(epoll_fd, EPOLL_CTL_ADD, fd, &event);
}

// Gère l'acceptation d'une nouvelle connexion sur server_fd,
// configure la socket client en non bloquant et l'ajoute à epoll.
void handle_new_client(int server_fd, int epoll_fd, struct sockaddr_in *address, socklen_t *addrlen) {
    int client_fd = accept(server_fd, (struct sockaddr *)address, addrlen);
    if (client_fd < 0) {
        perror("accept");
        return;
    }
    printf("Nouvelle connexion acceptée, fd: %d\n", client_fd);

    if (set_nonblocking(client_fd) < 0) {
        perror("set_nonblocking (client_fd) failed");
        close(client_fd);
        return;
    }
    if (add_to_epoll(epoll_fd, client_fd, EPOLLIN) < 0) {
        perror("epoll_ctl: client_fd");
        close(client_fd);
        return;
    }
}

// Traite la requête HTTP reçue sur la socket client_fd et renvoie la réponse adéquate.
void handle_client(int client_fd) {
    char buffer[512];
    ssize_t count = read(client_fd, buffer, sizeof(buffer) - 1);
    if (count <= 0) {
        if (count < 0)
            perror("read error");
        close(client_fd);
        printf("Fermeture de la connexion, fd: %d\n", client_fd);
        return;
    }
    buffer[count] = '\0';

    // Parsing de la première ligne de la requête HTTP : ex. "GET /api HTTP/1.1"
    char method[8], path[256], protocol[16];
    if (sscanf(buffer, "%7s %255s %15s", method, path, protocol) != 3) {
        close(client_fd);
        return;
    }

    const char *response;
    if (strcmp(path, "/") == 0) {
        response = "HTTP/1.1 200 OK\r\n"
                   "Content-Type: text/html\r\n"
                   "Content-Length: 49\r\n\r\n"
                   "<html><body><h1>Hello COBOL Webserver!</h1></body></html>";
    } else if (strcmp(path, "/api") == 0) {
        response = "HTTP/1.1 200 OK\r\n"
                   "Content-Type: text/plain\r\n"
                   "Content-Length: 13\r\n\r\n"
                   "API Response";
    } else {
        response = "HTTP/1.1 404 Not Found\r\n"
                   "Content-Type: text/plain\r\n"
                   "Content-Length: 13\r\n\r\n"
                   "404 Not Found";
    }

    write(client_fd, response, strlen(response));
    close(client_fd);
    printf("Réponse envoyée et connexion fermée (fd: %d)\n", client_fd);
}

#define PORT 8080
#define MAX_EVENTS 10

int main(void) {
    int server_fd, epoll_fd;
    struct sockaddr_in address;
    int addrlen = sizeof(address);
    struct epoll_event events[MAX_EVENTS];

    // Création de la socket serveur (IPv4, TCP)
    server_fd = socket(AF_INET, SOCK_STREAM, 0);
    if (server_fd < 0) {
        perror("socket failed");
        exit(EXIT_FAILURE);
    }
    if (set_nonblocking(server_fd) < 0) {
        perror("set_nonblocking failed");
        exit(EXIT_FAILURE);
    }

    // Configuration de l'adresse d'écoute
    address.sin_family = AF_INET;
    address.sin_addr.s_addr = INADDR_ANY;
    address.sin_port = htons(PORT);
    if (bind(server_fd, (struct sockaddr *)&address, sizeof(address)) < 0) {
        perror("bind failed");
        exit(EXIT_FAILURE);
    }
    if (listen(server_fd, 10) < 0) {
        perror("listen failed");
        exit(EXIT_FAILURE);
    }

    // Création de l'instance epoll
    epoll_fd = epoll_create1(0);
    if (epoll_fd < 0) {
        perror("epoll_create1 failed");
        exit(EXIT_FAILURE);
    }
    if (add_to_epoll(epoll_fd, server_fd, EPOLLIN) < 0) {
        perror("epoll_ctl: server_fd");
        exit(EXIT_FAILURE);
    }
    printf("Serveur en écoute sur le port %d\n", PORT);

    // Boucle principale : gestion des événements epoll
    while (1) {
        int n = epoll_wait(epoll_fd, events, MAX_EVENTS, -1);
        if (n < 0) {
            perror("epoll_wait");
            break;
        }
        for (int i = 0; i < n; i++) {
            if (events[i].data.fd == server_fd) {
                // Nouvel événement sur la socket d'écoute
                handle_new_client(server_fd, epoll_fd, &address, (socklen_t *)&addrlen);
            } else {
                // Événement sur une socket client acceptée
                handle_client(events[i].data.fd);
            }
        }
    }

    close(server_fd);
    return 0;
}
```

