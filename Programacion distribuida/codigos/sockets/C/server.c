#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <winsock2.h>
#include <time.h>

#pragma comment(lib, "ws2_32.lib") // Vincular la biblioteca Winsock

#define PORT 9999
#define BUFFER_SIZE 1024

int main() {
    WSADATA wsaData;
    SOCKET server_fd, new_socket;
    struct sockaddr_in address;
    int addrlen = sizeof(address);
    char buffer[BUFFER_SIZE] = {0};

    // Inicializar Winsock
    if (WSAStartup(MAKEWORD(2, 2), &wsaData) != 0) {
        printf("WSAStartup failed: %d\n", WSAGetLastError());
        return 1;
    }

    // Crear socket
    if ((server_fd = socket(AF_INET, SOCK_STREAM, 0)) == INVALID_SOCKET) {
        printf("Socket creation failed: %d\n", WSAGetLastError());
        WSACleanup();
        return 1;
    }

    // Configurar la dirección y el puerto
    address.sin_family = AF_INET;
    address.sin_addr.s_addr = INADDR_ANY;
    address.sin_port = htons(PORT);

    // Enlazar el socket
    if (bind(server_fd, (struct sockaddr*)&address, sizeof(address)) == SOCKET_ERROR) {
        printf("Bind failed: %d\n", WSAGetLastError());
        closesocket(server_fd);
        WSACleanup();
        return 1;
    }

    // Escuchar conexiones
    if (listen(server_fd, 5) == SOCKET_ERROR) {
        printf("Listen failed: %d\n", WSAGetLastError());
        closesocket(server_fd);
        WSACleanup();
        return 1;
    }

    printf("Server listening on port %d\n", PORT);

    while (1) {
        if ((new_socket = accept(server_fd, (struct sockaddr*)&address, &addrlen)) == INVALID_SOCKET) {
            printf("Accept failed: %d\n", WSAGetLastError());
            closesocket(server_fd);
            WSACleanup();
            return 1;
        }

        // Mostrar información del cliente conectado
        printf("Connected with IP: %s, PORT: %d\n",
               inet_ntoa(address.sin_addr), ntohs(address.sin_port));

        // Obtener la hora actual
        time_t now = time(NULL);
        struct tm* local_time = localtime(&now);
        char currentTime[BUFFER_SIZE];
        strftime(currentTime, BUFFER_SIZE, "%c\n", local_time);

        // Enviar la hora actual al cliente
        send(new_socket, currentTime, strlen(currentTime), 0);

        // Cerrar la conexión con el cliente
        closesocket(new_socket);
    }

    // Limpiar recursos de Winsock
    closesocket(server_fd);
    WSACleanup();

    return 0;
}
