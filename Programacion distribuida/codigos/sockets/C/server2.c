#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <winsock2.h>

#pragma comment(lib, "ws2_32.lib") // Vincular la biblioteca Winsock

#define PORT 60000
#define BUFFER_SIZE 1024

int main() {
    WSADATA wsaData;
    SOCKET server_socket, client_socket;
    struct sockaddr_in server_address, client_address;
    int addrlen = sizeof(client_address);
    char buffer[BUFFER_SIZE];

    // Inicializar Winsock
    if (WSAStartup(MAKEWORD(2, 2), &wsaData) != 0) {
        printf("WSAStartup failed: %d\n", WSAGetLastError());
        return 1;
    }

    // Crear socket
    if ((server_socket = socket(AF_INET, SOCK_STREAM, 0)) == INVALID_SOCKET) {
        printf("Socket creation failed: %d\n", WSAGetLastError());
        WSACleanup();
        return 1;
    }

    // Configurar la dirección del servidor
    server_address.sin_family = AF_INET;
    server_address.sin_addr.s_addr = INADDR_ANY;
    server_address.sin_port = htons(PORT);

    // Enlazar el socket
    if (bind(server_socket, (struct sockaddr*)&server_address, sizeof(server_address)) == SOCKET_ERROR) {
        printf("Bind failed: %d\n", WSAGetLastError());
        closesocket(server_socket);
        WSACleanup();
        return 1;
    }

    // Escuchar conexiones
    if (listen(server_socket, 15) == SOCKET_ERROR) {
        printf("Listen failed: %d\n", WSAGetLastError());
        closesocket(server_socket);
        WSACleanup();
        return 1;
    }

    printf("Server listening....\n");

    while (1) {
        // Aceptar conexión del cliente
        client_socket = accept(server_socket, (struct sockaddr*)&client_address, &addrlen);
        if (client_socket == INVALID_SOCKET) {
            printf("Accept failed: %d\n", WSAGetLastError());
            continue;
        }

        printf("Got connection from %s:%d\n",
               inet_ntoa(client_address.sin_addr), ntohs(client_address.sin_port));

        // Recibir datos del cliente
        int bytes_received = recv(client_socket, buffer, BUFFER_SIZE - 1, 0);
        if (bytes_received <= 0) {
            printf("Failed to receive data.\n");
            closesocket(client_socket);
            continue;
        }
        buffer[bytes_received] = '\0';
        printf("Server received: %s\n", buffer);

        // Enviar texto al cliente
        const char* message = "Hola, este es un archivo de prueba.\r\nEstá siendo enviado desde el servidor.\r\n¡Espero que lo recibas correctamente!";
        send(client_socket, message, strlen(message), 0);
        printf("Send: '%s'\n", message);

        closesocket(client_socket);
    }

    // Limpiar recursos
    closesocket(server_socket);
    WSACleanup();
    return 0;
}
