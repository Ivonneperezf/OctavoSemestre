#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <winsock2.h>

#pragma comment(lib, "ws2_32.lib") // Vincular la biblioteca Winsock

#define PORT 60000
#define BUFFER_SIZE 1024

int main() {
    WSADATA wsaData;
    SOCKET client_socket;
    struct sockaddr_in server_address;
    char buffer[BUFFER_SIZE];

    // Inicializar Winsock
    if (WSAStartup(MAKEWORD(2, 2), &wsaData) != 0) {
        printf("WSAStartup failed: %d\n", WSAGetLastError());
        return 1;
    }

    // Crear socket
    if ((client_socket = socket(AF_INET, SOCK_STREAM, 0)) == INVALID_SOCKET) {
        printf("Socket creation failed: %d\n", WSAGetLastError());
        WSACleanup();
        return 1;
    }

    // Configurar la dirección del servidor
    server_address.sin_family = AF_INET;
    server_address.sin_port = htons(PORT);
    server_address.sin_addr.s_addr = inet_addr("127.0.0.1"); // Cambiar a la IP del servidor si no está local

    // Conectar al servidor
    if (connect(client_socket, (struct sockaddr*)&server_address, sizeof(server_address)) == SOCKET_ERROR) {
        printf("Connection failed: %d\n", WSAGetLastError());
        closesocket(client_socket);
        WSACleanup();
        return 1;
    }

    printf("Connected to server.\n");

    // Enviar mensaje al servidor
    const char* message = "HelloServer!";
    send(client_socket, message, strlen(message), 0);

    // Recibir y mostrar datos
    FILE* file = fopen("received.txt", "wb");
    if (file == NULL) {
        printf("Error creating file.\n");
        closesocket(client_socket);
        WSACleanup();
        return 1;
    }

    printf("file opened\n");
    int bytes_received;
    while ((bytes_received = recv(client_socket, buffer, BUFFER_SIZE, 0)) > 0) {
        fwrite(buffer, 1, bytes_received, file);
        buffer[bytes_received] = '\0';  // Asegurarse de que el buffer esté correctamente terminado
        printf("receiving data...\n");
        printf("Data => %s\n", buffer);  // Mostrar el texto recibido
    }

    fclose(file);
    printf("File received successfully.\n");

    // Cerrar conexión
    closesocket(client_socket);
    WSACleanup();
    return 0;
}
