import java.io.*;
import java.net.*;
import java.util.*;

public class Server {
    public static void main(String[] args) {
        ServerSocket serverSocket = null;
        try {
            String host = InetAddress.getLocalHost().getHostName();
            int port = 9999;

            // Crear el servidor socket
            serverSocket = new ServerSocket(port, 5, InetAddress.getByName(host));

            System.out.println("Server listening on " + host + ":" + port);

            while (true) {
                // Aceptar conexiones de los clientes
                Socket clientSocket = serverSocket.accept();
                System.out.println("Connected with " + clientSocket.getInetAddress() + ":" + clientSocket.getPort());

                // Enviar la hora actual al cliente
                PrintWriter writer = new PrintWriter(clientSocket.getOutputStream(), true);
                String currentTime = new Date().toString();
                writer.println(currentTime);

                // Cerrar la conexión con el cliente
                clientSocket.close();
            }

        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            try {
                if (serverSocket != null) {
                    serverSocket.close();
                }
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }
}
