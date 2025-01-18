import java.io.*;
import java.net.*;

public class Server2 {
    public static void main(String[] args) {
        ServerSocket serverSocket = null;
        Socket clientSocket = null;
        DataInputStream inputStream = null;
        DataOutputStream outputStream = null;
        FileInputStream fileInputStream = null;
        try {
            String host = InetAddress.getLocalHost().getHostName();
            int port = 60000;

            // Crear el servidor socket
            serverSocket = new ServerSocket(port);
            System.out.println("Server listening...");

            while (true) {
                // Aceptar una conexión
                clientSocket = serverSocket.accept();
                System.out.println("Got connection from " + clientSocket.getInetAddress());

                // Crear flujos de entrada y salida
                inputStream = new DataInputStream(clientSocket.getInputStream());
                outputStream = new DataOutputStream(clientSocket.getOutputStream());

                // Recibir datos del cliente
                String data = inputStream.readUTF();
                System.out.println("Server received: " + data);

                // Enviar el archivo al cliente
                String filename = "mytext.txt";
                fileInputStream = new FileInputStream(filename);
                byte[] buffer = new byte[1024];
                int bytesRead;
                System.out.println("Sending file...");

                while ((bytesRead = fileInputStream.read(buffer)) != -1) {
                    outputStream.write(buffer, 0, bytesRead);
                    System.out.println("Sent: " + new String(buffer, 0, bytesRead));
                }

                fileInputStream.close();
                System.out.println("File sent successfully.");
            }

        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            try {
                if (inputStream != null) inputStream.close();
                if (outputStream != null) outputStream.close();
                if (serverSocket != null) serverSocket.close();
                if (clientSocket != null) clientSocket.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }
}
