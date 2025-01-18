import java.io.*;
import java.net.*;

public class Client {
    public static void main(String[] args) {
        Socket socket = null;
        try {
            String host = InetAddress.getLocalHost().getHostName();
            int port = 9999;

            // Crear una conexión con el servidor
            socket = new Socket(host, port);

            // Recibir el mensaje del servidor
            BufferedReader reader = new BufferedReader(new InputStreamReader(socket.getInputStream()));
            String timeMessage = reader.readLine();

            // Mostrar el mensaje
            System.out.println("Time connection server: " + timeMessage);

        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            try {
                if (socket != null) {
                    socket.close();
                }
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }
}
