import java.io.*;
import java.net.*;

public class Client2 {
    public static void main(String[] args) {
        Socket socket = null;
        DataOutputStream outputStream = null;
        DataInputStream inputStream = null;
        FileOutputStream fileOutputStream = null;
        try {
            String host = InetAddress.getLocalHost().getHostName();
            int port = 60000;

            // Crear la conexión con el servidor
            socket = new Socket(host, port);
            outputStream = new DataOutputStream(socket.getOutputStream());
            inputStream = new DataInputStream(socket.getInputStream());

            // Enviar mensaje al servidor
            String message = "HelloServer!";
            outputStream.writeUTF(message);

            // Crear el archivo para recibir los datos
            File file = new File("received.txt");
            fileOutputStream = new FileOutputStream(file);
            System.out.println("File opened");

            // Recibir datos y escribir en el archivo
            byte[] buffer = new byte[1024];
            int bytesRead;
            System.out.println("Receiving data...");

            while ((bytesRead = inputStream.read(buffer)) != -1) {
                System.out.println("Data => " + new String(buffer, 0, bytesRead));
                fileOutputStream.write(buffer, 0, bytesRead);
            }

            System.out.println("Successfully got the file");

        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            try {
                if (fileOutputStream != null) fileOutputStream.close();
                if (inputStream != null) inputStream.close();
                if (outputStream != null) outputStream.close();
                if (socket != null) socket.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
            System.out.println("Connection closed");
        }
    }
}
