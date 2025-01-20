import socket

class Client:
    def __init__(self):
        self.s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        self.host=socket.gethostname()
        self.port=50000

    def establecerConexion(self):
        self.s.connect((self.host,self.port))
        self.s.send('Hola, se establecio la comunicacion'.encode())
        data, addr = self.s.recvfrom(1024)  # Escuchar respuesta
        print(f"Respuesta del servidor: {data.decode('utf-8')}")
    

def main():
    client = Client()
    client.establecerConexion()

if __name__ == "__main__":
    main()