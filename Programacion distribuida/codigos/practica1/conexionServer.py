import threading
import socket

class Conexion:
    def __init__(self):
        self.serversocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

    def realizarConexion(self):
        host=socket.gethostname()
        port=50000
        self.serversocket.bind((host,port))