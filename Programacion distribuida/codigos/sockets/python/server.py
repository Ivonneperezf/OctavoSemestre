#server.py
import socket
import time

#create a socket object
serversocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
# get local machine name
host=socket.gethostname() #Insertar direccion IP para server que esten en otra maquina
port=9999
#bind to teh port
serversocket.bind((host,port))
#queue up to 5 requests
serversocket.listen(5)
#establish a connection
while(True):
    cliensocket, addr=serversocket.accept()
    print("Connected with[addr], [port]%s"%str(addr))
    currentTime=time.ctime(time.time())+"\r\n"
    cliensocket.send(currentTime.encode('ascii'))
    cliensocket.close