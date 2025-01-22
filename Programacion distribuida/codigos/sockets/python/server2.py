import socket
port = 60000
s = socket.socket()
host = socket.gethostname()
s.bind(("0.0.0.0", port))
s.listen(15)
print('Server listening....')
while True:
    conn, addr = s.accept()
    print('Got connection from',addr)
    data = conn.recv(1024)
    print('Server received', repr(data.decode()))
    filename = 'mytext.txt'
    f = open(filename, 'rb')
    l = f.read(1024)
    while(l):
        conn.send(l)
        print('Send', repr(l.decode()))
        l = f.read(1024)
        f.close()