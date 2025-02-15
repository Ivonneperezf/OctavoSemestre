from xmlrpc.server import SimpleXMLRPCServer
from xmlrpc.server import SimpleXMLRPCRequestHandler

def suma(x, y):
    return x + y

def resta(x, y):
    return x - y

def mult(x, y):
    return x * y

def div(x, y):
    if y==0:
        return "Error, no se puede dividir entre cero"
    return x / y

#Crear el servidor RPC
with SimpleXMLRPCServer(('localhost', 8000), requestHandler=SimpleXMLRPCRequestHandler) as server:
    server.register_function(suma, 'suma')
    server.register_function(resta, 'resta')
    server.register_function(mult, 'mult')
    server.register_function(div, 'div')
    print("Servidor RPC en ejecucion en el puerto 8000...")
    server.serve_forever()
    