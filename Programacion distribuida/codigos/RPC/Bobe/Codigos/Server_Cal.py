from xmlrpc.server import SimpleXMLRPCServer
from xmlrpc.server import SimpleXMLRPCRequestHandler
# Funciones de operaciones
def agregar(x, y):
    return x + y
def restar(x, y):
    return x - y
def multiplicar(x, y):
    return x * y
def dividir(x, y):
    return x / y if y != 0 else "No se puede dividir entre cero"
# Configuramos el servidor RPC
with SimpleXMLRPCServer(('localhost', 8000), requestHandler=SimpleXMLRPCRequestHandler) as servidor_rpc:
    # Registramos las funciones 
    servidor_rpc.register_function(agregar, 'suma')
    servidor_rpc.register_function(restar, 'resta')
    servidor_rpc.register_function(multiplicar, 'multiplicar')
    servidor_rpc.register_function(dividir, 'dividir')
    print("El servidor RPC está activo en el puerto 8000, esperando solicitudes...")
    servidor_rpc.serve_forever()
