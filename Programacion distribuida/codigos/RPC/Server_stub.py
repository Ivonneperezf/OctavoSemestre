from xmlrpc.server import SimpleXMLRPCServer
from xmlrpc.server import SimpleXMLRPCRequestHandler
# Funcion remota para sumar dos numeros
def add(x, y):
    return x + y
# Configuracion del servidor
with SimpleXMLRPCServer(('localhost', 8000), requestHandler=SimpleXMLRPCRequestHandler) as server:
    # Registro de funcion para invocar remotamente
    server.register_function(add, 'add')
    print("Servidor RPC en ejecución en el puerto 8000...")
    server.serve_forever()
