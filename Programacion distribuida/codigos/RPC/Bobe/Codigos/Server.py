from xmlrpc.server import SimpleXMLRPCServer
from xmlrpc.server import SimpleXMLRPCRequestHandler
# Funcion remota
def add(x, y):
    return x + y

# Inicializacion de servidor RPC
with SimpleXMLRPCServer(('localhost', 8000), requestHandler=SimpleXMLRPCRequestHandler) as server:
    # Habilitamos la funcion add de manera remota
    server.register_function(add, 'add')
    print("Servidor RPC en ejecución en el puerto 8000. Esperando conexiones...")
    # Mantenemos el servidor activo 
    server.serve_forever()
