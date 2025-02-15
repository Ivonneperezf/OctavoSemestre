from xmlrpc.server import SimpleXMLRPCServer
from xmlrpc.server import SimpleXMLRPCRequestHandler

# Funciones remotas de operaciones
def suma(x, y):
    return x + y

def resta(x, y):
    return x - y

def mult(x, y):
    return x * y

def div(x, y):
    return x / y if y != 0 else "Error: Division entre cero no permitida"

# Configuramos y lanzamos el servidor RPC
with SimpleXMLRPCServer(('localhost', 8000), requestHandler=SimpleXMLRPCRequestHandler) as servidor:
    
    # Registramos las funciones para que puedan ser llamadas de manera remota
    servidor.register_function(suma, 'suma')
    servidor.register_function(resta, 'resta')
    servidor.register_function(mult, 'mult')
    servidor.register_function(div, 'div')

    print("Servidor RPC en ejecución en el puerto 8000. Esperando solicitudes...")

    # Mantenemos el servidor en ejecucion
    servidor.serve_forever()