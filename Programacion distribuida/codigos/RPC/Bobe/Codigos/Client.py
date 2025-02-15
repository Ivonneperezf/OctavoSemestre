import xmlrpc.client 
# Inicializacion de cliente RPC
proxy = xmlrpc.client.ServerProxy('http://localhost:8000')
# Llamada a la funcion remota add
result = proxy.add(5, 3)
print("Resultado de la operación remota:", result)