import xmlrpc.client  # Biblioteca para la comunicación RPC
# Creamos un proxy para localhost en el puerto 8000
proxy = xmlrpc.client.ServerProxy('http://localhost:8000')
# Enviamos por el metodo add del sever los numeros 5 y 3
result = proxy.add(5, 3)
print("Resultado de la llamada remota:", result)