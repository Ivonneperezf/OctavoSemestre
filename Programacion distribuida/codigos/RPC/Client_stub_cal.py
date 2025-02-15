import xmlrpc.client

#Conectar al servidor RPC
proxy = xmlrpc.client.ServerProxy('http://localhost:8000')

#Llamar a la funcion remota add en el servidor
#Menú de la calculadora
def menu():
    print("*** Menú ***")
    print("1-Sumar")
    print("2-Restar")
    print("3-Multiplciar")
    print("4-Dividir")
    print("5-Salir")
    resp = input("Por favor ingrese un número del 1 al 5: ")
    return int(resp)

while (True):
    resp=menu()
    if resp==5:
        print("Saliendo...")
        break
    print("Ingrese el primer número: ")
    n1 = float(input())
    print("Ingrese el segundo número: ")
    n2 = float(input())
    
    if resp == 1:
        result = proxy.suma(n1,n2)
        print("Resultado de la llamada remota: ", result)
    elif resp == 2:
        result = proxy.resta(n1,n2)
        print("Resultado de la llamada remota: ", result)
    elif resp == 3:
        result = proxy.mult(n1,n2)
        print("Resultado de la llamada remota: ", result)
    elif resp == 4:
        result = proxy.div(n1,n2)
        print("Resultado de la llamada remota: ", result)
    else:
        print("Opción no válida.")
        continue
        