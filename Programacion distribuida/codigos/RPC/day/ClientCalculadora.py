import xmlrpc.client
proxy = xmlrpc.client.ServerProxy('http://localhost:8000')

# Funcion para mostrar el menu
def mostrar_menu():
    print("\n=== Calculadora Remota ===")
    print("1) Sumar")
    print("2) Restar")
    print("3) Multiplicar")
    print("4) Dividir")
    print("5) Salir")
    return int(input("Seleccione una opción (1-5): "))


while True:
    opcion = mostrar_menu()
    # Opcion para salir del programa
    if opcion == 5:
        print("Cerrando la calculadora remota...")
        break

    # Validacion de entrada
    if opcion not in [1, 2, 3, 4]:
        print("Opción no válida. Intente de nuevo.")
        continue 
    
    # Entradas de datos
    n1 = float(input("Ingrese el primer número: "))
    n2 = float(input("Ingrese el segundo número: "))

    # Opciones del server remoto
    if opcion == 1:
        resultado = proxy.suma(n1, n2)
    elif opcion == 2:
        resultado = proxy.resta(n1, n2)
    elif opcion == 3:
        resultado = proxy.mult(n1, n2)
    elif opcion == 4:
        resultado = proxy.div(n1, n2)

    # Mostramos el resultado
    print(f"Resultado: {resultado}")
