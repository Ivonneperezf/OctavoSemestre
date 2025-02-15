import xmlrpc.client
# Establecemos la conexion
cliente_rpc = xmlrpc.client.ServerProxy('http://localhost:8000')

# Funcion de menu
def mostrar_opciones():
    print("\n--- Menu de la Calculadora Remota ---")
    print("1) Realizar Suma")
    print("2) Realizar Resta")
    print("3) Realizar Multiplicacion")
    print("4) Realizar Division")
    print("5) Salir")
    return int(input("Elija una opcion (1-5): "))

while True:
    seleccion_usuario = mostrar_opciones()
    # Si la opción es salir, cerramos la calculadora
    if seleccion_usuario == 5:
        print("Saliendo...")
        break

    # Validacion de entrada de usuario
    if seleccion_usuario < 1 or seleccion_usuario > 4:
        print("La opcion ingresada no es valida. Intente de nuevo.")
        continue 

    # Entradas
    numero1 = float(input("Por favor, ingrese el primer numero: "))
    numero2 = float(input("Por favor, ingrese el segundo numero: "))

    # Llamada a funcion remota
    if seleccion_usuario == 1:
        resultado = cliente_rpc.suma(numero1, numero2)
    elif seleccion_usuario == 2:
        resultado = cliente_rpc.resta(numero1, numero2)
    elif seleccion_usuario == 3:
        resultado = cliente_rpc.multiplicar(numero1, numero2)
    elif seleccion_usuario == 4:
        resultado = cliente_rpc.dividir(numero1, numero2)

    # Resultado
    print(f"El resultado es: {resultado}")
