import os
from Archivo import Archivo
import json

#Hilo que realice la conexion al socket
#Hilo de actualizacion de archivos cada 5 minutos
#El usuario definira a que ruta enviarse
#Menu para seleccionar los archivos

class Server:
    def __init__(self):
        self.ruta = ""
        self.archivo = []
        self.contenido = []
        self.nuevos_datos = []
    

    
    def leerRuta(self): #Debe de insertar una ruta valida, modificar por si quiere listar la misma carpeta
        print("Inserte la ruta del directorio a conocer su listado")
        self.ruta = input()
        try:
            contenido_bruto = os.listdir(self.ruta)
            self.contenido = [archivo for archivo in contenido_bruto if '.' in archivo]
            print("Archivos con extensión encontrados:")
            print(self.contenido)
        except FileNotFoundError:
            print("La ruta proporcionada no es válida. Intente nuevamente.")
        except PermissionError:
            print("No tiene permiso para acceder a esta ruta.")


    def obtenerDatosDeUsuario(self, lista_archivos):
        self.nuevos_datos = []
        for archivo in lista_archivos:
            # Preguntar por el ttl del archivo
            ttl = input(f"Ingrese el valor de ttl para el archivo '{archivo}': ")
            try:
                ttl = int(ttl)  # Convertir ttl a entero
            except ValueError:
                print("Valor inválido para ttl, se asignará el valor predeterminado de 3600.")
                ttl = 3600  # Valor predeterminado en caso de error

            # Preguntar si se debe publicar el archivo
            publish = input(f"¿Desea publicar el archivo '{archivo}'? (si/no): ").strip().lower()
            publish = True if publish == 'si' else False

            # Agregar el archivo con su ttl y publish
            self.nuevos_datos.append({
                "nombre": archivo,
                "ttl": ttl,
                "publish": publish
            })

        # Mostrar los datos recolectados
        print("Datos recolectados:")
        for item in self.nuevos_datos:
            print(item)




    def guardarEnJson(self):
        # Crear una lista de objetos con la información requerida
        # self.nuevos_datos = [
        #     {"nombre": archivo, "ttl": 3600, "publish": True}
        #     for archivo in self.contenido
        # ]
        ruta_salida = "config.json"
        # Comprobar si el archivo JSON ya existe
        if os.path.exists(ruta_salida):
            try:
                # Leer el contenido existente del archivo JSON
                with open(ruta_salida, "r", encoding="utf-8") as archivo_json:
                    datos_existentes = json.load(archivo_json)

                # Verificar si algún archivo ya está guardado
                nombres_existentes = {item["nombre"] for item in datos_existentes}
                datos_a_guardar = []

                for archivo in self.nuevos_datos:
                    if archivo["nombre"] not in nombres_existentes:
                        datos_a_guardar.append(archivo)
                    else:
                        # Espacio para añadir código si el archivo ya está en el JSON
                        print(f"El archivo '{archivo['nombre']}' ya existe en el JSON.")
                        # Aquí puedes agregar tu lógica adicional

                # Actualizar el JSON con los nuevos datos
                datos_existentes.extend(datos_a_guardar)

                with open(ruta_salida, "w", encoding="utf-8") as archivo_json:
                    json.dump(datos_existentes, archivo_json, ensure_ascii=False, indent=4)
                print(f"Datos actualizados en {ruta_salida}.")
            except Exception as e:
                print(f"Error al leer o actualizar el archivo JSON: {e}")
        else:
            # Si el archivo JSON no existe, crearlo y guardar los datos (No habia nada guardado)
            self.obtenerDatosDeUsuario(self.contenido)
            try:
                with open(ruta_salida, "w", encoding="utf-8") as archivo_json:
                    json.dump(self.nuevos_datos, archivo_json, ensure_ascii=False, indent=4)
            except Exception as e:
                print(f"Error al crear el archivo JSON: {e}")

    
    # def mostrarArchivos(self):
    #     print("Los archivos disponibles son: ")
    #     i = 1
    #     for elemento in self.contenido:
    #         print (i, ".- ", elemento)
    #         i = i+1
    
    # def seleccionarArchivos(self):
    #     res = "S"
    #     tiempo = 0
    #     print("Agregue un archivo")
    #     while(res == "S" or res == "s"):
    #         print("Inserte que archivo desea agregar")
    #         indice = int(input())
    #         if (indice > len(self.contenido)+1 or indice < 1):
    #             print("El indice de archivo no esta disponible")
    #         else:
    #             self.archSelec.append(indice)
    #         print("Desea seguir agregando archivos? S/N")
    #         res = input()
    #     print("Inserte el tiempo de actualizacion TTL")
    #     tiempo = int(input())

    #     listado = {
    #         "archivos" : self.archSelec,
    #         "TTL" : tiempo
    #     }
    #     self.config = json.dumps(listado)

def main():
    server = Server()
    server.leerRuta()
    server.guardarEnJson()
    # server.mostrarArchivos()
    # server.seleccionarArchivos()

if __name__ == "__main__":
    main()