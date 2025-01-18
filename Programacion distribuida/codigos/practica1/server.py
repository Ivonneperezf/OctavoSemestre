import os
import time
from Archivo import Archivo
import json

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
        except FileNotFoundError:
            print("La ruta proporcionada no es válida. Intente nuevamente.")
        except PermissionError:
            print("No tiene permiso para acceder a esta ruta.")


    def obtenerDatosDeUsuario(self, lista_archivos):
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


    def guardarEnJson(self):
        ruta_salida = "config.json"
        # Comprobar si el archivo JSON ya existe
        if os.path.exists(ruta_salida):
            try:
                # Leer el contenido existente del archivo JSON
                with open(ruta_salida, "r", encoding="utf-8") as archivo_json:
                    datos_existentes = json.load(archivo_json)
                    # Crear un conjunto de los nombres de los archivos en el JSON
                    nombres_existentes = {item["nombre"] for item in datos_existentes}
                    datos_a_guardar = []

                    for archivo in self.contenido:
                        if archivo not in nombres_existentes:
                            # Si el archivo está en la lista pero no en el JSON, lo agregamos a la lista de nuevos archivos
                            datos_a_guardar.append(archivo)
                    # Eliminar archivos que están en el JSON pero no en la lista `contenido`
                    datos_existentes = [
                        item for item in datos_existentes if item["nombre"] in self.contenido
                    ]
                    # Si hay archivos nuevos que agregar, llamar al método para obtener los datos del usuario
                    if datos_a_guardar:
                        print(datos_a_guardar)
                        self.obtenerDatosDeUsuario(datos_a_guardar)
                        # Agregar los nuevos datos con los valores obtenidos del usuario
                        datos_existentes.extend(self.nuevos_datos)


                with open(ruta_salida, "w", encoding="utf-8") as archivo_json:
                    json.dump(datos_existentes, archivo_json, ensure_ascii=False, indent=4)
                print(f"Datos actualizados en {ruta_salida}.")
            except Exception as e:
                print(f"Error al leer o actualizar el archivo JSON: {e}")
        else:
            self.obtenerDatosDeUsuario(self.contenido)
            try:
                with open(ruta_salida, "w", encoding="utf-8") as archivo_json:
                    json.dump(self.nuevos_datos, archivo_json, ensure_ascii=False, indent=4)
            except Exception as e:
                print(f"Error al crear el archivo JSON: {e}")


def main():
    server = Server()
    server.leerRuta()
    server.guardarEnJson()

if __name__ == "__main__":
    main()