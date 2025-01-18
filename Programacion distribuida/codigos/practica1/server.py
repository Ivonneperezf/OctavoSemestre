import threading
import time
import os
import json

class Server:
    def __init__(self):
        self.ruta = ""
        self.archivo = []
        self.contenido = []
        self.nuevos_datos = []
    
    def leerRuta(self):
        print("Inserte la ruta del directorio a conocer su listado")
        self.ruta = input()

    def obtenerContenido(self):
        try:
            contenido_bruto = os.listdir(self.ruta)
            self.contenido = [archivo for archivo in contenido_bruto if '.' in archivo]
        except FileNotFoundError:
            print("La ruta proporcionada no es válida. Intente nuevamente.")
        except PermissionError:
            print("No tiene permiso para acceder a esta ruta.")

    def obtenerDatosDeUsuario(self, lista_archivos):
        for archivo in lista_archivos:
            ttl = input(f"Ingrese el valor de ttl para el archivo '{archivo}': ")
            try:
                ttl = int(ttl)
            except ValueError:
                print("Valor inválido para ttl, se asignará el valor predeterminado de 3600.")
                ttl = 3600
            publish = input(f"¿Desea publicar el archivo '{archivo}'? (si/no): ").strip().lower()
            publish = True if publish == 'si' else False

            self.nuevos_datos.append({
                "nombre": archivo,
                "ttl": ttl,
                "publish": publish
            })

    def guardarEnJson(self):
        while True:
            self.obtenerContenido()
            ruta_salida = "config.json"
            if os.path.exists(ruta_salida):
                try:
                    with open(ruta_salida, "r", encoding="utf-8") as archivo_json:
                        datos_existentes = json.load(archivo_json)
                        nombres_existentes = {item["nombre"] for item in datos_existentes}
                        datos_a_guardar = []

                        for archivo in self.contenido:
                            if archivo not in nombres_existentes:
                                datos_a_guardar.append(archivo)

                        datos_existentes = [
                            item for item in datos_existentes if item["nombre"] in self.contenido
                        ]

                        if datos_a_guardar:
                            print(datos_a_guardar)
                            self.obtenerDatosDeUsuario(datos_a_guardar)
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

            print("Esperando 5 minutos para la próxima actualización...")
            time.sleep(60)  # Esperar 5 minutos (300 segundos)

def main():
    server = Server()

    # Crear el hilo para leer la ruta
    hilo_leer_ruta = threading.Thread(target=server.leerRuta)
    hilo_leer_ruta.start()
    hilo_leer_ruta.join()  # Asegurarse de que termine antes de iniciar el hilo de guardar JSON

    # Crear el hilo para guardar en JSON
    hilo_guardar_json = threading.Thread(target=server.guardarEnJson)
    hilo_guardar_json.start()

if __name__ == "__main__":
    main()
