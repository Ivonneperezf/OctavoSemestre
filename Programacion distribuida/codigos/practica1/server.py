import threading
import time
import os
import json
import socket
import logging

class Server:
    def __init__(self):
        self.ruta = ""
        self.contenido = []
        self.nuevos_datos = []
        self.ruta_salida = "config.json"
        #Inicializacion de socket
        self.udp_socket = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        self.host=socket.gethostname()
        self.port=50000
        # Configuración del logger
        logging.basicConfig(
            filename="historial.log",
            level=logging.INFO,
            format="%(asctime)s - %(levelname)s - %(message)s",
            datefmt="%Y-%m-%d %H:%M:%S"
        )
        self.evento_ruta_completa = threading.Event()
    
    def leerRuta(self):
        while True:  # Bucle para permitir reintentos en caso de ruta incorrecta
            print("Inserte la ruta del directorio a conocer su listado:")
            self.ruta = input()
            if not os.path.isdir(self.ruta):  # Verifica si la ruta es un directorio válido
                print("La ruta proporcionada no es válida. Intente nuevamente.\n")
                logging.error(f"Ruta no valida proporcionada: {self.ruta}")
            else:
                try:
                    contenido_bruto = os.listdir(self.ruta)
                    # Filtrar solo los archivos con extensión
                    self.contenido = [archivo for archivo in contenido_bruto if '.' in archivo]
                    logging.info(f"Archivos obtenidos desde la ruta: {self.ruta}")
                    break 
                except PermissionError:
                    print("No tiene permiso para acceder a esta ruta.\n")
                    logging.error(f"Permiso denegado para acceder a la ruta: {self.ruta}")
                    break 
                except Exception as e:
                    print(f"Error inesperado al intentar acceder a la ruta: {e}\n")
                    logging.error(f"Error inesperado al intentar acceder a la ruta: {e}")
                    break  
        self.evento_ruta_completa.set()


    def getContenido(self):
        contenido_bruto = os.listdir(self.ruta)
        self.contenido = [archivo for archivo in contenido_bruto if '.' in archivo]

    def obtenerDatosDeUsuario(self, lista_archivos):
        for archivo in lista_archivos:
            ttl = input(f"Ingrese el valor de ttl para el archivo '{archivo}': ")
            try:
                ttl = int(ttl)
            except ValueError:
                print("Valor inválido para ttl, se asignará el valor predeterminado de 3600.")
                ttl = 3600
            publish = input(f"¿Desea publicar el archivo '{archivo}'? (S/N): ").strip().lower()
            publish = True if publish == 's' or publish == 'S' else False
            self.nuevos_datos.append({
                "nombre": archivo,
                "ttl": ttl,
                "publish": publish
            })

    def guardarEnJson(self):
        self.evento_ruta_completa.wait()
        while True:
            self.getContenido()
            if os.path.exists(self.ruta_salida):
                try:
                    with open(self.ruta_salida, "r", encoding="utf-8") as archivo_json:
                        datos_existentes = json.load(archivo_json)
                        nombres_existentes = {item["nombre"] for item in datos_existentes}
                        datos_a_guardar = []

                        for archivo in self.contenido:
                            if archivo not in nombres_existentes:
                                datos_a_guardar.append(archivo)

                        # Filtrar archivos que ya no están en el sistema
                        datos_eliminados = [
                            item["nombre"] for item in datos_existentes if item["nombre"] not in self.contenido
                        ]
                        datos_existentes = [
                            item for item in datos_existentes if item["nombre"] in self.contenido
                        ]

                        if datos_a_guardar:
                            self.obtenerDatosDeUsuario(datos_a_guardar)
                            datos_existentes.extend(self.nuevos_datos)
                            logging.info(f"Archivos agregados: {datos_a_guardar}")
                            print("Se agregaron nuevos archivos al archivo de configuracion")
                        
                        if datos_eliminados:
                            logging.info(f"Archivos eliminados: {datos_eliminados}")
                            print("Se eliminaron archivos en el archivo de configuracion")

                    with open(self.ruta_salida, "w", encoding="utf-8") as archivo_json:
                        json.dump(datos_existentes, archivo_json, ensure_ascii=False, indent=4)
                except Exception as e:
                    print(f"Error al leer o actualizar el archivo JSON: {e}")
                    logging.error(f"Error al actualizar el archivo JSON: {e}")
            else:
                self.obtenerDatosDeUsuario(self.contenido)
                try:
                    with open(self.ruta_salida, "w", encoding="utf-8") as archivo_json:
                        json.dump(self.nuevos_datos, archivo_json, ensure_ascii=False, indent=4)
                    logging.info(f"Archivo JSON creado con los datos iniciales: {self.nuevos_datos}")
                    print("Se guardaron nuevos archivos en la ruta especificada")
                except Exception as e:
                    print(f"Error al crear el archivo JSON: {e}")

            print("Esperando 5 minutos para la próxima actualización...")
            time.sleep(60)  # Esperar 5 minutos (300 segundos)

    def iniciar_socket_udp(self):
        print(f"Iniciando socket UDP en el puerto {self.port}...")
        self.udp_socket.bind(("0.0.0.0", self.port))
        while True:
            print("El socket servidor esta activo")
            time.sleep(30)
        #     data, addr = self.udp_socket.recvfrom(1024)
        #     mensaje = data.decode("utf-8")
        #     logging.info(f"Mensaje recibido desde {addr}: {mensaje}")
        #     print(f"Mensaje recibido desde {addr}: {mensaje}")

def main():
    server = Server()

    # Crear el hilo para leer la ruta
    hilo_leer_ruta = threading.Thread(target=server.leerRuta)
    hilo_leer_ruta.start()
    hilo_leer_ruta.join()  # Asegurarse de que termine antes de iniciar el hilo de guardar JSON

    # Crear el hilo para guardar en JSON
    hilo_guardar_json = threading.Thread(target=server.guardarEnJson)
    hilo_guardar_json.start()

    # Crear el hilo para iniciar el socket UDP
    hilo_socket_udp = threading.Thread(target=server.iniciar_socket_udp)
    hilo_socket_udp.daemon = True  # Permitir que el programa termine incluso si el hilo sigue activo
    hilo_socket_udp.start()

if __name__ == "__main__":
    main()