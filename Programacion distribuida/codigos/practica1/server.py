import threading
import time
import os
import json
import socket
import logging

class Server:
    def __init__(self):
        self.ruta_salida = "config.json"
        # Configuración del logger
        logging.basicConfig(
            filename="historial.log",
            level=logging.INFO,
            format="%(asctime)s - %(levelname)s - %(message)s",
            datefmt="%Y-%m-%d %H:%M:%S"
        )
        self.lock = threading.Lock()

    def validarRuta(self, ruta, direccion):
        respuesta = {"valida": False, "archivos": [], "leer": True}  # Estructura de respuesta inicial

        if not os.path.isdir(ruta):  # Verifica si la ruta es un directorio válido
            logging.error(f"Ruta no válida proporcionada: {ruta} por {direccion}")
            print("La ruta proporcionada no es válida. Intente nuevamente.\n")
            return respuesta
        else:
            try:
                # Obtener contenido del directorio y filtrar archivos
                contenido_bruto = os.listdir(ruta)
                archivos = [archivo for archivo in contenido_bruto if '.' in archivo]

                # Verifica si la ruta ya está en el archivo JSON
                if archivos:
                    with self.lock:
                        # Comprobar si la ruta ya está en el JSON
                        with open(self.ruta_salida, "r", encoding="utf-8") as archivo_json:
                            datos_existentes = json.load(archivo_json)

                    ruta_existente = next((item for item in datos_existentes if item["ruta"] == ruta), None)

                    if ruta_existente:
                        archivos_existentes = {archivo["nombre"] for archivo in ruta_existente["archivos"]}
                        archivos_nuevos = [archivo for archivo in archivos if archivo not in archivos_existentes]

                        if not archivos_nuevos:
                            # Si no hay archivos nuevos, añadir "leer": False para indicar que el cliente no debe leer la ruta
                            respuesta["leer"] = False
                        else:
                            # Si hay archivos nuevos, agregar solo esos archivos a la respuesta
                            respuesta["archivos"] = archivos_nuevos
                            logging.info(f"Archivos obtenidos desde la ruta: {ruta} por {direccion}")
                    else:
                        # Si no existe en el archivo JSON, agregar todos los archivos encontrados
                        respuesta["archivos"] = archivos
                        logging.info(f"Archivos obtenidos desde la ruta: {ruta} por {direccion}")

                    respuesta["valida"] = True  # Ruta válida
                else:
                    logging.info(f"No se encontraron archivos en la ruta: {ruta} por {direccion}")
                
                return respuesta
            except PermissionError:
                print("No tiene permiso para acceder a esta ruta.\n")
                logging.error(f"Permiso denegado para acceder a la ruta: {ruta} por {direccion}")
                return respuesta
            except Exception as e:
                print(f"Error inesperado al intentar acceder a la ruta: {e}\n")
                logging.error(f"Error inesperado al intentar acceder a la ruta: {e} por {direccion}")
                return respuesta

    def leerRuta(self, ruta):
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


    def getContenido(self, ruta):
        contenido_bruto = os.listdir(ruta)
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
                            for nuevo_dato in self.nuevos_datos:
                                if not any(dato["nombre"] == nuevo_dato["nombre"] for dato in datos_existentes):
                                    datos_existentes.append(nuevo_dato)
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
            time.sleep(300)  # Esperar 5 minutos (300 segundos)

    def guardarEnJsonCliente(self, mensaje_completo):
        with self.lock:
            self.getContenido(mensaje_completo["ruta"])
            if os.path.exists(self.ruta_salida):
                    try:
                        # Abrir el archivo JSON para leer los datos existentes
                        with open(self.ruta_salida, "r", encoding="utf-8") as archivo_json:
                            datos_existentes = json.load(archivo_json)

                        # Verificar si la ruta ya existe en el archivo JSON
                        ruta_existente = next((item for item in datos_existentes if item["ruta"] == mensaje_completo["ruta"]), None)
                        print(ruta_existente)

                        if ruta_existente:
                            # Si la ruta existe, verificar si hay archivos en el mensaje
                            if mensaje_completo["archivos"]:  # Solo continuar si hay archivos en el mensaje
                                archivos_existentes = {archivo["nombre"] for archivo in ruta_existente["archivos"]}

                                datos_a_guardar = []

                                # Identificar archivos nuevos o modificados
                                for archivo in mensaje_completo["archivos"]:
                                    if archivo["nombre"] not in archivos_existentes:
                                        datos_a_guardar.append(archivo)

                                # Agregar los archivos nuevos sin eliminar los existentes
                                if datos_a_guardar:
                                    for nuevo_dato in datos_a_guardar:
                                        if not any(dato["nombre"] == nuevo_dato["nombre"] for dato in ruta_existente["archivos"]):
                                            ruta_existente["archivos"].append(nuevo_dato)
                                    logging.info(f"Archivos agregados: {datos_a_guardar}")
                                    print("Se agregaron nuevos archivos en la ruta especificada")

                            else:
                                logging.info(f"No se encontraron archivos nuevos para la ruta: {mensaje_completo['ruta']}")
                                print(f"No se encontraron archivos nuevos para la ruta: {mensaje_completo['ruta']}")

                        else:
                            # Si la ruta no existe, agregarla como nueva
                            nuevo_dato = {
                                "ruta": mensaje_completo["ruta"],
                                "archivos": mensaje_completo["archivos"]
                            }
                            datos_existentes.append(nuevo_dato)
                            logging.info(f"Nueva ruta agregada: {mensaje_completo['ruta']}")
                            print(f"Se agregó una nueva ruta: {mensaje_completo['ruta']}")

                        # Escribir los cambios en el archivo JSON
                        with open(self.ruta_salida, "w", encoding="utf-8") as archivo_json:
                            json.dump(datos_existentes, archivo_json, ensure_ascii=False, indent=4)

                    except Exception as e:
                        print(f"Error al leer o actualizar el archivo JSON: {e}")
                        logging.error(f"Error al actualizar el archivo JSON: {e}")
            else:
                # Si el archivo JSON no existe, crear uno nuevo con los datos recibidos
                try:
                    with open(self.ruta_salida, "w", encoding="utf-8") as archivo_json:
                        json.dump([mensaje_completo], archivo_json, ensure_ascii=False, indent=4)
                    logging.info(f"Archivo JSON creado con los datos iniciales: {mensaje_completo}")
                    print("Se guardaron los datos en el archivo JSON")

                except Exception as e:
                    print(f"Error al crear el archivo JSON: {e}")
                    logging.error(f"Error al crear el archivo JSON: {e}")

            print("Esperando 5 minutos para la próxima actualización...")

    def iniciar_socket_udp(self):
        #Inicializacion de socket
        udp_socket = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        host=socket.gethostname()
        port=50000
        print(f"Iniciando socket UDP en el puerto {port}...")
        udp_socket.bind(("0.0.0.0", port))
        clientes_conectados = set()
        while True:
            data, addr = udp_socket.recvfrom(2048)  # Incrementar tamaño si es necesario
            mensaje = json.loads(data.decode("utf-8"))
            print(mensaje)
            if addr not in clientes_conectados:
                clientes_conectados.add(addr)
            print(f"Nuevo cliente conectado desde {addr[0]}:{addr[1]}")
            
            if "addr" in mensaje:  # Guardar datos de archivos
                self.guardarEnJsonCliente(mensaje)
                threading.Thread(target=self.guardarEnJsonCliente, args=(mensaje,)).start()
            elif "ruta" in mensaje:  # Validar ruta
                ruta = mensaje["ruta"]
                validacion = self.validarRuta(ruta, addr)
                udp_socket.sendto(json.dumps(validacion).encode("utf-8"), addr)
            time.sleep(300)

def main():
    server = Server()

    # Crear el hilo para iniciar el socket UDP
    hilo_socket_udp = threading.Thread(target=server.iniciar_socket_udp)
    #hilo_socket_udp.daemon = True  # Permitir que el programa termine incluso si el hilo sigue activo
    hilo_socket_udp.start()
    # Crear el hilo para leer la ruta
    # hilo_leer_ruta = threading.Thread(target=server.leerRuta)
    # hilo_leer_ruta.start()
    #hilo_leer_ruta.join()  # Asegurarse de que termine antes de iniciar el hilo de guardar JSON

    # Crear el hilo para guardar en JSON
    # hilo_guardar_json = threading.Thread(target=server.guardarEnJson)
    # hilo_guardar_json.start()

if __name__ == "__main__":
    main()