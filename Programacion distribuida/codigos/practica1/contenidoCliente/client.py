import socket
import json
import time

class Client:
    def __init__(self):
        self.s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        self.host=socket.gethostname()
        self.port=50000

    def establecerConexion(self):
        try:
            self.s.connect((self.host, self.port))  # Establecer conexión con el servidor
            print("Conexión establecida con el servidor.")
            print("Inserte la ruta del directorio a conocer su listado:")
            ruta = input()
            while True:
                mensaje = {"ruta": ruta}
                self.s.sendto(json.dumps(mensaje).encode("utf-8"), (self.host, self.port))
                try:
                    data, _ = self.s.recvfrom(2048)
                    respuesta = json.loads(data.decode("utf-8"))
                    
                    if (respuesta["valida"]):
                        if(not respuesta["leer"]):
                            print("No hay archivos nuevos")
                        else:
                            archivos = respuesta["archivos"]
                            if(archivos):
                                datos_archivos = []
                                for archivo in archivos:
                                    ttl = input(f"Ingrese el TTL para '{archivo}': ")
                                    try:
                                        ttl = int(ttl)
                                    except ValueError:
                                        print("Valor inválido. Se asignará 3600 por defecto.")
                                        ttl = 3600
                                    
                                    publish = input(f"¿Publicar '{archivo}'? (S/N): ").strip().lower() == 's'
                                    datos_archivos.append({
                                        "nombre": archivo,
                                        "ttl": ttl,
                                        "publish": publish
                                    })

                                mensaje_completo = {
                                    "addr": self.s.getpeername()[0],  # Dirección del cliente
                                    "ruta": ruta,  # Ruta proporcionada
                                    "archivos": datos_archivos  # Datos de los archivos
                                }

                                self.s.sendto(json.dumps(mensaje_completo).encode("utf-8"), (self.host, self.port))
                            else:
                                print("No hay archivos almacenado en esa ruta")
                            #print("Ruta validada correctamente")
                    else:
                        print("Ruta invalida, inserte nuevamente una ruta valida")
                        ruta = input()
                    #print(f"Respuesta del servidor: {data.decode('utf-8')}")
                except socket.timeout:
                    print("El servidor no respondió. Intente nuevamente.")
                #time.sleep(10)
        except ConnectionRefusedError:
            print("No se pudo establecer conexión con el servidor. Verifique si está en ejecución.")
        except Exception as e:
            print(f"Ocurrió un error: {e}")
    

def main():
    client = Client()
    client.establecerConexion()

if __name__ == "__main__":
    main()