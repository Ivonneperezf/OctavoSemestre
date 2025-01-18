#server.py
import os
import socket
import json

#Hilo que realice la conexion al socket
#Hilo de actualizacion de archivos cada 5 minutos
#El usuario definira a que ruta enviarse
#Menu para seleccionar los archivos

class Server:
    def __init__(self):
        self.ruta = ""
        self.contenido = []
        self.archSelec = []
        self.archComp = []
        self.config = None
    
    def listarArchivos(self): #Debe de insertar una ruta valida, modificar por si quiere listar la misma carpeta
        print("Inserte la ruta del directorio a conocer su listado")
        self.ruta = input()
        self.contenido = os.listdir(self.ruta)
    
    def mostrarArchivos(self):
        print("Los archivos disponibles son: ")
        i = 1
        for elemento in self.contenido:
            print (i, ".- ", elemento)
            i = i+1
    
    def seleccionarArchivos(self):
        res = "S"
        tiempo = 0
        print("Agregue un archivo")
        while(res == "S" or res == "s"):
            print("Inserte que archivo desea agregar")
            indice = int(input())
            if (indice > len(self.contenido)+1 or indice < 1):
                print("El indice de archivo no esta disponible")
            else:
                self.archSelec.append(indice)
            print("Desea seguir agregando archivos? S/N")
            res = input()
        print("Inserte el tiempo de actualizacion TTL")
        tiempo = int(input())

        listado = {
            "archivos" : self.archSelec,
            "TTL" : tiempo
        }
        self.config = json.dumps(listado)

def main():
    server = Server()
    server.listarArchivos()
    server.mostrarArchivos()
    server.seleccionarArchivos()

if __name__ == "__main__":
    main()