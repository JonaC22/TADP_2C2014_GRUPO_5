package tadp_grupo5

import scala.collection.mutable.Buffer

class Cliente(var sucursalOrigen: Sucursal, var sucursalDestino: Sucursal) {

  var paquete: Paquete = null

  def generarPaquete(volumenPaquete: Int, caracteristicaDePaquete: Caracteristica) {
    paquete = new Paquete(sucursalOrigen, sucursalDestino, volumenPaquete, caracteristicaDePaquete)
  }
  
  def pedirEnvio {
	sucursalDestino.notificarPaqueteAEntrar(paquete)
	sucursalOrigen.notificarPaqueteASalir(paquete)
	paquete = null
  }
}
