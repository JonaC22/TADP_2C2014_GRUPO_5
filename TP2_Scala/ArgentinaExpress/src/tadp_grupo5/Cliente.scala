package tadp_grupo5

import scala.collection.mutable.Buffer

class Cliente(var sucursalOrigen: Sucursal, var sucursalDestino: Sucursal) {

  var paquetes: Buffer[Paquete] = Buffer()

  def generarPaquete(volumenPaquete: Int, caracteristicaDePaquete: Caracteristica) {
    validarSucursales
    paquetes += new Paquete(sucursalOrigen, sucursalDestino, volumenPaquete, caracteristicaDePaquete)
  }
  
  def validarSucursales = if(paquetes.nonEmpty && !paqueteConSucursalesCorrectas(paquetes.head)) throw new PaquetesConSucursalesDistintas()

  def paqueteConSucursalesCorrectas(paquete : Paquete) : Boolean = {
    paquete.sucursalOrigen == sucursalOrigen && paquete.sucursalDestino == sucursalDestino
  }
  
  def pedirEnvio(transporte : Transporte) {
    transporte.asignarPaquetes(paquetes)
	sucursalDestino.notificarPaquetesAEntrar(paquetes)
	sucursalOrigen.notificarPaquetesASalir(paquetes)
	paquetes = Buffer()
  }
}

case class PaquetesConSucursalesDistintas() extends Exception
