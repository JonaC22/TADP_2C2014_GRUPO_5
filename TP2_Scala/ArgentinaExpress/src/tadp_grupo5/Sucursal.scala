package tadp_grupo5

import scala.collection.mutable.Buffer

class Sucursal (volumenDeposito : Int, val pais : String) {
  var paquetesEnSalir : Buffer[Paquete] = Buffer()
  var paquetesEnEntrar : Buffer[Paquete] = Buffer()
  var transportes : Buffer[Transporte] = Buffer()
  
  def capacidad : Int = volumenDeposito - paquetesEnEntrar.map(_.volumen).sum - paquetesEnSalir.map(_.volumen).sum  
  
  def esCasaCentral: Boolean = false
  
  def asignarPaquete(paquete : Paquete) {
    if(transportes.size != 0){
      var transporte : Option[Transporte] = transportes.find( x => x.puedeLlevarPaquete(paquete))
      transporte.get.asignarPaquete(paquete)
    }
  }
  
  def paquetesPendientes : Buffer[Paquete] = {
	  paquetesEnSalir.filterNot(x => transportes.exists(_.pedidos.contains(x)))
  }
  
  def asignarPendientes(){
    var paquetes = paquetesPendientes
    if(paquetes != 0) paquetes.foreach(x => asignarPaquete(x))
  }
  
  def notificarPaqueteAEntrar(paquete : Paquete) {
    validarCapacidad(paquete)
    paquetesEnEntrar += paquete
  }
  
  def notificarPaqueteASalir(paquete : Paquete) {
    validarCapacidad(paquete)
    paquetesEnSalir += paquete
    asignarPaquete(paquete)
    asignarPendientes
  } 
  
  def descargarEnvios(pedidos : Buffer[Paquete]){
    pedidos.foreach(x => descargarEnvio(x))
  }
  
  def descargarEnvio(pedido : Paquete){
    if(pedido.sucursalDestino  == this){
      paquetesEnEntrar = paquetesEnEntrar.filterNot(_== pedido)
    }
    else paquetesEnSalir = paquetesEnSalir.filterNot(_== pedido)
  }

  def validarCapacidad(paquete : Paquete) = if (capacidad < paquete.volumen) throw new SucursalSinCapacidad()
}

case class CasaCentral(volumenDeposito : Int, override val pais : String) extends Sucursal(volumenDeposito, pais){
  override def esCasaCentral : Boolean = true
}

case class SucursalSinCapacidad() extends Exception