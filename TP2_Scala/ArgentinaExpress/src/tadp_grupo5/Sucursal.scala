package tadp_grupo5

import scala.collection.mutable.Buffer

class Sucursal (volumenDeposito : Int, val pais : String) {
  var paquetesEnSalir : Buffer[Paquete] = Buffer()
  var paquetesEnEntrar : Buffer[Paquete] = Buffer()
  
  def capacidad : Int = volumenDeposito - paquetesEnEntrar.map(_.volumen).sum - paquetesEnSalir.map(_.volumen).sum  
  
  def esCasaCentral(transporte : Transporte) : Double = 0.0
  
  def notificarPaquetesAEntrar(paquetes : Buffer[Paquete]) {
    validarCapacidad(paquetes)
    paquetesEnEntrar ++= paquetes
  }
  
  def notificarPaquetesASalir(paquetes : Buffer[Paquete]) {
    validarCapacidad(paquetes)
    paquetesEnSalir ++= paquetes
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
  
  def validarCapacidad(paquetes : Buffer[Paquete]) =  if (capacidad < paquetes.map(_.volumen).sum) throw new SucursalSinCapacidad()
  def validarCapacidad(paquete : Paquete) = if (capacidad == 0) throw new SucursalSinCapacidad()
}

case class CasaCentral(volumenDeposito : Int, override val pais : String) extends Sucursal(volumenDeposito, pais){
  override def esCasaCentral(transporte : Transporte) : Double = transporte.costoAdicionalCamionCasaCentral
}

case class SucursalSinCapacidad() extends Exception