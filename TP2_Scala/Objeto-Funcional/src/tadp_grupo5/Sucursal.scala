package tadp_grupo5

import scala.collection.mutable.Buffer

case class Sucursal (volumenDeposito : Int, pais : String){
  var paquetesEnSalir : List[Paquete] = List()
  var paquetesEnEntrar : List[Paquete] = List()
  var transportes : List[Transporte] = List()
  
  def capacidad : Int = volumenDeposito - paquetesEnEntrar.map(_.volumen).sum - paquetesEnSalir.map(_.volumen).sum  
  
  def esCasaCentral: Boolean = false
  
  def transportesQuePuedenLLevar : Paquete => Unit = {
    paquete =>
     var lista = for{transporte <- transportes if transporte.puedeLlevarPaquete(paquete)}yield transporte
     if(!lista.isEmpty) lista.head.asignarPaquete(paquete)
  }
    
  def asignarPaquete : Paquete => Unit = paquete => transportesQuePuedenLLevar(paquete)
 
  def paquetesPendientes  = 
      for{
        paquete <- paquetesEnSalir if !transportes.exists(_.pedidos.contains(paquete))
      }yield paquete
  
  def asignarPendientes()= {
    var paquetes = paquetesPendientes
    if(paquetes.size != 0) paquetes.foreach(x => asignarPaquete(x))
  }
  
  def notificarPaqueteAEntrar(paquete : Paquete) {
    validarCapacidad(paquete)
    paquetesEnEntrar = paquetesEnEntrar :+ paquete
  }
  
  def notificarPaqueteASalir(paquete : Paquete) {
    validarCapacidad(paquete)
    paquetesEnSalir = paquetesEnSalir :+ paquete 
    asignarPaquete(paquete)
    asignarPendientes
  } 
 
   def validarCapacidad(paquete : Paquete) = 
     if (capacidad < paquete.volumen) throw new SucursalSinCapacidad()
  
  val descargarEnvios = (pedidos:List[Paquete]) => for (pedido <- pedidos) descargarEnvio(pedido)
  
  def descargarEnvio(pedido : Paquete){
    if(pedido.sucursalDestino  == this){
      paquetesEnEntrar = paquetesEnEntrar.filterNot(_== pedido) 
    } else paquetesEnSalir = paquetesEnSalir.filterNot(_== pedido)
   }
}

class CasaCentral(volumenDeposito : Int, override val pais : String) extends Sucursal(volumenDeposito, pais){
  override def esCasaCentral : Boolean = true
}

case class SucursalSinCapacidad() extends Exception

