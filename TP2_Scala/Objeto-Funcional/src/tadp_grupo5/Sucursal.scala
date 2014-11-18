package tadp_grupo5

import scala.collection.mutable.Buffer

case class Sucursal (
    volumenDeposito : Int,
    pais : String)
{
  var paquetesEnSalir : List[Paquete] 
  var paquetesEnEntrar : List[Paquete]
  var transportes : List[Transporte]
  
  def capacidad : Int = volumenDeposito - paquetesEnEntrar.map(_.volumen).sum - paquetesEnSalir.map(_.volumen).sum  
  
  def esCasaCentral: Boolean = false
  
  val transportesQuePuedenLLevar :Paquete => List[Transporte] = {
    paquete =>
      for{
       transporte <- transportes if transporte.puedeLlevarPaquete(paquete)
      }yield transporte
  }
    
  val asignarPaquete =  (paquete:Paquete) =>  transportesQuePuedenLLevar(paquete).head 
 
  val paquetesPendientes  = 
      for{
        paquete <- paquetesEnSalir if transportes.exists(_.pedidos.contains(paquete))
      }yield paquete
  
  def asignarPendientes(){
    var paquetes = paquetesPendientes
    if(paquetes != 0) paquetes.foreach(x => asignarPaquete(x))
  }
  
  def notificarPaqueteAEntrar(paquete : Paquete) {
    validarCapacidad(paquete)
    agregarPaquete(paquete,paquetesEnEntrar)  
  }
  
  val agregarPaquete = (_:Paquete)::(_:List[Paquete]) //Esto por ahora no sirve para nada...
  
  
  def notificarPaqueteASalir(paquete : Paquete) {
    validarCapacidad(paquete)
    agregarPaquete(paquete,paquetesEnSalir)
    asignarPaquete(paquete)
    asignarPendientes
  } 
 
   def validarCapacidad(paquete : Paquete) = 
     if (capacidad < paquete.volumen) throw new SucursalSinCapacidad()
  
  val descargarEnvios = (pedidos:List[Paquete]) => for (pedido <- pedidos) descargarEnvio(pedido)
  
  def descargarEnvio(pedido : Paquete){
    if(pedido.sucursalDestino  == this){
      paquetesEnEntrar = paquetesEnEntrar.filterNot(_== pedido) 
    }
    if (capacidad < pedido.volumen) throw new SucursalSinCapacidad()
   }

}

class CasaCentral(volumenDeposito : Int, override val pais : String) extends Sucursal(volumenDeposito, pais){
  override def esCasaCentral : Boolean = true
}

case class SucursalSinCapacidad() extends Exception

