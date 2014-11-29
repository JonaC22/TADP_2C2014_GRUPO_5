package tadp_grupo5


case class Sucursal (volumenDeposito : Int, pais : String){
  var paquetesPorSalir : List[Paquete] = List()
  var paquetesPorEntrar : List[Paquete] = List()
  var transportes : List[Transporte] = List()
  var enviosRealizados: List [Envio] = List()
  var pedidosPendientes: List [Paquete] = List()
  
  def capacidad : Int = volumenDeposito - paquetesPorEntrar.map(_.volumen).sum - paquetesPorSalir.map(_.volumen).sum  
  
  def actualizarTransportes(transporteAnterior: Transporte, transporteNuevo: Transporte) = { //reemplazo la referencia vieja del transporte por una nueva
    var filtrados : List[Transporte] = transportes.filterNot(_.equals(transporteAnterior))//elimino el estado anterior del transporte
    transportes = filtrados :+ transporteNuevo //agrego el nuevo estado del transporte
  }
  
  def esCasaCentral : Boolean = this match {
    case _ : CasaCentral => true
    case _ => false
  }
  
  def asignarPaquete(paquete: Paquete) = {
    var transportesValidos: List[Transporte] = filtrarTransportesValidos(paquete,transportePuedeLlevar)
    if(!transportesValidos.isEmpty){
      var trans: Transporte = transportesValidos.head.agregarPedido(paquete)
      actualizarTransportes(transportesValidos.head, trans)
      pedidosPendientes = pedidosPendientes.filter(_.equals(paquete))//elimino pedido de la lista de pendientes
    }
	else pedidosPendientes = pedidosPendientes :+ paquete
  }
  
  def asignarPendientes()= {
    if(!pedidosPendientes.isEmpty) pedidosPendientes.foreach(x => asignarPaquete(x))
  }
  
  def notificarPaqueteAEntrar(paquete : Paquete) {
    validarCapacidad(paquete)
    paquetesPorEntrar = paquetesPorEntrar :+ paquete
  }
  
  def notificarPaqueteASalir(paquete : Paquete) {
    validarCapacidad(paquete)
    paquetesPorSalir = paquetesPorSalir :+ paquete 
    asignarPaquete(paquete)
    asignarPendientes
  } 
 
  def validarCapacidad(paquete : Paquete) = if (capacidad < paquete.volumen) throw new SucursalSinCapacidad()
  
  def descargarEnvio(envio: Envio) = {
    for (pedido <- envio.paquetes) descargarPedido(pedido)
    if(envio.sucursalOrigen  == this){
    	enviosRealizados = enviosRealizados :+ envio
    	var unTransporte = envio.transporte.vaciarTransporte
    	actualizarTransportes(envio.transporte, unTransporte)
    }
  }
  
  def descargarPedido(pedido : Paquete){
    if(pedido.sucursalDestino  == this){
      paquetesPorEntrar = paquetesPorEntrar.filterNot(_== pedido) 
    } else paquetesPorSalir = paquetesPorSalir.filterNot(_== pedido)
  }

  def filtrarTransportes(f: Transporte => Boolean): List[Transporte] = {
    transportes.filter(x => f(x))
  }
  
  val transporteCargado: Transporte => Boolean = !_.pedidos.isEmpty
  
  val transportePuedeLlevar: (Transporte,Paquete) => Boolean = (transporte,paquete) => 
    try { transporte.puedeLlevar(paquete) } 
    catch { 
      case tex : TransporteException => false
      case ex : Exception => throw ex
    }
  
  def filtrarTransportesValidos : (Paquete, (Transporte,Paquete) => Boolean) => List[Transporte] = {
    (paquete,f) => for { transporte <- transportes if(f(transporte,paquete))} yield transporte
  }
  
  def despacharEnvios = {
    filtrarTransportes(transporteCargado).foreach(_.hacerEnvio)
  }
}

class CasaCentral(volumenDeposito : Int, override val pais : String) extends Sucursal(volumenDeposito, pais)

case class SucursalSinCapacidad() extends Exception

