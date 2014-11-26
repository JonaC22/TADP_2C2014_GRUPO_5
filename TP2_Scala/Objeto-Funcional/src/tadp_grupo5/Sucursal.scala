package tadp_grupo5


case class Sucursal (volumenDeposito : Int, pais : String){
  var paquetesPorSalir : List[Paquete] = List()
  var paquetesPorEntrar : List[Paquete] = List()
  var transportes : List[Transporte] = List()
  
  var enviosRealizados: List [Envio] = List()
  var pedidosPendientes: List [Paquete] = List()

  
  val despachante: Despachante = Despachante()
  
  def capacidad : Int = volumenDeposito - paquetesPorEntrar.map(_.volumen).sum - paquetesPorSalir.map(_.volumen).sum  
  
  def esCasaCentral: Boolean = false
  
  def asignarPaquete(paquete: Paquete) = {//faltaria buscar el transporte que se modifica y reemplazarlo por el nuevo
    var cargadosValidos: List[Transporte] = filtrarValidos(paquete,filtrarTransportes(filtroCargados),filtroValidos)
    var vaciosValidos: List[Transporte] = filtrarValidos(paquete,filtrarTransportes(filtroVacios),filtroValidos)
    if(!cargadosValidos.isEmpty)  despachante.agregarPedido(cargadosValidos.head, paquete)
	else if(!vaciosValidos.isEmpty) despachante.agregarPedido(vaciosValidos.head, paquete)
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
  
  def descargarEnvios(envio: Envio) = {
    for (pedido <- envio.paquetes) descargarEnvio(pedido)
    if(envio.sucursalOrigen  == this)
    	enviosRealizados = enviosRealizados :+ envio
    	var unTransporte = despachante.vaciarTransporte(envio.transporte)
    	//faltaria buscar el transporte cargado y reemplazarlo por el vacio en la lista de transportes
  }
  
  def descargarEnvio(pedido : Paquete){
    if(pedido.sucursalDestino  == this){
      paquetesPorEntrar = paquetesPorEntrar.filterNot(_== pedido) 
    } else paquetesPorSalir = paquetesPorSalir.filterNot(_== pedido)
  }
  
  
  //Esta parte se puede mejorar y usar composicion (pero logro hacerlo andar)
  def filtrarTransportes(f: Transporte => Boolean): List[Transporte] = {
    for { transporte <- transportes if(f(transporte))} yield transporte
  }
  
  val filtroVacios : Transporte => Boolean = _.pedidos.isEmpty
  val filtroCargados: Transporte => Boolean = !_.pedidos.isEmpty
  
  val filtroValidos: (Transporte,Paquete) => Boolean = (transporte,paquete) => transporte.validar(paquete)
  
  def filtrarValidos : (Paquete, List[Transporte], (Transporte,Paquete) => Boolean) => List[Transporte] = {
    (paquete,trans,f) => for { transporte <- trans if(f(transporte,paquete))} yield transporte
  }
}

class CasaCentral(volumenDeposito : Int, override val pais : String) extends Sucursal(volumenDeposito, pais){
  override def esCasaCentral : Boolean = true
}

case class SucursalSinCapacidad() extends Exception

