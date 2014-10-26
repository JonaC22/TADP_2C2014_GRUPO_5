package tadp_grupo5

abstract class Transporte(volumen : Int, costo : Int, velocidad: Int){

  var pedidos : Seq[Paquete] = Seq()
  
  def capacidad : Int = {
	  volumen - pedidos.map(_.volumen).sum
  }
  
  def asignarPaquetes(nuevosPaquetes : Seq[Paquete]) : Unit = {
    validarPaquetes(nuevosPaquetes)
    pedidos = pedidos ++ nuevosPaquetes
  }
  
  def validarPaquetes(nuevosPaquetes : Seq[Paquete]) : Unit = {
    validarCapacidad(nuevosPaquetes)
    validarDestinoPaquetes(nuevosPaquetes)
  }
  
  def validarCapacidad(nuevosPaquetes : Seq[Paquete]) {
	if(capacidad < nuevosPaquetes.map(_.volumen).sum) throw new TransporteSinCapacidad()
  }
  
  def validarDestinoPaquetes(paquetes : Seq[Paquete]) : Unit = {
    var destino = paquetes.head.sucursalDestino
    
    if(pedidos.size != 0) destino = pedidos.head.sucursalDestino
    
    if(paquetes.exists(x => x.sucursalDestino != destino)) throw new PaquetesDestinoErroneo()
  }
  
  def costoPeajes : Int
  
  def distanciaEntreSucursales : Double
  
  def costoDelViaje : Double = {
    costoBasePaquetes + costo * distanciaEntreSucursales + costosAdicionales
  }
  
  def costoBasePaquetes : Int = {
   pedidos.foldLeft(0)((b,a) => b+a.costoBase)
  }
  
  def costosAdicionales : Int = {
    5// aca se deben sumar peajes, costo por refrigeracion, impuesto(aviones), etc
  }
}

case class Camion() extends Transporte(45, 100, 60){
  override def costoPeajes : Int = 12// * calculadorDistancia.cantidadPeajesEntre
  override def distanciaEntreSucursales : Double = 999//calculadorDistancia.distanciaTerrestre
}
case class Furgoneta() extends Transporte(9,40,80){
  override def costoPeajes : Int = 6// * calculadorDistancia.cantidadPeajesEntre
  override def distanciaEntreSucursales : Double = 999//calculadorDistancia.distanciaTerrestre
}
case class Avion() extends Transporte(200,500,500){
  override def costoPeajes : Int = 0
  override def distanciaEntreSucursales : Double = 999//calculadorDistancia.distanciaAerea
  
  //debe tirar una excepcion si es utilizado para distancias menor o igual a 1000km
}

case class PaquetesDestinoErroneo() extends Exception
case class TransporteSinCapacidad() extends Exception