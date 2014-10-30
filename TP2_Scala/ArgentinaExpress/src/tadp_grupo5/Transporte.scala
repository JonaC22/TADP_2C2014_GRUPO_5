package tadp_grupo5

abstract class Transporte(volumen : Int, costo : Int, velocidad: Int){
  
  var sistemaExterno : CalculadorDistancia

  var pedidos : Seq[Paquete] = Seq()
  
  def sucursalOrigen : Sucursal = pedidos.head.sucursalOrigen 
  
  def sucursalDestino : Sucursal = pedidos.head.sucursalDestino 
  
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
    
    if(pedidos.size != 0) destino = this.sucursalDestino
    
    if(paquetes.exists(x => x.sucursalDestino != destino)) throw new PaquetesDestinoErroneo()
  }
  
  def distanciaEntreSucursales : Double 
  
  def costoEnvio : Double = {
    costoBasePaquetes + costoDelViaje
  }
  
  def gananciaEnvio : Double = {
    precioPaquetes - costoEnvio
  }
  
  def precioPaquetes : Double = {
    pedidos.map(x => x.precio).sum
  }
  
  def costoDelViaje : Double = {
    costo * distanciaEntreSucursales
  }
  
  def costoPeajes : Double = {
    sistemaExterno.cantidadPeajesEntre(sucursalOrigen, sucursalDestino)
  }
  
  def costoBasePaquetes : Double = {
    pedidos.map(x => x.costo).sum
  }
  
  def costosAdicionales : Int = {
    5// aca se deben sumar peajes, costo por refrigeracion, impuesto(aviones), etc
  }
}

case class Camion(override var sistemaExterno : CalculadorDistancia) extends Transporte(45, 100, 60){
  override def distanciaEntreSucursales : Double = {
    sistemaExterno.distanciaTerrestreEntre(sucursalOrigen, sucursalDestino)
  }
}
case class Furgoneta(override var sistemaExterno : CalculadorDistancia) extends Transporte(9,40,80){
  
  override def distanciaEntreSucursales : Double = {
    sistemaExterno.distanciaTerrestreEntre(sucursalOrigen, sucursalDestino)
  }
}
case class Avion(override var sistemaExterno : CalculadorDistancia) extends Transporte(200,500,500){

  override def distanciaEntreSucursales : Double = {
    sistemaExterno.distanciaAereaEntre(sucursalOrigen, sucursalDestino)
  }
  
  //debe tirar una excepcion si es utilizado para distancias menor o igual a 1000km
}

case class PaquetesDestinoErroneo() extends Exception
case class TransporteSinCapacidad() extends Exception