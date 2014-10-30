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
    costoBasePaquetes + costoDelViaje + costoPeajes + costosAdicionales
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
  
  def costoPeajes : Double = sistemaExterno.cantidadPeajesEntre(sucursalOrigen, sucursalDestino)
  
  def costoBasePaquetes : Double = {
    pedidos.map(x => x.costo).sum
  }
  
  def costosAdicionales : Double = {
    0
  }
}

case class Camion(override var sistemaExterno : CalculadorDistancia) extends Transporte(45, 100, 60){
  override def distanciaEntreSucursales : Double = {
    sistemaExterno.distanciaTerrestreEntre(sucursalOrigen, sucursalDestino)
  }
  override def costoPeajes : Double = super.costoPeajes * 12
}
case class Furgoneta(override var sistemaExterno : CalculadorDistancia) extends Transporte(9,40,80){
  
  override def distanciaEntreSucursales : Double = {
    sistemaExterno.distanciaTerrestreEntre(sucursalOrigen, sucursalDestino)
  }
  
  override def costoPeajes : Double = super.costoPeajes * 6
}
case class Avion(override var sistemaExterno : CalculadorDistancia) extends Transporte(200,500,500){

  override def distanciaEntreSucursales : Double = {
    var distancia = sistemaExterno.distanciaAereaEntre(sucursalOrigen, sucursalDestino)
    if( distancia <= 1000 ) throw new EnvioConDistanciaMenorA1000KM()
    else distancia
  }
  
  override def costoPeajes : Double = 0
  
  override def costoEnvio: Double = {
    if(sucursalOrigen.pais != sucursalDestino.pais) super.costoEnvio * 1.1
    else super.costoEnvio
  }
}

case class PaquetesDestinoErroneo() extends Exception
case class TransporteSinCapacidad() extends Exception
case class EnvioConDistanciaMenorA1000KM() extends Exception