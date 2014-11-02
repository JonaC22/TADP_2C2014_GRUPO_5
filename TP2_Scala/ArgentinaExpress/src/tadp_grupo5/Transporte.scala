package tadp_grupo5

abstract class Transporte(volumen : Int, costo : Int, velocidad: Int){
  
  var sistemaExterno : CalculadorDistancia
  
  var servicioExtra : Option[ServicioExtra]//puede tener seguimiento satelital, seguimiento satelital con video o ninguno de los dos

  var pedidos : Seq[Paquete] = Seq()
  
  def sucursalOrigen : Sucursal = pedidos.head.sucursalOrigen 
  
  def sucursalDestino : Sucursal = pedidos.head.sucursalDestino 
  
  def capacidad : Int =  volumen - pedidos.map(_.volumen).sum
  
  def asignarPaquetes(nuevosPaquetes : Seq[Paquete]) {
    validarPaquetes(nuevosPaquetes)
    pedidos = pedidos ++ nuevosPaquetes
  }
  
  def validarPaquetes(nuevosPaquetes : Seq[Paquete]) {
    validarCapacidad(nuevosPaquetes)
    validarDestinoPaquetes(nuevosPaquetes)
  }
  
  def validarCapacidad(nuevosPaquetes : Seq[Paquete]) {
	if(capacidad < nuevosPaquetes.map(_.volumen).sum) throw new TransporteSinCapacidad()
  }
  
  def validarDestinoPaquetes(paquetes : Seq[Paquete]) {
    var destino = paquetes.head.sucursalDestino
    
    if(pedidos.size != 0) destino = this.sucursalDestino
    
    if(paquetes.exists(x => x.sucursalDestino != destino)) throw new PaquetesDestinoErroneo()
  }
  
  def volumenOcupadoAceptable : Boolean = capacidad >= volumen * 0.20 // si es mayor o igual al 20% es aceptable
  
  def distanciaEntreSucursales : Double 
  
  def costoConCasaCentral : Double = sucursalDestino.esCasaCentral(this)
  
  def costoEnvio : Double = costoBasePaquetes + costoDelViaje
  
  def costoEnvioConAdicionales : Double = costoEnvio + costosAdicionales
  
  def gananciaEnvio : Double = precioPaquetes - costoEnvioConAdicionales
  
  def precioPaquetes : Double = pedidos.map(_.precio).sum
  
  def costoDelViaje : Double = costo * distanciaEntreSucursales
  
  def costoPeajes : Double = sistemaExterno.cantidadPeajesEntre(sucursalOrigen, sucursalDestino)
  
  def costoBasePaquetes : Double = pedidos.map(_.costo).sum
  
  def costoAdicionalCamionCasaCentral : Double = 0.0
  
  def costoServicioExtra : Double = {
    if(!servicioExtra.isEmpty){
      servicioExtra.get.costoAdicional(distanciaEntreSucursales * 2) // ida y vuelta
    }
    else 0.0
  }
  
  def costosAdicionales : Double = costoPeajes + costoServicioExtra
}

case class Camion(override var sistemaExterno : CalculadorDistancia, override var servicioExtra : Option[ServicioExtra]) extends Transporte(45, 100, 60){
  override def distanciaEntreSucursales : Double = sistemaExterno.distanciaTerrestreEntre(sucursalOrigen, sucursalDestino)
  
  override def costoPeajes : Double = super.costoPeajes * 12
  
  override def costoAdicionalCamionCasaCentral : Double = costoDelViaje * 0.02
  
  override def costosAdicionales : Double = super.costosAdicionales + costoConCasaCentral
}

case class Furgoneta(override var sistemaExterno : CalculadorDistancia, override var servicioExtra : Option[ServicioExtra]) extends Transporte(9,40,80){
  
  override def distanciaEntreSucursales : Double = sistemaExterno.distanciaTerrestreEntre(sucursalOrigen, sucursalDestino)
  
  override def costoPeajes : Double = super.costoPeajes * 6
}

case class Avion(override var sistemaExterno : CalculadorDistancia, override var servicioExtra : Option[ServicioExtra]) extends Transporte(200,500,500){

  override def distanciaEntreSucursales : Double = {
    var distancia = sistemaExterno.distanciaAereaEntre(sucursalOrigen, sucursalDestino)
    if( distancia <= 1000 ) throw new EnvioConDistanciaMenorA1000KM()
    else distancia
  }
  
  override def costoPeajes : Double = 0.0
  
  override def costosAdicionales: Double = {
    if(sucursalOrigen.pais != sucursalDestino.pais) super.costosAdicionales + costoDelViaje * 0.1
    else super.costoDelViaje   
  }
}

case class PaquetesDestinoErroneo() extends Exception
case class TransporteSinCapacidad() extends Exception
case class EnvioConDistanciaMenorA1000KM() extends Exception