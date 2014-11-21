package tadp_grupo5

import scala.collection.immutable.List
import java.util.Date

abstract class Transporte(val volumen: Double, costo: Double, val velocidad: Double) {
  
  type Infraestructura = Double => Double
  type ServicioExtra = Double => Double
  
  val seguimientoSatelital : ServicioExtra = kilometros => kilometros * 0.5
  
  val seguimientoSatelitalConVideo : ServicioExtra = kilometros => kilometros * 3.74
  
  val sustanciasPeligrosas : Infraestructura = kilometros => 600
  
  val animales : Infraestructura = {
    case kilometros if kilometros < 100 => 50
    case kilometros if kilometros < 200 => 86
    case _ => 137
  }
  
  var servicioExtra : Option[ServicioExtra] = None
  
  var infraestructura: Option[Infraestructura] = None

  var sistemaExterno: CalculadorDistancia

  var pedidos: List[Paquete] = List()

  var historialEnvios: List[Envio] = List()
  
  var tipoDePaquetesValidos : List[Caracteristica] = List(Normal)

  def sucursalOrigen: Sucursal = pedidos.head.sucursalOrigen

  def sucursalDestino: Sucursal = pedidos.head.sucursalDestino

  def capacidad: Double = volumen - pedidos.map(_.volumen).sum
  
  def hacerEnvio {
    agregarEnvioAHistorial
    descargarTransporte
  }

  def agregarEnvioAHistorial = historialEnvios = historialEnvios :+ new Envio(sucursalOrigen, sucursalDestino, pedidos.toList, distanciaEntreSucursales, gananciaEnvio, costoEnvioConAdicionales, sistemaExterno.fechaActual)

  def descargarTransporte {
    sucursalOrigen.descargarEnvios(pedidos)
    sucursalDestino.descargarEnvios(pedidos)
    pedidos = List()
  }

  def asignarPaquete(nuevoPaquete : Paquete) {
    validarPaquete(nuevoPaquete)
    pedidos = pedidos :+ nuevoPaquete
  }
  
  def puedeLlevarPaquete(nuevoPaquete : Paquete) : Boolean = {
    try { 
      validarPaquete(nuevoPaquete) 
      true
    } 
    catch {
      case tex : TransporteException => false
      case ex : Exception => throw ex
    }
  }

  def validarPaquete(nuevoPaquete : Paquete) {
    validarTipoDePaquete(nuevoPaquete)
    validarCapacidad(nuevoPaquete)
    validarDestinoPaquete(nuevoPaquete)
  }
  
  def validarTipoDePaquete(paquete: Paquete) = if(!puedeLlevarTipoDePaquete(paquete)) throw new PaqueteTipoInvalido()
  
  def puedeLlevarTipoDePaquete(paquete : Paquete) : Boolean = {
    paquete.caracteristica match {
      case NecesitaRefrigeracion => false //el camion es el unico que puede llevar paquetes que necesitan refrigeracion
      case _ => tipoDePaquetesValidos.contains(paquete.caracteristica)
    }
  }

  val capacidadEsValida: Paquete => Boolean = capacidad < _.volumen
  
  def validarCapacidad(nuevoPaquete: Paquete) = if (capacidadEsValida(nuevoPaquete)) throw new TransporteSinCapacidad()

  val destinoEsValido: Paquete => Boolean = {pedidos.size != 0 && _.sucursalDestino != sucursalDestino}
  
  def validarDestinoPaquete(nuevoPaquete: Paquete) {
    if (destinoEsValido(nuevoPaquete)) throw new PaquetesDestinoErroneo()
  }

  def volumenOcupadoAceptable: Boolean = (volumen - capacidad) >= volumen * 0.20 // si es mayor o igual al 20% es aceptable
  
  def distanciaEntreSucursales: Double

  def costoConCasaCentral: Double = if(sucursalDestino.esCasaCentral) costoAdicionalCasaCentral else 0.0

  def costoEnvio: Double = costoBasePaquetes + costoDelViaje

  def costoEnvioConAdicionales: Double = costoEnvio + costosAdicionales

  def gananciaEnvio: Double = precioPaquetes - costoEnvioConAdicionales

  def precioPaquetes: Double = (for{ pedido <- pedidos } yield pedido.precio).sum //pedidos.map(_.precio).sum

  def costoDelViaje: Double = costo * distanciaEntreSucursales

  def costoPeajes: Double = sistemaExterno.cantidadPeajesEntre(sucursalOrigen, sucursalDestino)

  def costoBasePaquetes: Double = (for{ pedido <- pedidos } yield pedido.costo).sum //pedidos.map(_.costo).sum

  def costoAdicionalCasaCentral: Double = 0.0

  def costoServicioExtra: Double = {
    servicioExtra match {
      case Some(x) => x(distanciaEntreSucursales * 2)// ida y vuelta
      case None => 0.0
    }
  }
  
  def costoInfraestructura : Double = { //se asume que si un transporte tiene una infraestructura, los paquetes que lleva son para tal
    infraestructura match {
      case Some(x) => x(distanciaEntreSucursales)
      case None => 0.0
    }
  }
  
  def costoSustanciasUrgentes : Double = 0.0
  
  def costoVolumen : Double = 0.0
  
  def costosAdicionales: Double = costoPeajes + costoConCasaCentral + costoServicioExtra + costoInfraestructura + costoSustanciasUrgentes + costoVolumen
     
  def paquetesUrgentes: List[Paquete] = for {paquete <- pedidos if paquete.caracteristica == Urgente} yield paquete
}

case class Camion(override var sistemaExterno: CalculadorDistancia) extends Transporte(45, 100, 60) {
  override def distanciaEntreSucursales: Double = sistemaExterno.distanciaTerrestreEntre(sucursalOrigen, sucursalDestino)

  override def costoPeajes: Double = super.costoPeajes * 12

  override def costoAdicionalCasaCentral: Double = if(sistemaExterno.fechaActual.getDate() > 21) costoEnvio * 0.02 else 0.0
  
  override def puedeLlevarTipoDePaquete(paquete : Paquete) : Boolean = {
    paquete.caracteristica match {
      case NecesitaRefrigeracion => true
      case _ => super.puedeLlevarTipoDePaquete(paquete)
    }
  }
  
  override def costoSustanciasUrgentes : Double = {
    infraestructura match {
      case Some(sustanciasPeligrosas) => costoAdicionalPaquetesUrgentes
      case _ => 0.0
    }
  }
  
  def costoAdicionalPaquetesUrgentes : Double = {
    var volUrgentes : Double = (for { paquete <- paquetesUrgentes} yield paquete.volumen).sum
    3 * (volUrgentes/ volumen)
  }
  
  override def costoVolumen: Double = if (!volumenOcupadoAceptable && !sucursalDestino.esCasaCentral && !sucursalOrigen.esCasaCentral) costoEnvio * ((volumen - capacidad)/ volumen) else 0.0
  
}

case class Furgoneta(override var sistemaExterno: CalculadorDistancia) extends Transporte(9, 40, 80) {

  override def distanciaEntreSucursales: Double = sistemaExterno.distanciaTerrestreEntre(sucursalOrigen, sucursalDestino)

  override def costoPeajes: Double = super.costoPeajes * 6
  
  override def costoVolumen: Double = {
    var cantidadUrgentes : Int = paquetesUrgentes.size
    if (!volumenOcupadoAceptable && cantidadUrgentes < 3) costoEnvio else 0.0
  }

}

case class Avion(override var sistemaExterno: CalculadorDistancia) extends Transporte(200, 500, 500) {

  override def distanciaEntreSucursales: Double = {
    var distancia = sistemaExterno.distanciaAereaEntre(sucursalOrigen, sucursalDestino)
    if (distancia <= 1000) throw new EnvioConDistanciaMenorA1000KM()
    else distancia
  }

  override def costoPeajes: Double = 0.0 //los aviones no tienen costo de peaje

  override def costoAdicionalCasaCentral: Double = if(sistemaExterno.fechaActual.getDate() > 20) -costoEnvio * 0.2 else 0.0
  
  override def costosAdicionales: Double = {
    if (sucursalOrigen.pais != sucursalDestino.pais) super.costosAdicionales + costoEnvio* 0.1 //se le suma 10% de impuestos si son envios internacionales
    else super.costosAdicionales
  }
  
  override def costoVolumen: Double = if (!volumenOcupadoAceptable) costoEnvio * 2 else 0.0 //si el volumen ocupado es menor al 20% el costo de envio se contabiliza 2 veces mas

}

abstract class TransporteException() extends Exception
case class PaqueteTipoInvalido() extends TransporteException
case class PaquetesDestinoErroneo() extends TransporteException
case class TransporteSinCapacidad() extends TransporteException
case class EnvioConDistanciaMenorA1000KM() extends TransporteException