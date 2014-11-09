package tadp_grupo5

import scala.collection.mutable.Queue
import scala.collection.mutable.Buffer
import java.util.Date

abstract class Transporte(val volumen: Int, costo: Int, velocidad: Int, var servicioExtra: Option[ServicioExtra] = None, var infraestructura: Option[Infraestructura] = None) {

  var sistemaExterno: CalculadorDistancia

  var pedidos: Buffer[Paquete] = Buffer()

  var historialEnvios: Queue[Envio] = Queue()
  
  var tipoDePaquetesValidos : Buffer[Caracteristica] = Buffer(Normal)

  def sucursalOrigen: Sucursal = pedidos.head.sucursalOrigen

  def sucursalDestino: Sucursal = pedidos.head.sucursalDestino

  def capacidad: Int = volumen - pedidos.map(_.volumen).sum

  def hacerEnvio {
    agregarEnvioAHistorial
    descargarTransporte
  }

  def agregarEnvioAHistorial {
    historialEnvios enqueue new Envio(sucursalOrigen, sucursalDestino, pedidos, distanciaEntreSucursales, gananciaEnvio, costoEnvio)
  }

  def descargarTransporte {
    sucursalOrigen.descargarEnvios(pedidos)
    sucursalDestino.descargarEnvios(pedidos)
    pedidos = Buffer()
  }

  def asignarPaquete(nuevoPaquete : Paquete) {
    validarPaquete(nuevoPaquete)
    pedidos += nuevoPaquete
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
  
  def validarTipoDePaquete(paquete: Paquete) {
    if(!puedeLlevarTipoDePaquete(paquete)){
      throw new PaqueteTipoInvalido()
    }
  }
  
  def puedeLlevarTipoDePaquete(paquete : Paquete) : Boolean = {
    if(paquete.caracteristica != NecesitaRefrigeracion){ //el camion es el unico que puede llevar paquetes que necesitan refrigeracion
      tipoDePaquetesValidos.contains(paquete.caracteristica)
    }
    else{
      false
    }
  }

  def validarCapacidad(nuevoPaquete: Paquete) {
    if (capacidad < nuevoPaquete.volumen) throw new TransporteSinCapacidad()
  }

  def validarDestinoPaquete(nuevoPaquete: Paquete) {
    if (pedidos.size != 0 && nuevoPaquete.sucursalDestino != sucursalDestino) throw new PaquetesDestinoErroneo()
  }

  def volumenOcupadoAceptable: Boolean = capacidad >= volumen * 0.20 // si es mayor o igual al 20% es aceptable
  
  def distanciaEntreSucursales: Double

  def costoConCasaCentral: Double = if(sucursalDestino.esCasaCentral) costoAdicionalCasaCentral else 0

  def costoEnvio: Double = costoBasePaquetes + costoDelViaje

  def costoEnvioConAdicionales: Double = costoEnvio + costosAdicionales

  def gananciaEnvio: Double = precioPaquetes - costoEnvioConAdicionales

  def precioPaquetes: Double = pedidos.map(_.precio).sum

  def costoDelViaje: Double = costo * distanciaEntreSucursales

  def costoPeajes: Double = sistemaExterno.cantidadPeajesEntre(sucursalOrigen, sucursalDestino)

  def costoBasePaquetes: Double = pedidos.map(_.costo).sum

  def costoAdicionalCasaCentral: Double = 0.0

  def costoServicioExtra: Double = {
    if (!servicioExtra.isEmpty) {
      servicioExtra.get.costoAdicional(distanciaEntreSucursales * 2) // ida y vuelta
    } else 0.0
  }
  
  def costoInfraestructura : Double = {
    if (!infraestructura.isEmpty) {
      infraestructura.get.costoAdicional(distanciaEntreSucursales)
    } else 0.0
  }
  
  def costoSustanciasUrgentes : Double = {
    0.0
  }
  
  def costosAdicionales: Double = costoPeajes + costoServicioExtra + costoInfraestructura + costoSustanciasUrgentes
}

case class Camion(override var sistemaExterno: CalculadorDistancia) extends Transporte(45, 100, 60) {
  override def distanciaEntreSucursales: Double = sistemaExterno.distanciaTerrestreEntre(sucursalOrigen, sucursalDestino)

  override def costoPeajes: Double = super.costoPeajes * 12

  override def costoAdicionalCasaCentral: Double = costoDelViaje * 0.02

  override def costosAdicionales: Double = super.costosAdicionales + costoConCasaCentral
  
  override def puedeLlevarTipoDePaquete(paquete : Paquete) : Boolean = {
    if(paquete.caracteristica == NecesitaRefrigeracion){
      true
    }
    else {
      super.puedeLlevarTipoDePaquete(paquete)
    }
  }
  
  override def costoSustanciasUrgentes : Double = {
    if (infraestructura == Some(SustanciasPeligrosas)) costoAdicionalPaquetesUrgentes else 0
  }
  
  def costoAdicionalPaquetesUrgentes : Double = {
    var volUrgentes : Double = pedidos.filter(pedido => pedido.caracteristica == Urgente).map(_.volumen).sum
    3 * (volUrgentes / volumen)
    //ver si es por paquete urgente individualmente o en conjunto
  }
  
  override def costoEnvio: Double = {
    if (!volumenOcupadoAceptable && (sucursalDestino.esCasaCentral || sucursalOrigen.esCasaCentral))
    super.costoEnvio * (1 + capacidad - volumen) else super.costoEnvio
  }
  
}

case class Furgoneta(override var sistemaExterno: CalculadorDistancia) extends Transporte(9, 40, 80) {

  override def distanciaEntreSucursales: Double = sistemaExterno.distanciaTerrestreEntre(sucursalOrigen, sucursalDestino)

  override def costoPeajes: Double = super.costoPeajes * 6
  
  override def costoEnvio: Double = {
    var cantidadUrgentes : Int = pedidos.filter(pedido => pedido.caracteristica == Urgente).size
    if (!volumenOcupadoAceptable && cantidadUrgentes < 3) super.costoEnvio * 2 else super.costoEnvio
  }
}

case class Avion(override var sistemaExterno: CalculadorDistancia) extends Transporte(200, 500, 500) {

  override def distanciaEntreSucursales: Double = {
    var distancia = sistemaExterno.distanciaAereaEntre(sucursalOrigen, sucursalDestino)
    if (distancia <= 1000) throw new EnvioConDistanciaMenorA1000KM()
    else distancia
  }

  override def costoPeajes: Double = 0.0

  override def costoAdicionalCasaCentral: Double = {
    var fecha: Date = new Date() //tomo la fecha de consulta del costo como si fuera la fecha de envio
    if(fecha.getDay() > 20) -costoDelViaje * 0.2 else 0
  }
  
  override def costosAdicionales: Double = {
    if (sucursalOrigen.pais != sucursalDestino.pais) super.costosAdicionales + costoDelViaje * 0.1 + costoConCasaCentral
    else super.costoDelViaje
  }
  
  override def costoEnvio: Double = if (!volumenOcupadoAceptable) super.costoEnvio * 3 else super.costoEnvio
}

abstract class TransporteException() extends Exception
case class PaqueteTipoInvalido() extends TransporteException
case class PaquetesDestinoErroneo() extends TransporteException
case class TransporteSinCapacidad() extends TransporteException
case class EnvioConDistanciaMenorA1000KM() extends TransporteException