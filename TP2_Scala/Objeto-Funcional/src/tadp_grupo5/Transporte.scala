package tadp_grupo5

trait Transporte {
	val volumen: Double
	val costoKm: Double
	val velocidad: Double
	
	val tipoDePaquetesValidos: List[Caracteristica]
	val sistemaExterno : CalculadorDistancia
	val pedidos: List[Paquete]
	
	def costoBase: Double
	def costo: Double = costoBase + costosAdicionales
	def costosAdicionales: Double
	def capacidad: Double = volumen - pedidos.map(_.volumen).sum
		
	def puedeLlevar(nuevoPaquete: Paquete): Boolean = {
	  tipoDePaquetesValidos.contains(nuevoPaquete.caracteristica) && (capacidad > nuevoPaquete.volumen) && !(pedidos.size != 0 && (nuevoPaquete.sucursalDestino != pedidos.head.sucursalDestino))
	}
	
	def validar(nuevoPaquete: Paquete): Boolean = {
	  validarTipo(nuevoPaquete)
	  validarCapacidad(nuevoPaquete)
	  validarDestino(nuevoPaquete)
	  true
	}
	
	def validarTipo(pedido: Paquete) = if(tipoDePaquetesValidos.contains(pedido.caracteristica)) true else throw new PaqueteTipoInvalido()
		
	def validarCapacidad(nuevoPaquete: Paquete) = if (capacidad < nuevoPaquete.volumen) throw new TransporteSinCapacidad() else true
	
	def validarDestino(nuevoPaquete: Paquete) = {
		if (pedidos.size != 0 && nuevoPaquete.sucursalDestino != pedidos.head.sucursalDestino) throw new PaquetesDestinoErroneo() else true
	}
	
	def volumenOcupadoAceptable: Boolean = (volumen - capacidad) >= volumen * 0.20
	
	def sucursalOrigen: Sucursal = pedidos.head.sucursalOrigen
	def sucursalDestino: Sucursal = pedidos.head.sucursalDestino
	
	def distanciaEntre(origen: Sucursal, destino: Sucursal): Double = {
	  sistemaExterno.distanciaTerrestreEntre(origen, destino)
	}
	
	def paquetesUrgentes: List[Paquete] = for{ paquete <- pedidos if paquete.caracteristica == Urgente} yield paquete
	
	def hacerEnvio {
	  var envio: Envio = Envio(sucursalOrigen,sucursalDestino,pedidos,this)
	  sucursalOrigen.descargarEnvios(envio)
	  sucursalDestino.descargarEnvios(envio)
	}
	//agregados para los test
	def costoEnvio: Double = {
	  var envio: Envio = Envio(sucursalOrigen,sucursalDestino,pedidos,this,sistemaExterno.fechaActual)
	  envio.costoConAdicionales
	}
	
	def gananciaEnvio: Double = {
	  var envio: Envio = Envio(sucursalOrigen,sucursalDestino,pedidos,this,sistemaExterno.fechaActual)
	  envio.ganancia
	}
		
}

case class Camion(tipoDePaquetesValidos: List[Caracteristica] = List(Normal,NecesitaRefrigeracion), sistemaExterno : CalculadorDistancia, pedidos: List[Paquete] = List(), servicioExtra: Option[ServicioExtra] = None, infraestructura: Option[Infraestructura] = None) extends Transporte() {
  val volumen: Double = 45
  val costoKm: Double = 100
  val velocidad: Double = 60
  
  def multiplicadorVolumen: Double = {
    if(!volumenOcupadoAceptable && !sucursalDestino.esCasaCentral && !sucursalOrigen.esCasaCentral) 1 + ((volumen - capacidad)/ volumen) else 1
  }
  
  override def costoBase : Double = {
    costoKm * distanciaEntre(sucursalOrigen, sucursalDestino) * multiplicadorVolumen
  }
  
//  def costo : Double = costoBase + costosAdicionales
  
  override def costosAdicionales : Double = costoPeajes + costoExtras + costoSustanciasUrgentes
  
  def costoExtras : Double = costoSatelital + costoInfraestructura
  
  def costoSatelital: Double = {
    servicioExtra match {
      case Some(x) => x.costoAdicional(distanciaEntre(sucursalOrigen, sucursalDestino) * 2)
      case None => 0.0
    }
  }
  
  def costoInfraestructura: Double = {
    infraestructura match {
      case Some(x) => x.costoAdicional(distanciaEntre(sucursalOrigen, sucursalDestino))
      case None => 0.0
    }
  }
  
  def costoPeajes : Double = sistemaExterno.cantidadPeajesEntre(sucursalOrigen , sucursalDestino) * 12
  
  def costoSustanciasUrgentes : Double = {
    infraestructura match {
      case Some(SustanciasPeligrosas) => costoAdicionalPaquetesUrgentes
      case _ => 0.0
    }
  }
  
  def costoAdicionalPaquetesUrgentes : Double = {
    var volUrgentes : Double = (for { paquete <- paquetesUrgentes} yield paquete.volumen).sum
    3 * (volUrgentes/ volumen)
  }
}

case class Furgoneta(tipoDePaquetesValidos: List[Caracteristica] = List(Normal), sistemaExterno : CalculadorDistancia, pedidos: List[Paquete] = List(), servicioExtra: Option[ServicioExtra] = None, infraestructura: Option[Infraestructura] = None) extends Transporte(){
  val volumen: Double = 9
  val costoKm: Double = 40
  val velocidad: Double = 80
  
  def multiplicadorVolumen: Double = if(!volumenOcupadoAceptable && paquetesUrgentes.size < 3) 2 else 1
  
  override def costoBase: Double = costoKm * distanciaEntre(sucursalOrigen, sucursalDestino) * multiplicadorVolumen
  
//  def costo : Double = costoBase + costosAdicionales
  
  override def costosAdicionales : Double = costoPeajes + costoExtras
  
  def costoExtras : Double = costoSatelital + costoInfraestructura
  
  def costoSatelital: Double = {
    servicioExtra match {
      case Some(x) => x.costoAdicional(distanciaEntre(sucursalOrigen, sucursalDestino) * 2)
      case None => 0.0
    }
  }
  
  def costoInfraestructura: Double = {
    infraestructura match {
      case Some(x) => x.costoAdicional(distanciaEntre(sucursalOrigen, sucursalDestino) * 2)
      case None => 0.0
    }
  }
  
  def costoPeajes : Double = sistemaExterno.cantidadPeajesEntre(sucursalOrigen , sucursalDestino) * 6

}

case class Avion(tipoDePaquetesValidos: List[Caracteristica] = List(Normal), sistemaExterno : CalculadorDistancia, pedidos: List[Paquete] = List(), servicioExtra: Option[ServicioExtra] = None, infraestructura: Option[Infraestructura] = None) extends Transporte(){
  val volumen: Double = 200
  val costoKm: Double = 500
  val velocidad: Double = 500
  
  override def validarDestino(pedido: Paquete) = if(super.validarDestino(pedido) && distanciaEntre(pedido.sucursalOrigen ,pedido.sucursalDestino) > 1000) true else throw new EnvioConDistanciaMenorA1000KM()
  
  override def distanciaEntre(origen: Sucursal, destino: Sucursal): Double = {
	  sistemaExterno.distanciaAereaEntre(origen, destino)
  }
  
  def multiplicadorVolumen: Double = if(!volumenOcupadoAceptable) 3 else 1
  
  override def costoBase = costoKm * distanciaEntre(sucursalOrigen, sucursalDestino) * multiplicadorVolumen
  
//  def costo: Double = costoBase + costosAdicionales
  
  override def costosAdicionales: Double = costoExtras
  
  def costoExtras: Double = costoSatelital + costoInfraestructura
  
  def costoSatelital: Double = {
    servicioExtra match {
      case Some(x) => x.costoAdicional(distanciaEntre(sucursalOrigen, sucursalDestino) * 2)
      case None => 0.0
    }
  }
  
  def costoInfraestructura: Double = {
    infraestructura match {
      case Some(x) => x.costoAdicional(distanciaEntre(sucursalOrigen, sucursalDestino) * 2)
      case None => 0.0
    }
  }
}


abstract class TransporteException() extends Exception
case class PaqueteTipoInvalido() extends TransporteException
case class PaquetesDestinoErroneo() extends TransporteException
case class TransporteSinCapacidad() extends TransporteException
case class EnvioConDistanciaMenorA1000KM() extends TransporteException
case class TransporteInvalido() extends TransporteException




