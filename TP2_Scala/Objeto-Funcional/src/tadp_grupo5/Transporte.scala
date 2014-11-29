package tadp_grupo5

case class Transporte(
    sistemaExterno : CalculadorDistancia, 
    tipoTransporte : TipoTransporte,
    tipoDePaquetesValidos: List[Caracteristica] = List(Normal), 
    pedidos: List[Paquete] = List(), 
    servicioExtra: Option[ServicioExtra] = None, 
    infraestructura: Option[Infraestructura] = None) {
	
	def costoBase: Double = tipoTransporte.costoBase(this)
	def costosAdicionales: Double = tipoTransporte.costosAdicionales(this)
	def costo: Double = costoBase + costosAdicionales
	def capacidad: Double = tipoTransporte.volumen - pedidos.map(_.volumen).sum
	def velocidad: Double = tipoTransporte.velocidad
	
	def puedeLlevar(nuevoPaquete: Paquete): Boolean = {
	  validarTipo(nuevoPaquete) && validarCapacidad(nuevoPaquete) && validarDestino(nuevoPaquete)
	}
	
	def validarTipo(pedido: Paquete) : Boolean = 
	  if((tipoTransporte match {
	   			case _ : Camion => pedido.caracteristica == NecesitaRefrigeracion
	   			case _ => false
	   }) || tipoDePaquetesValidos.contains(pedido.caracteristica) ) true else throw new PaqueteTipoInvalido()
		
	def validarCapacidad(nuevoPaquete: Paquete) : Boolean = if (capacidad < nuevoPaquete.volumen) throw new TransporteSinCapacidad() else true
	
	def validarDestino(nuevoPaquete: Paquete) : Boolean = if (pedidos.size != 0 && nuevoPaquete.sucursalDestino != pedidos.head.sucursalDestino) throw new PaquetesDestinoErroneo() else true
	
	def volumenOcupadoAceptable: Boolean = (tipoTransporte.volumen - capacidad) >= tipoTransporte.volumen * 0.20
	
	def sucursalOrigen: Sucursal = pedidos.head.sucursalOrigen
	def sucursalDestino: Sucursal = pedidos.head.sucursalDestino
	
	def envioNoAceptableCamion : Boolean = !volumenOcupadoAceptable && !sucursalDestino.esCasaCentral && !sucursalOrigen.esCasaCentral
	
	def distanciaEntreSucursales = tipoTransporte match{
	  case _ : Avion => sistemaExterno.distanciaAereaEntre(sucursalOrigen, sucursalDestino)
	  case _ => sistemaExterno.distanciaTerrestreEntre(sucursalOrigen, sucursalDestino)
	}
	
	def cantidadPeajes : Double = sistemaExterno.cantidadPeajesEntre(sucursalOrigen, sucursalDestino)
	
	def paquetesUrgentes: List[Paquete] = for{ paquete <- pedidos if paquete.caracteristica == Urgente} yield paquete
	
	def hacerEnvio : Transporte ={
	  tipoTransporte match {
	    case _ : Avion => if (distanciaEntreSucursales < 1000) throw new EnvioConDistanciaMenorA1000KM()
	    case _ =>
	  }
	  var envio: Envio = Envio(this, distanciaEntreSucursales, sistemaExterno.fechaActual)
	  sucursalOrigen.descargarEnvio(envio)
	  sucursalDestino.descargarEnvio(envio)
	  this.vaciarTransporte
	}

	def costoEnvio: Double = {
	  Envio(this, distanciaEntreSucursales, sistemaExterno.fechaActual).costoConAdicionales
	}
	
	def gananciaEnvio: Double = {
	  Envio(this, distanciaEntreSucursales, sistemaExterno.fechaActual).ganancia
	}
	
	def agregarPedido(pedido: Paquete) = {	 
		puedeLlevar(pedido)
		this.copy(pedidos = pedidos :+ pedido)
	}

	def vaciarTransporte = this.copy(pedidos = List())
	
	def modificarTiposValidos(tipos: List[Caracteristica]) = this.copy(tipoDePaquetesValidos = tipos)
	
	def modificarServicioExtra(servicio: Option[ServicioExtra]) = this.copy(servicioExtra = servicio)
	
	def modificarInfraestructura(inf: Option[Infraestructura]) = this.copy(infraestructura = inf)

}

abstract class TipoTransporte(val volumen : Double, val costoKm : Double, val velocidad : Double) {
  
	def multiplicadorVolumen(transporte : Transporte) : Double = 0
	
	def distancia(transporte : Transporte) : Double = transporte.distanciaEntreSucursales
	
	def costoBase(transporte : Transporte) : Double = costoKm * distancia(transporte) * multiplicadorVolumen(transporte)
	
	def costosAdicionales(transporte : Transporte) : Double = costoPeajes(transporte) + costoExtras(transporte) + costoSustanciasUrgentes(transporte)
	
	def costoPeajes(transporte : Transporte) : Double = transporte.cantidadPeajes
	
	def costoSustanciasUrgentes(transporte : Transporte) : Double = 0
	
	def costoExtras(transporte : Transporte) : Double = costoSatelital(transporte) + costoInfraestructura(transporte)
	
	def costoSatelital(transporte : Transporte): Double = {
		transporte.servicioExtra match {
			case Some(x) => x.costoAdicional(distancia(transporte) * 2)
			case None => 0.0
    	}
	}
  
	def costoInfraestructura(transporte : Transporte): Double = {
		transporte.infraestructura match {
	      case Some(x) => x.costoAdicional(distancia(transporte) * 2)
	      case None => 0.0
	    }
	}
}

case class Camion() extends TipoTransporte(45, 100, 60) {
  
  override def multiplicadorVolumen(transporte : Transporte): Double = {
    if(transporte.envioNoAceptableCamion) 1 + ((volumen - transporte.capacidad)/ volumen) else 1
  }
  
  override def costoSatelital(transporte : Transporte) : Double = {
    transporte.servicioExtra match {
      case Some(x) => x.costoAdicional(distancia(transporte) * 2)
      case None => 0.0
    }
  }
  
  override def costoInfraestructura(transporte : Transporte): Double = {
    transporte.infraestructura match {
      case Some(x) => x.costoAdicional(distancia(transporte))
      case None => 0.0
    }
  }
  
  override def costoPeajes(transporte : Transporte) : Double = super.costoPeajes(transporte) * 12
  
  override def costoSustanciasUrgentes(transporte : Transporte) : Double = {
    transporte.infraestructura match {
      case Some(SustanciasPeligrosas) => costoAdicionalPaquetesUrgentes(transporte)
      case _ => 0.0
    }
  }
  
  def costoAdicionalPaquetesUrgentes(transporte : Transporte) : Double = {
    var volUrgentes : Double = (for { paquete <- transporte.paquetesUrgentes} yield paquete.volumen).sum
    3 * (volUrgentes/ volumen)
  }
}

case class Furgoneta() extends TipoTransporte(9, 40, 80){
  
  override def multiplicadorVolumen (transporte : Transporte): Double = if(!transporte.volumenOcupadoAceptable && transporte.paquetesUrgentes.size < 3) 2 else 1
  
  override def costoPeajes(transporte : Transporte) : Double = super.costoPeajes(transporte) * 6

}

case class Avion() extends TipoTransporte(200, 500, 500){
  
  override def costoPeajes(transporte : Transporte) : Double = 0.0
  
  override def multiplicadorVolumen(transporte : Transporte) : Double = if(!transporte.volumenOcupadoAceptable) 3 else 1
}


abstract class TransporteException() extends Exception
case class PaqueteTipoInvalido() extends TransporteException
case class PaquetesDestinoErroneo() extends TransporteException
case class TransporteSinCapacidad() extends TransporteException
case class EnvioConDistanciaMenorA1000KM() extends TransporteException
case class TransporteInvalido() extends TransporteException




