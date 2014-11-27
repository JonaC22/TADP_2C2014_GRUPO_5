package tadp_grupo5

case class Despachante() {
	def agregarPedido(unTransporte: Transporte, pedido: Paquete) = {	  
	  unTransporte match {
			case Camion(_,_,_,_,_) => {unTransporte.validar(pedido); unTransporte.asInstanceOf[Camion].copy(pedidos = unTransporte.pedidos :+ pedido)}
			case Furgoneta(_,_,_,_,_) => {unTransporte.validar(pedido); unTransporte.asInstanceOf[Furgoneta].copy(pedidos = unTransporte.pedidos :+ pedido)}
			case Avion(_,_,_,_,_) => {unTransporte.validar(pedido); unTransporte.asInstanceOf[Avion].copy(pedidos = unTransporte.pedidos :+ pedido)}
			case _ => throw new TransporteInvalido()
		}
	}

	def vaciarTransporte(unTransporte: Transporte) = {
		unTransporte match {
			case Camion(_,_,_,_,_) => unTransporte.asInstanceOf[Camion].copy(pedidos = List())
			case Furgoneta(_,_,_,_,_) => unTransporte.asInstanceOf[Furgoneta].copy(pedidos = List())
			case Avion(_,_,_,_,_) => unTransporte.asInstanceOf[Avion].copy(pedidos = List())
			case _ => throw new TransporteInvalido()
		}
	}
	
	def modificarTiposValidos(unTransporte: Transporte, tipos: List[Caracteristica]) = {
	  unTransporte match {
			case Camion(_,_,_,_,_) => unTransporte.asInstanceOf[Camion].copy(tipoDePaquetesValidos = tipos)
			case Furgoneta(_,_,_,_,_) => unTransporte.asInstanceOf[Furgoneta].copy(tipoDePaquetesValidos = tipos)
			case Avion(_,_,_,_,_) => unTransporte.asInstanceOf[Avion].copy(tipoDePaquetesValidos = tipos)
			case _ => throw new TransporteInvalido()
		}
	}
	
	def modificarServicioExtra(unTransporte: Transporte, servicio: Option[ServicioExtra]) = {
	  unTransporte match {
			case Camion(_,_,_,_,_) => unTransporte.asInstanceOf[Camion].copy(servicioExtra = servicio)
			case Furgoneta(_,_,_,_,_) => unTransporte.asInstanceOf[Furgoneta].copy(servicioExtra = servicio)
			case Avion(_,_,_,_,_) => unTransporte.asInstanceOf[Avion].copy(servicioExtra = servicio)
			case _ => throw new TransporteInvalido()
		}
	}
	
	def modificarInfraestructura(unTransporte: Transporte, inf: Option[Infraestructura]) = {
	  unTransporte match {
			case Camion(_,_,_,_,_) => unTransporte.asInstanceOf[Camion].copy(infraestructura = inf)
			case Furgoneta(_,_,_,_,_) => unTransporte.asInstanceOf[Furgoneta].copy(infraestructura = inf)
			case Avion(_,_,_,_,_) => unTransporte.asInstanceOf[Avion].copy(infraestructura = inf)
			case _ => throw new TransporteInvalido()
		}
	}

}
