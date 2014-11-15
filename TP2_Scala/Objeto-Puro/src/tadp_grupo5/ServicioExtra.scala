package tadp_grupo5

abstract class ServicioExtra {
	def costoAdicional(kilometros : Double) : Double
}

case object SeguimientoSatelital extends ServicioExtra(){
  override def costoAdicional(kilometros : Double) : Double = kilometros * 0.5
}
case object SeguimientoSatelitalConVideo extends ServicioExtra(){
  override def costoAdicional(kilometros : Double) : Double = kilometros * 3.74
}
