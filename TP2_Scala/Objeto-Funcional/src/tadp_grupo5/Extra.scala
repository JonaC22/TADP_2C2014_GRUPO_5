package tadp_grupo5

abstract class Infraestructura {
	def costoAdicional(kilometros : Double) : Double
}

case object SustanciasPeligrosas extends Infraestructura(){
  override def costoAdicional(kilometros : Double) : Double = 600
}
case object Animales extends Infraestructura(){
  override def costoAdicional(kilometros : Double) : Double = {
    if(kilometros<100) 50 else if (kilometros<200) 86 else 137
  }
}

abstract class ServicioExtra {
	def costoAdicional(kilometros : Double) : Double
}

case object SeguimientoSatelital extends ServicioExtra(){
  override def costoAdicional(kilometros : Double) : Double = kilometros * 0.5
}
case object SeguimientoSatelitalConVideo extends ServicioExtra(){
  override def costoAdicional(kilometros : Double) : Double = kilometros * 3.74
}