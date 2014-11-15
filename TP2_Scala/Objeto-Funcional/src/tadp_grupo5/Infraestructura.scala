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