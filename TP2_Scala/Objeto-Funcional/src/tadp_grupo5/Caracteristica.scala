package tadp_grupo5

abstract class Caracteristica(costoBase : Double, precioBase : Double) {
  def costo : Double = costoBase
  def precio : Double = precioBase
}

case object Normal extends Caracteristica(10,80)
case object Urgente extends Caracteristica(20,110)
case object NecesitaRefrigeracion extends Caracteristica(70,210){
  override def costo : Double = super.costo + 5 //costoBase + $5 por necesitar refrigeracion
}
case object Fragil extends Caracteristica(18,120)