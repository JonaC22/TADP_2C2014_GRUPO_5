package tadp_grupo5

abstract class Caracteristica

case class Normal() extends Caracteristica
case class Urgente() extends Caracteristica
case class NecesitaRefrigeracion() extends Caracteristica
case class Fragil() extends Caracteristica