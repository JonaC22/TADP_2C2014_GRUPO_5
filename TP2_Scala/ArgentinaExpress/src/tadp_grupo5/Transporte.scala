package tadp_grupo5

class Transporte(volumen : Int, costo : Int, velocidad: Int){
  var pedidos : Seq[Paquete] = Seq()
  
  def capacidad : Int = {
	  volumen - pedidos.map(_.volumen).sum
  }
  
  def asignarPaquetes(nuevosPaquetes : Seq[Paquete]) : Unit = {
    if(capacidad < nuevosPaquetes.map(_.volumen).sum) throw new TransporteSinCapacidad()
    else pedidos = pedidos ++ nuevosPaquetes
  }
}

case class Camion() extends Transporte(45, 100, 60)
case class Furgoneta() extends Transporte(9,40,80)
case class Avion() extends Transporte(200,500,500){
  //debe tirar una excepcion si es utilizado para distancias menor o igual a 1000km
}

case class TransporteSinCapacidad() extends Exception