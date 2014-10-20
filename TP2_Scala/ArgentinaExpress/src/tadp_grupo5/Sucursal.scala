package tadp_grupo5

class Sucursal (volumenDeposito : Int) {
  var paquetesEnSalir : Set[Paquete] = Set()
  var paquetesEnEntrar : Set[Paquete] = Set()
  
  def volumenTotal : Int = {
    val volumen = volumenDeposito - paquetesEnEntrar.map(_.volumen).sum - paquetesEnSalir.map(_.volumen).sum
    if(volumen < 1) throw new SucursalSinCapacidad()
    volumen
  }
  
  def notificarRecepcion(paquetes : Set[Paquete]) = paquetesEnEntrar = paquetesEnEntrar ++ paquetes
  
  def notificarEnvios(paquetes : Set[Paquete]) = paquetesEnSalir = paquetesEnSalir ++ paquetes
}

case class SucursalSinCapacidad() extends Exception