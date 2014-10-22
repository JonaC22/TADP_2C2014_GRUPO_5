package tadp_grupo5

class Sucursal (volumenDeposito : Int) {
  var paquetesEnSalir : Seq[Paquete] = Seq()
  var paquetesEnEntrar : Seq[Paquete] = Seq()
  
  def capacidad : Int = {
    volumenDeposito - paquetesEnEntrar.map(_.volumen).sum - paquetesEnSalir.map(_.volumen).sum  
  }
  
  def notificarRecepcion(paquetes : Seq[Paquete]) {
    validarCapacidad(paquetes)
    paquetesEnEntrar = paquetesEnEntrar ++ paquetes
  }
  
  def notificarEnvios(paquetes : Seq[Paquete]) {
    validarCapacidad(paquetes)
    paquetesEnSalir = paquetesEnSalir ++ paquetes
  } 
  
  def validarCapacidad(paquetes : Seq[Paquete]) : Unit = {
    if (capacidad < paquetes.map(_.volumen).sum) throw new SucursalSinCapacidad()
  }
}

case class SucursalSinCapacidad() extends Exception