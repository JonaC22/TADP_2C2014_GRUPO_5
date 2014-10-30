package tadp_grupo5

class Sucursal (volumenDeposito : Int, val pais : String) {
  var paquetesEnSalir : Seq[Paquete] = Seq()
  var paquetesEnEntrar : Seq[Paquete] = Seq()
  
  def capacidad : Int = volumenDeposito - paquetesEnEntrar.map(_.volumen).sum - paquetesEnSalir.map(_.volumen).sum  
  
  def esCasaCentral(transporte : Transporte) : Double = 0.0
  
  def notificarRecepcion(paquetes : Seq[Paquete]) {
    validarCapacidad(paquetes)
    paquetesEnEntrar = paquetesEnEntrar ++ paquetes
  }
  
  def notificarEnvios(paquetes : Seq[Paquete]) {
    validarCapacidad(paquetes)
    paquetesEnSalir = paquetesEnSalir ++ paquetes
  } 
  
  def validarCapacidad(paquetes : Seq[Paquete]) =  if (capacidad < paquetes.map(_.volumen).sum) throw new SucursalSinCapacidad()
}

case class CasaCentral(volumenDeposito : Int, override val pais : String) extends Sucursal(volumenDeposito, pais){
  override def esCasaCentral(transporte : Transporte) : Double = transporte.costoAdicionalCamionCasaCentral
}

case class SucursalSinCapacidad() extends Exception