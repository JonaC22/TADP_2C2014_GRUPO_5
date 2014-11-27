package tadp_grupo5

import java.util.Date

case class Envio(sucursalOrigen: Sucursal, sucursalDestino: Sucursal, paquetes: List[Paquete], transporte : Transporte, fecha: Date = new Date){
  
  def costosPaquetes: List[Double] = for{ paquete <- paquetes } yield paquete.caracteristica.costo
  def paquetesRefrigeracion: List[Paquete] = for{ paquete <- paquetes if paquete.caracteristica == NecesitaRefrigeracion} yield paquete
  def paquetesUrgentes: List[Paquete] = for{ paquete <- paquetes if paquete.caracteristica == Urgente} yield paquete
  
  //costo base paquetes
  def costoBase: Double = {
    transporte match {
      case Camion(_,_,_,_,_) | Furgoneta(_,_,_,_,_) => costosPaquetes.sum + paquetesRefrigeracion.size * 5
      case _ => costosPaquetes.sum
    }
  }
  //costo sin adicionales
  def costo: Double = costoBase + transporte.costo
  
  //costo con adicionales
  def costoConAdicionales: Double = {
    transporte match {
      case Camion(_,_,_,_,_) => if(sucursalDestino.esCasaCentral && fecha.getDate() > 21) costo * 1.2 else costo
      case Avion(_,_,_,_,_) => {
        if(sucursalDestino.esCasaCentral && fecha.getDate() > 21 && sucursalDestino.pais != sucursalOrigen.pais) (costo * 1.1)* 0.80 //10% impuestos y 20% descuento
        else if (sucursalDestino.esCasaCentral && fecha.getDate() > 21) costo * 0.80 //20% de descuento
        else costo * 1.1 //10% impuesto
      }
      case _ => transporte.costo
    }
  }
  
  def precio: Double = (for{ paquete <- paquetes } yield paquete.caracteristica.precio).sum
  
  def ganancia: Double = precio - costo
  
  def distanciaRecorrida : Double = transporte.distanciaEntre(sucursalOrigen, sucursalDestino)
  
}