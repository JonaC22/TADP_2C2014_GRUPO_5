package tadp_grupo5

import java.util.Date

case class Envio(transporte : Transporte, distanciaRecorrida : Double, fecha : Date = new Date()){
  
  def costosPaquetes: List[Double] = for{ paquete <- paquetes } yield paquete.costo
  def paquetesRefrigeracion: List[Paquete] = for{ paquete <- paquetes if paquete.caracteristica == NecesitaRefrigeracion} yield paquete
  def paquetesUrgentes: List[Paquete] = for{ paquete <- paquetes if paquete.caracteristica == Urgente} yield paquete
  def paquetes = transporte.pedidos 
  def sucursalDestino = transporte.sucursalDestino
  def sucursalOrigen = transporte.sucursalOrigen
  def velocidad = transporte.velocidad
  def tipoTransporte : TipoTransporte = transporte.tipoTransporte 
  
  //costo base paquetes
  def costoBase: Double = {
    transporte.tipoTransporte  match {
      case _ : Camion | _ : Furgoneta => costosPaquetes.sum + paquetesRefrigeracion.size * 5
      case _ => costosPaquetes.sum
    }
  }
  //costo sin adicionales
  def costo: Double = costoBase + transporte.costo
  
  //costo con adicionales
  def costoConAdicionales: Double = {
    transporte.tipoTransporte  match {
      case _ : Camion => if(sucursalDestino.esCasaCentral && fecha.getDate() > 21) costo * 1.02 else costo
      case _ : Avion => {
        if(sucursalDestino.esCasaCentral && fecha.getDate() > 21 && sucursalDestino.pais != sucursalOrigen.pais) (costo * 1.1)* 0.80 //10% impuestos y 20% descuento
        else if (sucursalDestino.esCasaCentral && fecha.getDate() > 21) costo * 0.80 //20% de descuento
        else if(sucursalDestino.pais != sucursalOrigen.pais) costo * 1.1 //10% impuesto
        else costo
      }
      case _ => costo
    }
  }
  
  def precio: Double = (for{ paquete <- transporte.pedidos } yield paquete.precio).sum
  
  def ganancia: Double = precio - costo
  
}