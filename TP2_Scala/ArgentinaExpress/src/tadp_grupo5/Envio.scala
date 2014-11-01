package tadp_grupo5

import java.util.Date

case class Envio 
(sucursalOrigen : Sucursal,
 sucursalDestino : Sucursal,
 transporte : Transporte,
 paquetesEnviados : Seq[Paquete],
 fecha : Date = new Date()) { 
  
  def deepCopy : Envio = {
    //TODO copia profunda porque los valores de los historicos no pueden cambiar
    this
  }
}