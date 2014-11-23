package tadp_grupo5

import java.util.Date

case class Envio(sucursalOrigen: Sucursal,
  sucursalDestino: Sucursal,
  paquetesEnviados: List[Paquete],
  distanciaRecorrida: Double,
  gananciaEnvio: Double,
  costoEnvioConAdicionales: Double,
  fecha: Date = new Date(),
  transporte : Transporte)