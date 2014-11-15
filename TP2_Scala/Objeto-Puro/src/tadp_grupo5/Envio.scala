package tadp_grupo5

import java.util.Date
import scala.collection.mutable.Buffer

case class Envio(sucursalOrigen: Sucursal,
  sucursalDestino: Sucursal,
  paquetesEnviados: Buffer[Paquete],
  distanciaRecorrida: Double,
  gananciaEnvio: Double,
  costoEnvioConAdicionales: Double,
  fecha: Date = new Date())