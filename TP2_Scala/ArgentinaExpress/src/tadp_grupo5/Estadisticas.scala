package tadp_grupo5

import scala.collection.mutable.Set

class Estadisticas {

  var transportesEnEstudio : Set[Transporte] = Set()

  def agregarTransporte(transporte: Transporte) = transportesEnEstudio += transporte

  def gananciaTotalDeTodosLosTransportes : Double = {
    transportesEnEstudio.map(_.historialEnvios.map(_.gananciaEnvio).sum).sum
  }

}