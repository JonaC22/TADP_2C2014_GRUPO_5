package tadp_grupo5

import scala.collection.mutable.Set
import java.util.Date
import scala.collection.mutable.Buffer

class Estadisticas {

  var transportesEnEstudio : Buffer[Transporte] = Buffer()
  
  var restriccionesEnvio : Set[RestriccionEnvio] = Set()
  
  var restriccionesTransporte : Set[RestriccionTransporte] = Set()

  def agregarTransporte(transporte: Transporte) = transportesEnEstudio += transporte

  def gananciaTotalDeTodosLosTransportes : Double = {
	obtenerEnvios.filter(x => aplicarRestriccionesEnvios(x))
    .map(_.gananciaEnvio).sum
  }
  
  def obtenerEnvios : Buffer[Envio] = {
    transportesEnEstudio flatMap(_.historialEnvios)
  }
  
  def aplicarRestriccionesEnvios(envio : Envio) : Boolean = {
    restriccionesEnvio.forall(_.aplicarRestriccion(envio))
  }

}

trait RestriccionEnvio {
  def aplicarRestriccion(envio : Envio) : Boolean
}
trait RestriccionTransporte {
  def aplicarRestriccion(transporte : Transporte) : Boolean
}

class RestriccionPorFecha() extends RestriccionEnvio{
  var fechaDesde : Date = new Date()
  var fechaHasta : Date = new Date()
  
  def aplicarRestriccion(envio : Envio) : Boolean = {
    (envio.fecha after fechaDesde) && (envio.fecha before fechaHasta)
  }
  
}
