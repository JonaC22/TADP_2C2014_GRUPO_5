package tadp_grupo5

import scala.collection.mutable.Set
import java.util.Date
import scala.collection.mutable.Buffer
import scala.collection.mutable.Map

class Estadisticas {
  
  var sucursalesEnEstudio : Buffer[Sucursal] = Buffer()
  
  var restriccionesEnvio : Set[RestriccionEnvio] = Set()
  
  var restriccionesTransporte : Set[RestriccionTransporte] = Set()

  def agregarSucursal(sucursal : Sucursal) = sucursalesEnEstudio += sucursal
  
  def estadisticaGanancias : Map[Sucursal, Double] = {
    var mapa : Map[Sucursal, Double] = Map()
    sucursalesEnEstudio foreach (x => gananciaTotalDeTodosLosTransportes(x, mapa))
    mapa
  }

  def gananciaTotalDeTodosLosTransportes (sucursal : Sucursal, mapa : Map[Sucursal, Double]) {
	var ganancia : Double = obtenerEnvios(sucursal).filter(x => aplicarRestriccionesEnvios(x)).map(_.gananciaEnvio).sum
    if(mapa.contains(sucursal)) mapa(sucursal) += ganancia
    else mapa += (sucursal -> ganancia)
  }
  
  def obtenerEnvios (sucursal : Sucursal) : Buffer[Envio] = {
    sucursal.transportes flatMap(_.historialEnvios)
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
