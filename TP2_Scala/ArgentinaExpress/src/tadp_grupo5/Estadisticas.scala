package tadp_grupo5

import scala.collection.mutable.Set
import java.util.Date
import scala.collection.mutable.Buffer
import scala.collection.mutable.Map

class Estadisticas {
  
  var sucursalesEnEstudio : Buffer[Sucursal] = Buffer()
  
  var restriccionesEnvio : Set[RestriccionEnvio] = Set()
  
  var restriccionesTransporte : Set[RestriccionTransporte] = Set()
  
  var restriccionesPaquete : Set[RestriccionPaquete] = Set()

  def agregarSucursal(sucursal : Sucursal) = sucursalesEnEstudio += sucursal
  
  def estadisticasPromedioGanancias : Map[Sucursal, Double] = {
    var mapa : Map[Sucursal, Double] = Map()
    sucursalesEnEstudio foreach (x => gananciaPromedioViajes(x, mapa))
    mapa
  }
  
  def estadisticasPromedioCostos : Map[Sucursal, Double] = {
    var mapa : Map[Sucursal, Double] = Map()
    sucursalesEnEstudio foreach (x => costoPromedioViajes(x, mapa))
    mapa
  }
  
  def estadisticasPromedioTiempos : Map[Sucursal, Double] = {
    var mapa : Map[Sucursal, Double] = Map()
    sucursalesEnEstudio foreach (x => tiempoPromedioViajes(x, mapa))
    mapa
  }
  
  def estadisticasCantidadEnvios : Map[Sucursal, Double] = {
    var mapa : Map[Sucursal, Double] = Map()
    sucursalesEnEstudio foreach (x => cantidadEnvios(x, mapa))
    mapa
  }
  
  def estadisticasCantidadViajes : Map[Sucursal, Double] = {
    var mapa : Map[Sucursal, Double] = Map()
    sucursalesEnEstudio foreach (x => cantidadViajes(x, mapa))
    mapa
  }
  
  def estadisticasFacturacionTotal: Map[Sucursal, Double] = {
    var mapa : Map[Sucursal, Double] = Map()
    sucursalesEnEstudio foreach (x => facturacionTotal(x, mapa))
    mapa
  }

//  def gananciaTotalDeTodosLosTransportes (sucursal : Sucursal, mapa : Map[Sucursal, Double]) {
//	var ganancia : Double = obtenerEnvios(sucursal).filter(x => aplicarRestriccionesEnvios(x)).map(_.gananciaEnvio).sum
//    if(mapa.contains(sucursal)) mapa(sucursal) += ganancia
//    else mapa += (sucursal -> ganancia)
//  }
  
//  def obtenerEnvios (sucursal : Sucursal) : Buffer[Envio] = {
//    sucursal.transportes flatMap(_.historialEnvios)
//  }
  
  def obtenerEnviosPrima (sucursal : Sucursal) : Buffer[Envio] = {
    sucursal.transportes.filter(x => aplicarRestriccionesTransportes(x)).flatMap(_.historialEnvios).filter(x => aplicarRestriccionesEnvios(x))
  }
  
  def obtenerPaquetes (envios : Buffer[Envio]): Buffer[Paquete] = {
    envios.flatMap(_.paquetesEnviados).filter(x => aplicarRestriccionesPaquetes(x))
  }
  
  def obtenerTiempos(sucursal: Sucursal): Buffer[Double] = {
    sucursal.transportes.filter(x => aplicarRestriccionesTransportes(x)).map(x => x.historialEnvios.filter(x => aplicarRestriccionesEnvios(x))
        .map(_.distanciaRecorrida).sum / x.getVelocidad)
  }
  
  def aplicarRestriccionesEnvios(envio : Envio) : Boolean = {
    restriccionesEnvio.forall(_.aplicarRestriccion(envio))
  }
  
  def aplicarRestriccionesTransportes(transporte : Transporte) : Boolean = {
    restriccionesTransporte.forall(_.aplicarRestriccion(transporte))
  }
  
  def aplicarRestriccionesPaquetes(paquete : Paquete): Boolean = {
    restriccionesPaquete.forall(_.aplicarRestriccion(paquete))
  }
  
  def costoPromedioViajes (sucursal: Sucursal, mapa : Map[Sucursal, Double]) {
    var costoTotal: Double = obtenerEnviosPrima(sucursal).map(_.costoEnvioConAdicionales).sum
    var cantidadViajes: Double = obtenerEnviosPrima(sucursal).size
    var costoPromedio = costoTotal / cantidadViajes
    if(mapa.contains(sucursal)) mapa(sucursal) += costoPromedio
    else mapa += (sucursal -> costoPromedio)
  }
  
  def gananciaPromedioViajes(sucursal: Sucursal, mapa : Map[Sucursal, Double]) {
    var gananciaTotal: Double = obtenerEnviosPrima(sucursal).map(_.gananciaEnvio).sum
    var cantidadViajes: Double = obtenerEnviosPrima(sucursal).size
    var gananciaPromedio = gananciaTotal / cantidadViajes
    if(mapa.contains(sucursal)) mapa(sucursal) += gananciaPromedio
    else mapa += (sucursal -> gananciaPromedio)
  }
  
  def tiempoPromedioViajes(sucursal: Sucursal, mapa : Map[Sucursal, Double]) {
    var tiempoTotal: Double = obtenerTiempos(sucursal).sum
    var cantidadViajes: Double = obtenerEnviosPrima(sucursal).size
    var tiempoPromedio = tiempoTotal / cantidadViajes
    if(mapa.contains(sucursal)) mapa(sucursal) += tiempoPromedio
    else mapa += (sucursal -> tiempoPromedio)
  }
  
  def cantidadEnvios(sucursal: Sucursal, mapa : Map[Sucursal, Double]) {
   var cantidad: Double = obtenerPaquetes(obtenerEnviosPrima(sucursal)).size
   if(mapa.contains(sucursal)) mapa(sucursal) += cantidad
    else mapa += (sucursal -> cantidad)
  }
  
  def cantidadViajes(sucursal: Sucursal, mapa : Map[Sucursal, Double]) {
    var cantidad: Double = obtenerEnviosPrima(sucursal).size
    if(mapa.contains(sucursal)) mapa(sucursal) += cantidad
    else mapa += (sucursal -> cantidad)
  }
  
  def facturacionTotal(sucursal: Sucursal, mapa : Map[Sucursal, Double]){
    var gananciaTotal: Double = obtenerEnviosPrima(sucursal).map(_.gananciaEnvio).sum
    var costoTotal: Double = obtenerEnviosPrima(sucursal).map(_.costoEnvioConAdicionales).sum
    var facturacionTotal: Double = gananciaTotal + costoTotal
    if(mapa.contains(sucursal)) mapa(sucursal) += facturacionTotal
    else mapa += (sucursal -> facturacionTotal)
  }
  
  
  

}

trait RestriccionEnvio {
  def aplicarRestriccion(envio : Envio) : Boolean
}
trait RestriccionTransporte {
  def aplicarRestriccion(transporte : Transporte) : Boolean
}
trait RestriccionPaquete {
  def aplicarRestriccion(paquete : Paquete) : Boolean
}

class RestriccionPorTransporte() extends RestriccionTransporte{
  var tipoTransporte: String = ""
  
  def aplicarRestriccion(transporte : Transporte) : Boolean = {
    transporte.getClass.toString() == tipoTransporte
  }
  
}

class RestriccionPorTipo() extends RestriccionPaquete{
  var tipoPaquete: Caracteristica = Normal
  
  def aplicarRestriccion(paquete : Paquete) : Boolean = {
    paquete.caracteristica == tipoPaquete
  }
  
}

class RestriccionPorFecha() extends RestriccionEnvio{
  var fechaDesde : Date = new Date()
  var fechaHasta : Date = new Date()
  
  def aplicarRestriccion(envio : Envio) : Boolean = {
    (envio.fecha after fechaDesde) && (envio.fecha before fechaHasta)
  }
  
}  


