package tadp_grupo5

import scala.collection.immutable.Set
import scala.collection.immutable.List
import java.util.Date

class Estadisticas {
  
  type tuplaSucursalValue = (Sucursal, Double)
  type tuplaCompaniaSucursalValue = (Compania, tuplaSucursalValue)
 
  var companiasEnEstudio : List[Compania] = List()
  var restriccionesEnvio : Set[RestriccionEnvio] = Set()
  var restriccionesTransporte : Set[RestriccionTransporte] = Set()
  var restriccionesPaquete : Set[RestriccionPaquete] = Set()
  
  def sucursalesEnEstudio : List[Sucursal] = {
    companiasEnEstudio.map(_.sucursales).flatten
  }
  
  val estadisticasCompania : (Sucursal => Double) => Compania => List[tuplaCompaniaSucursalValue] = {
     f => compania => for {
       sucursal <- compania.sucursales
     } yield (compania, estadisticasSucursal(f)(sucursal))
  }
  
   val estadisticasSucursal : (Sucursal => Double) => Sucursal => tuplaSucursalValue = {
	 f => sucursal => (sucursal, f(sucursal))
  }
  
  val facturacionTotalSucursal : Sucursal => Double = {
    { sucursal : Sucursal => 
      (for {
    	  envio <- obtenerEnviosSucursal(sucursal)
      	} yield envio.gananciaEnvio).sum
    }
  }
  
  val estadisticasFacturacionTotalSucursal : Sucursal => tuplaSucursalValue = {
	sucursal => estadisticasSucursal(facturacionTotalSucursal)(sucursal)
  }
  
  val estadisticasFacturacionTotalCompania : Compania => List[tuplaCompaniaSucursalValue] = {
    estadisticasCompania(facturacionTotalSucursal)
  }
  
  def estadisticasFacturacionTotalCompanias : List[tuplaCompaniaSucursalValue] = {
    (for {
      compania <- companiasEnEstudio 
    } yield estadisticasCompania(facturacionTotalSucursal)(compania)).flatten
  }

  def agregarCompania(compania : Compania) = companiasEnEstudio = companiasEnEstudio :+ compania
  
//  def estadisticasFacturacionTotalTransportes() : Double = {
//    estadisticasFacturacionTotalSucursal.foldLeft(0.0)(_+_._2)
//  }
  
//  def estadisticasPromedioGanancias : tuplaSucursalValue = {
//    sucursalesEnEstudio foreach (x => gananciaPromedioViajes(x))
//  }
//  
//  def estadisticasPromedioCostos : tuplaSucursalValue = {
//    sucursalesEnEstudio foreach (x => costoPromedioViajes(x))
//  }
//  
//  def estadisticasPromedioTiempos : tuplaSucursalValue = {
//    sucursalesEnEstudio foreach (x => tiempoPromedioViajes(x))
//  }
//  
//  def estadisticasCantidadPaquetesEnviados : tuplaSucursalValue = {
//    sucursalesEnEstudio foreach (x => cantidadPaquetesEnviados(x))
//  }
//  
//  def estadisticasCantidadViajes : tuplaSucursalValue = {
//    sucursalesEnEstudio foreach (x => cantidadViajes(x))
//  }
  
  def estadisticasFacturacionTotalSucursales : List[tuplaSucursalValue] = {
    for {sucursal <- sucursalesEnEstudio} yield estadisticasFacturacionTotalSucursal(sucursal)
  }

  def obtenerEnviosTransporte (transporte : Transporte) : List[Envio] = {
    for {
      envio <- transporte.historialEnvios if aplicarRestriccionesEnvios(envio)
    } yield envio
  }
  
  val obtenerEnviosSucursal : Sucursal => List[Envio] = {
    { sucursal =>
    	for {
    		transporte <- obtenerTransportes(sucursal)
    		envio <- obtenerEnviosTransporte(transporte)
    	} yield envio      
    }
  }
  
  def obtenerTransportes (sucursal : Sucursal) : List[Transporte] = {
    for {
      transporte <- sucursal.transportes if aplicarRestriccionesTransportes(transporte)
    } yield transporte
  }
  
  def obtenerPaquetes (envios : List[Envio]): List[Paquete] = {
    envios.flatMap(_.paquetesEnviados).filter(x => aplicarRestriccionesPaquete(x))
  }
  
  def obtenerTiempos(sucursal: Sucursal): List[Double] = {
    obtenerTransportes(sucursal).map(x => obtenerEnviosTransporte(x)
        .map(_.distanciaRecorrida).sum / x.velocidad)
  }
  
  def aplicarRestriccionesEnvios(envio : Envio) : Boolean = {
    restriccionesEnvio.forall(_.aplicarRestriccion(envio))
  }
  
  def aplicarRestriccionesTransportes(transporte : Transporte) : Boolean = {
    restriccionesTransporte.forall(_.aplicarRestriccion(transporte))
  }
  
  def aplicarRestriccionesPaquete(paquete : Paquete) : Boolean = {
    restriccionesPaquete.forall(_.aplicarRestriccion(paquete))
  }
  
  val costoPromedioViajes : Sucursal => tuplaSucursalValue = {
    { sucursal =>
    	var costoTotal: Double = obtenerEnviosSucursal(sucursal).map(_.costoEnvioConAdicionales).sum
    	var cantidadViajes: Double = obtenerEnviosSucursal(sucursal).size
    	var costoPromedio = if(cantidadViajes != 0) costoTotal / cantidadViajes else 0
    	(sucursal, costoPromedio)    
    }
  }
  
//  def gananciaPromedioViajes(tupla : tuplaSucursalValue) {
//    var gananciaTotal: Double = obtenerEnviosSucursal(sucursal).map(_.gananciaEnvio).sum
//    var cantidadViajes: Double = obtenerEnviosSucursal(sucursal).size
//    var gananciaPromedio = if(cantidadViajes != 0) gananciaTotal / cantidadViajes else 0
//    if(mapa.contains(sucursal)) mapa(sucursal) += gananciaPromedio
//    else mapa += (sucursal -> gananciaPromedio)
//  }
//  
//  def tiempoPromedioViajes(tupla : tuplaSucursalValue) {
//    var tiempoTotal: Double = obtenerTiempos(sucursal).sum
//    var cantidadViajes: Double = obtenerEnviosSucursal(sucursal).size
//    var tiempoPromedio = if(cantidadViajes != 0) tiempoTotal / cantidadViajes else 0
//    if(mapa.contains(sucursal)) mapa(sucursal) += tiempoPromedio
//    else mapa += (sucursal -> tiempoPromedio)
//  }
//  
//  def cantidadPaquetesEnviados(tupla : tuplaSucursalValue) {
//   var cantidad: Double = obtenerPaquetes(obtenerEnviosSucursal(sucursal)).size
//   if(mapa.contains(sucursal)) mapa(sucursal) += cantidad
//    else mapa += (sucursal -> cantidad)
//  }
//  
//  def cantidadViajes(tupla : tuplaSucursalValue) {
//    var cantidad: Double = obtenerEnviosSucursal(sucursal).size
//    if(mapa.contains(sucursal)) mapa(sucursal) += cantidad
//    else mapa += (sucursal -> cantidad)
//  }

}

trait RestriccionPaquete {
  def aplicarRestriccion(paquete : Paquete) : Boolean
}

trait RestriccionEnvio {
  def aplicarRestriccion(envio : Envio) : Boolean
}

trait RestriccionTransporte {
  def aplicarRestriccion(transporte : Transporte) : Boolean
}

class RestriccionPorTipoTransporte(var tipoTransporte : String = "") extends RestriccionTransporte{
  
  def aplicarRestriccion(transporte : Transporte) : Boolean = {
    transporte.tipoTransporte == tipoTransporte
  }
}

class RestriccionPorTipoPaquete(var tipoPaquete : Caracteristica) extends RestriccionPaquete{
  
  def aplicarRestriccion(paquete : Paquete) : Boolean = {
    paquete.caracteristica  == tipoPaquete
  }
}

class RestriccionPorFecha() extends RestriccionEnvio{
  var fechaDesde : Date = new Date()
  var fechaHasta : Date = new Date()
  
  def aplicarRestriccion(envio : Envio) : Boolean = {
    (envio.fecha after fechaDesde) && (envio.fecha before fechaHasta)
  }
}  



