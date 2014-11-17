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
  
  val obtenerEnviosTransporte : Transporte => List[Envio] = {
    transporte =>
    for {
      envio <- transporte.historialEnvios if aplicarRestriccionesEnvios(envio)
    } yield envio
  }
  
  val obtenerTransportes : Sucursal => List[Transporte] = {
    sucursal =>
    for {
      transporte <- sucursal.transportes if aplicarRestriccionesTransportes(transporte)
    } yield transporte
  }
  
  val obtenerEnviosSucursal : Sucursal => List[Envio] = {
    sucursal =>
    for {
    	transporte <- obtenerTransportes(sucursal)
    	envio <- obtenerEnviosTransporte(transporte)
    } yield envio      
  }
  
  val obtenerPaquetes : Sucursal => List[Paquete] = {
    sucursal =>
    for {
      envio <- obtenerEnviosSucursal(sucursal)
      paquete <- envio.paquetesEnviados if aplicarRestriccionesPaquete(paquete)
    } yield paquete
  }
  
  val estadisticasCompania : (Sucursal => Double) => Compania => List[tuplaCompaniaSucursalValue] = {
     f => compania => for {
       sucursal <- compania.sucursales
     } yield (compania, estadisticasSucursal(f)(sucursal))
  }
  
  val estadisticasSucursal : (Sucursal => Double) => Sucursal => tuplaSucursalValue = {
	 f => sucursal => (sucursal, f(sucursal))
  }
  
  val estadisticasSucursales : (Sucursal => Double) => List[tuplaSucursalValue] = {
    f => 
    for {sucursal <- sucursalesEnEstudio} yield estadisticasSucursal(f)(sucursal)
  }
  
  val estadisticasPromedioSucursales : (Sucursal => Double) => List[tuplaSucursalValue] = {
    f => estadisticasSucursales(promedioViaje(f))
  }
  
  val facturacionTotalSucursal : Sucursal => Double = {
    { sucursal : Sucursal => 
      (for {
    	  envio <- obtenerEnviosSucursal(sucursal)
      	} yield envio.gananciaEnvio).sum
    }
  }
  
  val costosEnviosSucursal : Sucursal => Double = {
    sucursal => (for {
      envio <- obtenerEnviosSucursal(sucursal)
      } yield envio.costoEnvioConAdicionales).sum
  }
  
  val cantidadViajesSucursal : Sucursal => Double = obtenerEnviosSucursal(_).size
  
  val cantidadPaquetesEnviados : Sucursal => Double = obtenerPaquetes(_).size
  
  val tiempoTotalViajesSucursal : Sucursal => Double = {
    sucursal => (for {
      transporte <- obtenerTransportes(sucursal)
      envio <- obtenerEnviosTransporte(transporte)
      } yield envio.distanciaRecorrida/transporte.velocidad).sum
  }
  
  val promedioViaje : (Sucursal => Double) => Sucursal => Double = {
    funcionValorTotal => sucursal => 
      if(cantidadViajesSucursal(sucursal) != 0.0) funcionValorTotal(sucursal) / cantidadViajesSucursal(sucursal) else 0.0  
  }
  
  def agregarCompania(compania : Compania) = companiasEnEstudio = companiasEnEstudio :+ compania
  
  def sucursalesEnEstudio : List[Sucursal] = companiasEnEstudio.map(_.sucursales).flatten
  
  def estadisticasFacturacionTotalCompanias : List[tuplaCompaniaSucursalValue] = {
    (for {
      compania <- companiasEnEstudio 
    } yield estadisticasCompania(facturacionTotalSucursal)(compania)).flatten
  }
  
  def estadisticasFacturacionTotalTransportes() : Double = {
    estadisticasFacturacionTotalSucursales.foldLeft(0.0)(_+_._2)
  }
  
  def estadisticasFacturacionTotalSucursales : List[tuplaSucursalValue] = estadisticasSucursales(facturacionTotalSucursal)
  
  def estadisticasCantidadPaquetesEnviados : List[tuplaSucursalValue] = estadisticasSucursales(cantidadPaquetesEnviados)
  
  def estadisticasCantidadViajes : List[tuplaSucursalValue] = estadisticasSucursales(cantidadViajesSucursal)
  
  def estadisticasPromedioCostos : List[tuplaSucursalValue] = estadisticasPromedioSucursales(costosEnviosSucursal)
  
  def estadisticasPromedioGanancias : List[tuplaSucursalValue] = estadisticasPromedioSucursales(facturacionTotalSucursal)
  
  def estadisticasPromedioTiempos : List[tuplaSucursalValue] = estadisticasPromedioSucursales(tiempoTotalViajesSucursal)
  
  def obtenerPaquetes (envios : List[Envio]): List[Paquete] = {
    envios.flatMap(_.paquetesEnviados).filter(x => aplicarRestriccionesPaquete(x))
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
}

trait RestriccionPaquete {
  val aplicarRestriccion : Paquete => Boolean
}

trait RestriccionEnvio {
  val aplicarRestriccion : Envio => Boolean
}

trait RestriccionTransporte {
  def aplicarRestriccion(transporte : Transporte) : Boolean
}

case class RestriccionPorTipoTransporte(var tipoTransporte : String = "") extends RestriccionTransporte{
  def aplicarRestriccion(transporte : Transporte) : Boolean = transporte.tipoTransporte == tipoTransporte
}

case class RestriccionPorTipoPaquete(var tipoPaquete : Caracteristica) extends RestriccionPaquete{
  val aplicarRestriccion : Paquete => Boolean = _.caracteristica  == tipoPaquete
}

case class RestriccionPorFecha() extends RestriccionEnvio{
  var fechaDesde : Date = new Date()
  var fechaHasta : Date = new Date()
  val aplicarRestriccion : Envio => Boolean = envio => envio.fecha.after(fechaDesde) && envio.fecha.before(fechaHasta)
}  



