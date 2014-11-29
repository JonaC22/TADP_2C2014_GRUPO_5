package tadp_grupo5

import scala.collection.immutable.Set
import scala.collection.immutable.List
import java.util.Date

case class Estadisticas() {
  
  type tuplaSucursalValue = (Sucursal, Double)
  type tuplaCompaniaSucursalValue = (Compania, tuplaSucursalValue)
  type funcionCalculo = List[Envio] => Double
 
  var companiasEnEstudio : List[Compania] = List()
  var restriccionesEnvio : Set[RestriccionEnvio] = Set()
  var restriccionesPaquete : Set[RestriccionPaquete] = Set()
  
  val obtenerEnviosSucursal : Sucursal => List[Envio] = {
    sucursal =>
    for {
    	envio <- sucursal.enviosRealizados if aplicarRestriccionesEnvios(envio)
    } yield envio      
  }
  
  val obtenerPaquetes : Sucursal => List[Paquete] = {
    sucursal =>
    for {
      envio <- obtenerEnviosSucursal(sucursal)
      paquete <- envio.paquetes if aplicarRestriccionesPaquete(paquete)
    } yield paquete
  }
  
  val estadisticasCompania : funcionCalculo => Compania => List[tuplaCompaniaSucursalValue] = {
     f => compania => for {
       sucursal <- compania.sucursales
     } yield (compania, estadisticasSucursal(f)(sucursal))
  }
  
  val estadisticasSucursal : funcionCalculo => Sucursal => tuplaSucursalValue = {
	 f => sucursal => (sucursal, f(obtenerEnviosSucursal(sucursal)))
  }
  
  val estadisticasPromedioSucursal : funcionCalculo => Sucursal => tuplaSucursalValue = {
	 f => sucursal => (sucursal, promedioViaje(f)(sucursal))
  }
  
  val estadisticasSucursales : funcionCalculo => List[tuplaSucursalValue] = {
    f => 
    for {sucursal <- sucursalesEnEstudio} yield estadisticasSucursal(f)(sucursal)
  }
  
  val estadisticasPromedioSucursales : funcionCalculo => List[tuplaSucursalValue] = {
    f => 
    for {sucursal <- sucursalesEnEstudio} yield estadisticasPromedioSucursal(f)(sucursal)
  }
  
  val facturacionTotalSucursal : funcionCalculo = {
    envios =>
      (for {
    	  envio <- envios
      	} yield envio.ganancia).sum
  }
  
  val costosEnviosSucursal : funcionCalculo = {
    envios => (for {
      envio <- envios
      } yield envio.costoConAdicionales).sum
  }
  
  val cantidadViajesSucursal : funcionCalculo = _.size
  
  val cantidadPaquetesEnviados : funcionCalculo = obtenerPaquetes(_).size
  
  val tiempoTotalViajesSucursal : funcionCalculo = {
    envios => (for {
      envio <- envios
      } yield envio.distanciaRecorrida/envio.velocidad).sum
  }
  
  val promedioViaje : funcionCalculo => Sucursal => Double = {
    funcionValorTotal => sucursal => 
      if(cantidadViajesSucursal(obtenerEnviosSucursal(sucursal)) != 0.0) funcionValorTotal(obtenerEnviosSucursal(sucursal)) / cantidadViajesSucursal(obtenerEnviosSucursal(sucursal)) else 0.0  
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
    envios.flatMap(_.paquetes).filter(x => aplicarRestriccionesPaquete(x))
  }
  
  def aplicarRestriccionesEnvios(envio : Envio) : Boolean = {
    restriccionesEnvio.forall(_.aplicarRestriccion(envio))
  }
  
  def aplicarRestriccionesPaquete(paquete : Paquete) : Boolean = {
    restriccionesPaquete.forall(_.aplicarRestriccion(paquete))
  }
}

trait RestriccionPaquete {
  def aplicarRestriccion(paquete : Paquete) : Boolean
}

trait RestriccionEnvio {
  def aplicarRestriccion(envio : Envio) : Boolean
}

case class RestriccionPorCamion() extends RestriccionEnvio{
  def aplicarRestriccion(envio : Envio) : Boolean = {
    envio.tipoTransporte match {
    case _ : Camion => true
    case _ => false
    }
  }
}

case class RestriccionPorFurgoneta() extends RestriccionEnvio{
  def aplicarRestriccion(envio : Envio) : Boolean = {
    envio.tipoTransporte match {
    case _ : Furgoneta => true
    case _ => false
    }
  }
}

case class RestriccionPorAvion() extends RestriccionEnvio{
  def aplicarRestriccion(envio : Envio) : Boolean = {
    envio.tipoTransporte match {
    case _ : Avion => true
    case _ => false
    }
  }
}

case class RestriccionPorTipoPaquete(var tipoPaquete : Caracteristica) extends RestriccionPaquete{
  def aplicarRestriccion(paquete : Paquete) : Boolean = paquete.caracteristica  == tipoPaquete
}

case class RestriccionPorFecha() extends RestriccionEnvio{
  var fechaDesde : Date = new Date()
  var fechaHasta : Date = new Date()
  def aplicarRestriccion(envio : Envio) : Boolean = envio.fecha.after(fechaDesde) && envio.fecha.before(fechaHasta)
}  



