package tadp_grupo5

import org.scalatest._
import scala.collection.mutable.Queue
import scala.collection.mutable.Buffer
import scala.collection.mutable.Set
import java.util.Date

class TestsArgentinaExpress extends FlatSpec with BeforeAndAfter with Matchers{
  
	object SistemaExterno extends CalculadorDistancia {
		var distanciaTerrestre : Double = 0.0
		var distanciaAerea : Double = 0.0
		var cantidadPeajes : Int = 0
		var fechaActual : Date = new Date()

		override def distanciaTerrestreEntre(sucursal10: Sucursal, sucursal20: Sucursal): Double = {
			distanciaTerrestre
		}

		override def distanciaAereaEntre(sucursal10: Sucursal, sucursal20: Sucursal): Double = {
			distanciaAerea
		}

		override def cantidadPeajesEntre(sucursal10: Sucursal, sucursal20: Sucursal): Int = {
			cantidadPeajes
		}
	}

	val sucursal10 = new Sucursal(10, "Argentina")
	val sucursal20 = new Sucursal(20, "Argentina")
    val sucursal30 = new Sucursal(30, "Uruguay")
	val sucursal1000 = new Sucursal(1000, "Brasil")
    val sucursal3000 = new Sucursal(3000, "Argentina")
	val casaCentral = new CasaCentral(20, "Brasil")
	val sucursales = Buffer(sucursal10, sucursal20, sucursal30, sucursal1000, sucursal3000, casaCentral)
	
	val cliente = new Cliente(sucursal1000, sucursal3000)
	  
	val estadisticas = new Estadisticas()
	
	val camion = new Camion(SistemaExterno)
	val avion = new Avion(SistemaExterno)
	val furgoneta = new Furgoneta(SistemaExterno)
	
	val transportes = Buffer(camion, avion, furgoneta)
	
	val paquete1 = new Paquete(sucursal10, sucursal20,1, Normal)
	val paqueteInvertido1 = new Paquete(sucursal20, sucursal10,1, Normal)
	val paquete2 = new Paquete(sucursal10, sucursal20,2, Normal)
	val paquete5 =new Paquete(sucursal30, sucursal20,5, Normal)
	val paquete10 = new Paquete(sucursal10, sucursal20,10, Normal)
	val paquete10CasaCentral = new Paquete(sucursal10, casaCentral,10, Normal)
	val paquete20 = new Paquete(sucursal10, sucursal20,20, Normal)
	val paqueteConMuchoVolumen = new Paquete(sucursal10, sucursal20, 9999, Normal)
	val paqueteUrgenteLiviano = new Paquete(sucursal10, sucursal20,1, Urgente) 
	val paqueteUrgentePesado = new Paquete(sucursal10, sucursal20,20, Urgente) 
	val paqueteConRefrigeracion = new Paquete(sucursal10, sucursal20,10, NecesitaRefrigeracion)
	val paqueteFragil = new Paquete(sucursal10, sucursal20,2, Fragil)
	
	after{
	  cliente.paquete = null
	  cliente.sucursalOrigen = sucursal1000
	  cliente.sucursalDestino = sucursal3000
	  transportes.foreach(_.pedidos = Buffer())
	  transportes.foreach(_.servicioExtra = None)
	  transportes.foreach(_.infraestructura = None)
	  transportes.foreach(_.tipoDePaquetesValidos = Buffer(Normal))
	  transportes.foreach(_.historialEnvios  = Queue())
	  sucursales.foreach(_.paquetesEnEntrar = Buffer())
	  sucursales.foreach(_.paquetesEnSalir = Buffer())
	  sucursales.foreach(_.transportes = Buffer())
	  SistemaExterno.distanciaTerrestre  = 0.0
	  SistemaExterno.distanciaAerea  = 0.0
	  SistemaExterno.cantidadPeajes  = 0
	  SistemaExterno.fechaActual.setDate(1)
	  estadisticas.sucursalesEnEstudio = Buffer()
	  estadisticas.restriccionesEnvio = Set()
	  estadisticas.restriccionesPaquete = Set()
	  estadisticas.restriccionesTransporte  = Set()
	}
    
	"Una sucursal" should "tener capacidad" in {
		sucursal10.notificarPaqueteASalir(paquete1)
		assert(sucursal10.capacidad == 9) //10 - 1 = 9
	}				

	it should " poder agregarse mas paquetes" in {
		sucursal10.notificarPaqueteASalir(paquete5)
		assert(sucursal10.capacidad == 5)
		sucursal10.notificarPaqueteAEntrar(paquete5)
		assert(sucursal10.capacidad == 0)
	}
	
	it should "no poder agregarse mas paquetes" in {
	   sucursal30.notificarPaqueteAEntrar(paquete1)
	   intercept[SucursalSinCapacidad]{
		 sucursal30.notificarPaqueteAEntrar(paqueteConMuchoVolumen)
	   }
	}
	
	"Mock de Sistema Externo" should "responder a consultas" in {

	  SistemaExterno.distanciaTerrestre  = 250.5
	  SistemaExterno.distanciaAerea = 1200.5
	  SistemaExterno.cantidadPeajes = 4
	  
	  assert(SistemaExterno.distanciaTerrestreEntre(sucursal10, sucursal20) == 250.5 )
	  assert(SistemaExterno.distanciaAereaEntre(sucursal10, sucursal20) == 1200.5)
	  assert(SistemaExterno.cantidadPeajesEntre(sucursal10, sucursal20) == 4)
	}
	
	"Un transporte" should "tener capacidad" in {
		camion.asignarPaquete(paquete10)
		camion.asignarPaquete(paquete20)
		assert(camion.pedidos.size == 2)
		assert(camion.capacidad == 15) //45 - 10 - 20 = 15

		camion.asignarPaquete(paquete5)
		camion.asignarPaquete(paquete10)
		assert(camion.pedidos.size == 4)
		assert(camion.capacidad == 0) //45 - 10 - 20 - 5 - 10 = 0
	}
	
	it should "no tener capacidad" in {
		camion.asignarPaquete(paquete10)
		camion.asignarPaquete(paquete20)
		assert(camion.pedidos.size == 2)
		assert( camion.capacidad == 15) //45 - 10 - 20 = 15

		intercept[TransporteSinCapacidad]{
		  camion.asignarPaquete(paqueteConMuchoVolumen) //excedo la capacidad
		}
		
		assert(camion.capacidad == 15)
		assert(camion.pedidos.size == 2)
	}
	
	it should "no llevar paquetes de destinos diferentes" in {

		camion.asignarPaquete(paquete1)
		
		intercept[PaquetesDestinoErroneo] {
			camion.asignarPaquete(paqueteInvertido1) //asigno paquetes iniciales con destino distinto
	    }
	   
		assert(camion.pedidos.size == 1)

		camion.asignarPaquete(paquete2)
		assert(camion.pedidos.size == 2)
		assert(camion.capacidad == 42) //45 - 1 - 2 = 42
	}
	
	it should "llevar tipos de paquetes especificados" in {
	  intercept[PaqueteTipoInvalido]{
	    furgoneta.asignarPaquete(paqueteUrgenteLiviano) //no puede llevar paquetes fragiles ni urgentes
	  }
	  furgoneta.tipoDePaquetesValidos = Buffer(Normal, Urgente)
	  intercept[PaqueteTipoInvalido]{
	    furgoneta.asignarPaquete(paqueteFragil) //aun no puede enviar el paquete fragil
	  }
	  furgoneta.tipoDePaquetesValidos = Buffer(Normal, Urgente, Fragil)
	  furgoneta.asignarPaquete(paqueteUrgenteLiviano)
	  furgoneta.asignarPaquete(paqueteFragil)
	  assert(furgoneta.pedidos.size == 2)
	}
	
	it should "calcular costo del envio" in {

	  camion.asignarPaquete(paquete10)
	  
	  assert(camion.costoEnvio == 10)
	}
	
	it should "calcular costo con adicionales" in {

	  camion.asignarPaquete(paquete1)
	  
	  assert(camion.costoEnvioConAdicionales === 10.22 +- 0.01)
	}
	
	it should "calcular la ganancia de un envio" in {

	  camion.asignarPaquete(paquete10)
	  camion.asignarPaquete(paquete20)

	  SistemaExterno.distanciaTerrestre = 0.5
	  SistemaExterno.cantidadPeajes  = 2
	  
	  assert(camion.gananciaEnvio == 66)
	}
	
	it should "calcular la ganancia de un envio con distintos seguimientos" in {

	  camion.asignarPaquete(paquete10)
	  camion.asignarPaquete(paquete20)
	  camion.servicioExtra = Some(SeguimientoSatelital)
	  SistemaExterno.distanciaTerrestre = 0.5
	  SistemaExterno.cantidadPeajes  = 2
	  
	  assert(camion.gananciaEnvio == 65.5)//160 - (20 + 100*0.5 + 2*12 + 0.5)

	  camion.servicioExtra = Some(SeguimientoSatelitalConVideo)
	  
	  assert(camion.gananciaEnvio === 62.25 +- 0.01)
	}
	
	it should "calcular el costo de un envio dependiendo la infraestructura" in {
	  camion.asignarPaquete(paquete10)
	  camion.asignarPaquete(paquete20)
	  camion.infraestructura = Some(SustanciasPeligrosas)
	  SistemaExterno.distanciaTerrestre = 0.5
	  SistemaExterno.cantidadPeajes  = 2
	  
	  assert(camion.costoEnvioConAdicionales == 694)//20 + 100*0.5 + 2*12 + 600
	
	  camion.infraestructura = Some(Animales)
	  //distancia menor a 100km
	  SistemaExterno.distanciaTerrestre = 50
	  assert(camion.costoEnvioConAdicionales == 5094)//20 + 100*50 + 2*12 + 50
	  
	  //distancia menor a 200km
	  SistemaExterno.distanciaTerrestre = 130
	  assert(camion.costoEnvioConAdicionales == 13130)//20 + 100*130 + 2*12 + 86
	  
	  //distancia mayor a 200km
	  SistemaExterno.distanciaTerrestre = 240
	  assert(camion.costoEnvioConAdicionales == 24181)//20 + 100*240 + 2*12 + 137
	}
	
	"Un camion" should "poder llevar paquetes con refrigeracion" in {
	  camion.asignarPaquete(paqueteConRefrigeracion)
	  assert(camion.pedidos.size == 1)
	}
	
	it should "calcular el costo de un envio con sustancias peligrosas y paquetes urgentes" in {
	  camion.tipoDePaquetesValidos = Buffer(Normal, Urgente)
	  camion.asignarPaquete(paqueteUrgentePesado)
	  camion.asignarPaquete(paqueteUrgentePesado)
	  camion.infraestructura = Some(SustanciasPeligrosas)
	  
	  assert(camion.capacidad == 5)
	  assert(camion.costoEnvio == 40)
	  assert(camion.costoSustanciasUrgentes === 2.66 +- 0.01)
	  assert(camion.costoInfraestructura == 600)
	  assert(camion.costosAdicionales === 602.66 +- 0.01)
	  assert(camion.costoEnvioConAdicionales === 642.66 +- 0.01)
	}
	
	it should "calcular costo de ultima semana del mes yendo a casa central" in {
	  camion.asignarPaquete(paquete10CasaCentral)
	  SistemaExterno.fechaActual.setDate(24)
	  
	  assert(camion.costoEnvio == 10)
	  assert(camion.sucursalDestino.esCasaCentral)
	  assert(camion.costoAdicionalCasaCentral == 0.2)
	  assert(camion.costoEnvioConAdicionales == 10.2)
	  
	}
	
	it should "calcular costo con adicional por volumen no aceptable" in {

	  camion.asignarPaquete(paquete1)
	  
	  assert(camion.costoEnvio == 10) 
	  assert(camion.volumen - camion.capacidad == 1)
	  assert(camion.volumen == 45)
	  assert(camion.costoVolumen === 0.22 +- 0.01)
	}
	
	"Un avion" should "no poder hacer viajes menor o igual a 1000 kilometros" in {
	  avion.asignarPaquete(paquete1)
	  SistemaExterno.distanciaAerea = 900.0
	  
	  intercept[EnvioConDistanciaMenorA1000KM]{
	    avion.gananciaEnvio
	  }
	  
	  SistemaExterno.distanciaAerea = 1000.0
	  
	  intercept[EnvioConDistanciaMenorA1000KM]{
	    avion.gananciaEnvio
	  }	  
	  
	  SistemaExterno.distanciaAerea = 1100.0
	  
	  assert(avion.gananciaEnvio == -1649950.0)
	}
	
	it should "no poder llevar paquetes con refrigeracion" in {
	  intercept[PaqueteTipoInvalido]{
	    avion.asignarPaquete(paqueteConRefrigeracion)
	  }
	}
	
	"Las estadisticas" should "mostrar costo promedio de las sucursales en analisis" in {
	  estadisticas agregarSucursal(sucursal1000)
	  estadisticas agregarSucursal(sucursal3000)
	  sucursal1000.transportes += camion
	  cliente.generarPaquete(10, Normal)
	  cliente.pedirEnvio
	  
	  camion.hacerEnvio
	  
	  assert(estadisticas.estadisticasPromedioCostos.get(sucursal1000).contains(10))
	  camion.tipoDePaquetesValidos = Buffer(Normal, Urgente)
	  cliente.generarPaquete(30, Urgente)
	  cliente.pedirEnvio
	  
	  camion.hacerEnvio
	  assert(estadisticas.estadisticasPromedioCostos.get(sucursal1000).contains(15)) // (10+20)/2
	  
	  furgoneta.tipoDePaquetesValidos = Buffer(Normal, Urgente)
	  cliente.sucursalOrigen = sucursal3000
	  sucursal3000.transportes += furgoneta
	  cliente.sucursalDestino = sucursal1000
	  cliente.generarPaquete(2, Normal)
	  cliente.pedirEnvio
	  cliente.generarPaquete(3, Urgente)
	  cliente.pedirEnvio
	  
	  furgoneta.hacerEnvio
	  
	  cliente.generarPaquete(4, Normal)
	  cliente.pedirEnvio
	  
	  furgoneta.hacerEnvio
	  
	  assert(estadisticas.estadisticasPromedioCostos.get(sucursal1000).contains(15)) // (10+20)/2
	  assert(estadisticas.estadisticasPromedioCostos.get(sucursal3000).contains(20)) // (30+10)/2
	}
	
	it should "mostrar ganancia promedio de todas las sucursales en analisis" in {
	  estadisticas agregarSucursal(sucursal1000)
	  estadisticas agregarSucursal(sucursal3000)
	  sucursal1000.transportes += camion
	  cliente.generarPaquete(10, Normal)
	  cliente.pedirEnvio
	  
	  camion.hacerEnvio
	 
	  assert(estadisticas.estadisticasPromedioGanancias.get(sucursal1000).contains(70))
	  camion.tipoDePaquetesValidos = Buffer(Normal, Urgente)
	  cliente.generarPaquete(30, Urgente)
	  cliente.pedirEnvio
	  
	  camion.hacerEnvio
	  assert(estadisticas.estadisticasPromedioGanancias.get(sucursal1000).contains(80)) // (70+90)/2
	  
	  furgoneta.tipoDePaquetesValidos = Buffer(Normal, Urgente)
	  cliente.sucursalOrigen = sucursal3000
	  sucursal3000.transportes += furgoneta
	  cliente.sucursalDestino = sucursal1000
	  cliente.generarPaquete(2, Normal)
	  cliente.pedirEnvio
	  cliente.generarPaquete(3, Urgente)
	  cliente.pedirEnvio
	  
	  furgoneta.hacerEnvio
	  
	  cliente.generarPaquete(4, Normal)
	  cliente.pedirEnvio
	  
	  furgoneta.hacerEnvio
	  
	  assert(estadisticas.estadisticasPromedioGanancias.get(sucursal1000).contains(80)) // (70+90)/2
	  assert(estadisticas.estadisticasPromedioGanancias.get(sucursal3000).contains(115)) // ((70+90)+70)/2
	  
	}
	
	it should "mostrar tiempo promedio de todas las sucursales en analisis" in {
	  SistemaExterno.distanciaTerrestre = 50
	  estadisticas agregarSucursal(sucursal1000)
	  estadisticas agregarSucursal(sucursal3000)
	  sucursal1000.transportes += camion
	  cliente.generarPaquete(10, Normal)
	  cliente.pedirEnvio
	  
	  camion.hacerEnvio
	 
	  assert(estadisticas.estadisticasPromedioTiempos.get(sucursal1000).get === 0.83 +- 0.01)
	  SistemaExterno.distanciaTerrestre = 100
	  camion.tipoDePaquetesValidos = Buffer(Normal, Urgente)
	  cliente.generarPaquete(30, Urgente)
	  cliente.pedirEnvio
	  
	  camion.hacerEnvio
	  assert(estadisticas.estadisticasPromedioTiempos.get(sucursal1000).contains(1.25)) // ((50/60)+(100/60))/2
	  
	  SistemaExterno.distanciaTerrestre = 150
	  furgoneta.tipoDePaquetesValidos = Buffer(Normal, Urgente)
	  cliente.sucursalOrigen = sucursal3000
	  sucursal3000.transportes += furgoneta
	  cliente.sucursalDestino = sucursal1000
	  cliente.generarPaquete(2, Normal)
	  cliente.pedirEnvio
	  cliente.generarPaquete(3, Urgente)
	  cliente.pedirEnvio
	  
	  furgoneta.hacerEnvio
	  
	  SistemaExterno.distanciaTerrestre = 100
	  cliente.generarPaquete(4, Normal)
	  cliente.pedirEnvio
	  
	  furgoneta.hacerEnvio
	  
	  assert(estadisticas.estadisticasPromedioTiempos.get(sucursal1000).contains(1.25)) // ((50/60)+(100/60))/2
	  assert(estadisticas.estadisticasPromedioTiempos.get(sucursal3000).get === 1.56 +- 0.01) // ((150/80)+(100/80))/2
	  
	}
	
	it should "mostrar cantidad de paquetes enviados de todas las sucursales en analisis" in {
	  
	  estadisticas agregarSucursal(sucursal1000)
	  estadisticas agregarSucursal(sucursal3000)
	  sucursal1000.transportes += camion
	  cliente.generarPaquete(10, Normal)
	  cliente.pedirEnvio
	  
	  camion.hacerEnvio
	 
	  assert(estadisticas.estadisticasCantidadPaquetesEnviados.get(sucursal1000).contains(1))
	  
	  camion.tipoDePaquetesValidos = Buffer(Normal, Urgente)
	  cliente.generarPaquete(30, Urgente)
	  cliente.pedirEnvio
	  
	  camion.hacerEnvio
	  assert(estadisticas.estadisticasCantidadPaquetesEnviados.get(sucursal1000).contains(2))
	  
	  furgoneta.tipoDePaquetesValidos = Buffer(Normal, Urgente)
	  cliente.sucursalOrigen = sucursal3000
	  sucursal3000.transportes += furgoneta
	  cliente.sucursalDestino = sucursal1000
	  cliente.generarPaquete(2, Normal)
	  cliente.pedirEnvio
	  cliente.generarPaquete(3, Urgente)
	  cliente.pedirEnvio
	  
	  furgoneta.hacerEnvio
	  
	  cliente.generarPaquete(4, Normal)
	  cliente.pedirEnvio
	  
	  furgoneta.hacerEnvio
	  
	  assert(estadisticas.estadisticasCantidadPaquetesEnviados.get(sucursal1000).contains(2))
	  assert(estadisticas.estadisticasCantidadPaquetesEnviados.get(sucursal3000).contains(3)) 
	  
	}
	
	it should "mostrar cantidad de viajes de todas las sucursales en analisis" in {
	  
	  estadisticas agregarSucursal(sucursal1000)
	  estadisticas agregarSucursal(sucursal3000)
	  sucursal1000.transportes += camion
	  cliente.generarPaquete(10, Normal)
	  cliente.pedirEnvio
	  
	  camion.hacerEnvio
	 
	  assert(estadisticas.estadisticasCantidadViajes.get(sucursal1000).contains(1))
	  
	  camion.tipoDePaquetesValidos = Buffer(Normal, Urgente)
	  cliente.generarPaquete(30, Urgente)
	  cliente.pedirEnvio
	  
	  camion.hacerEnvio
	  assert(estadisticas.estadisticasCantidadViajes.get(sucursal1000).contains(2))
	  
	  furgoneta.tipoDePaquetesValidos = Buffer(Normal, Urgente)
	  cliente.sucursalOrigen = sucursal3000
	  sucursal3000.transportes += furgoneta
	  cliente.sucursalDestino = sucursal1000
	  cliente.generarPaquete(2, Normal)
	  cliente.pedirEnvio
	  cliente.generarPaquete(3, Urgente)
	  cliente.pedirEnvio
	  
	  furgoneta.hacerEnvio
	  
	  cliente.generarPaquete(4, Normal)
	  cliente.pedirEnvio
	  
	  furgoneta.hacerEnvio
	  
	  cliente.generarPaquete(4, Normal)
	  cliente.pedirEnvio
	  
	  furgoneta.hacerEnvio
	  
	  assert(estadisticas.estadisticasCantidadViajes.get(sucursal1000).contains(2))
	  assert(estadisticas.estadisticasCantidadViajes.get(sucursal3000).contains(3)) 
	  
	}
	
	it should "mostrar facturacion total de todas las sucursales en analisis" in {
	  
	  estadisticas agregarSucursal(sucursal1000)
	  estadisticas agregarSucursal(sucursal3000)
	  sucursal1000.transportes += camion
	  cliente.generarPaquete(10, Normal)
	  cliente.pedirEnvio
	  
	  camion.hacerEnvio
	 
	  assert(estadisticas.estadisticasFacturacionTotal.get(sucursal1000).contains(70)) // 80 - 10 = 70
	  
	  camion.tipoDePaquetesValidos = Buffer(Normal, Urgente)
	  cliente.generarPaquete(30, Urgente)
	  cliente.pedirEnvio
	  
	  camion.hacerEnvio
	  assert(estadisticas.estadisticasFacturacionTotal.get(sucursal1000).contains(160))
	  
	  furgoneta.tipoDePaquetesValidos = Buffer(Normal, Urgente)
	  cliente.sucursalOrigen = sucursal3000
	  sucursal3000.transportes += furgoneta
	  cliente.sucursalDestino = sucursal1000
	  cliente.generarPaquete(2, Normal)
	  cliente.pedirEnvio
	  cliente.generarPaquete(3, Urgente)
	  cliente.pedirEnvio
	  
	  furgoneta.hacerEnvio
	  
	  cliente.generarPaquete(4, Normal)
	  cliente.pedirEnvio
	  
	  furgoneta.hacerEnvio
	  
	  assert(estadisticas.estadisticasFacturacionTotal.get(sucursal1000).contains(160))
	  assert(estadisticas.estadisticasFacturacionTotal.get(sucursal3000).contains(230)) 
	  
	}
	
	it should "filtrar envios analizados por una restriccion de fecha" in {
	  var restriccionFecha = new RestriccionPorFecha()
	  restriccionFecha.fechaDesde.setDate(9)
	  restriccionFecha.fechaHasta.setDate(15)
	  
	  SistemaExterno.fechaActual.setDate(11)
	  
	  estadisticas.restriccionesEnvio += restriccionFecha
	  estadisticas agregarSucursal(sucursal1000)
	  sucursal1000.transportes += camion
	  cliente.generarPaquete(10, Normal)
	  cliente.pedirEnvio
	  
	  camion.hacerEnvio
	  
	  assert(estadisticas.estadisticasPromedioCostos.get(sucursal1000).contains(10))
	  
	  restriccionFecha.fechaDesde.setDate(12)

	  assert(estadisticas.estadisticasPromedioCostos.get(sucursal1000).contains(0))
	}
	
	it should "restringir por tipo de transporte" in {
	  estadisticas agregarSucursal(sucursal1000)
	  sucursal1000.transportes += camion
	  cliente.generarPaquete(10, Normal)
	  cliente.pedirEnvio
	  
	  camion.hacerEnvio
	  
	  assert(estadisticas.estadisticasPromedioCostos.get(sucursal1000).contains(10))
	  assert(estadisticas.estadisticasCantidadViajes.get(sucursal1000).contains(1))
	  
	  var restriccionTransporte = new RestriccionPorTransporte()
	  restriccionTransporte.tipoTransporte = "Camion"
	  estadisticas.restriccionesTransporte += restriccionTransporte
	  
	  assert(estadisticas.estadisticasPromedioCostos.get(sucursal1000).contains(0))
	  assert(estadisticas.estadisticasCantidadViajes.get(sucursal1000).contains(0))
	}
	
	it should "restringir por tipo de envio" in {
	  camion.tipoDePaquetesValidos = Buffer(Normal, Urgente)
	  estadisticas agregarSucursal(sucursal1000)
	  sucursal1000.transportes += camion
	  cliente.generarPaquete(10, Normal)
	  cliente.pedirEnvio
	  cliente.generarPaquete(10, Normal)
	  cliente.pedirEnvio
	  cliente.generarPaquete(10, Urgente)
	  cliente.pedirEnvio
	  camion.hacerEnvio
	  
	  assert(estadisticas.estadisticasCantidadPaquetesEnviados.get(sucursal1000).contains(3))
	  
	  var restriccionPaquete = new RestriccionPorTipo()
	  restriccionPaquete.tipoPaquete = Urgente
	  estadisticas.restriccionesPaquete += restriccionPaquete
	  
	  assert(estadisticas.estadisticasCantidadPaquetesEnviados.get(sucursal1000).contains(2))
	}
}