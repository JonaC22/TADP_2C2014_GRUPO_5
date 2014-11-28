package tadp_grupo5

import org.scalatest._
import scala.collection.immutable.Set
import java.util.Date

class TestsArgentinaExpress extends FlatSpec with BeforeAndAfter with Matchers{
  
    type funcionCalculo = List[Envio] => Double
  
	object SistemaExterno extends CalculadorDistancia {
		var distanciaTerrestre : Double = 0
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
	val sucursal2000 = new Sucursal(2000, "Brasil")
    val sucursal3000 = new Sucursal(3000, "Argentina")
	val casaCentral = new CasaCentral(20, "Brasil")
	val sucursales = List(sucursal10, sucursal20, sucursal30, sucursal1000, sucursal2000, sucursal3000, casaCentral)
	
	val flechaBus = new Compania()
	val chevallier = new Compania()
	val companias = List(flechaBus, chevallier)
	
	val cliente = new Cliente(sucursal1000, sucursal3000)
	  
	val estadisticas = new Estadisticas()
	
	val camion = Camion(SistemaExterno)
	val otroCamion = Camion(SistemaExterno)
	val avion = Avion(SistemaExterno)
	val furgoneta = Furgoneta(SistemaExterno)

	val paquete1 = new Paquete(sucursal10, sucursal20,1, Normal)
	val paqueteInvertido1 = new Paquete(sucursal20, sucursal10,1, Normal)
	val paquete2 = new Paquete(sucursal10, sucursal20,2, Normal)
	val paquete5 =new Paquete(sucursal30, sucursal20,5, Normal)
	val paquete10 = new Paquete(sucursal10, sucursal20,10, Normal)
	val paquete10CasaCentral = new Paquete(sucursal10, casaCentral,10, Normal)
	val paquete20 = new Paquete(sucursal10, sucursal20,20, Normal)
	val paquete50nacional = new Paquete(sucursal1000, sucursal2000, 50, Normal)
	val paquete50internacional = new Paquete(sucursal1000, sucursal3000, 50, Normal)
	val paqueteConMuchoVolumen = new Paquete(sucursal10, sucursal20, 9999, Normal)
	val paqueteUrgenteLiviano = new Paquete(sucursal10, sucursal20,1, Urgente) 
	val paqueteUrgentePesado = new Paquete(sucursal10, sucursal20,20, Urgente) 
	val paqueteConRefrigeracion = new Paquete(sucursal10, sucursal20,10, NecesitaRefrigeracion)
	val paqueteFragil = new Paquete(sucursal10, sucursal20,2, Fragil)
	
	after{
	  cliente.paquete = null
	  cliente.sucursalOrigen = sucursal1000
	  cliente.sucursalDestino = sucursal3000
	  sucursales.foreach(_.paquetesPorEntrar = List())
	  sucursales.foreach(_.paquetesPorSalir = List())
	  sucursales.foreach(_.transportes = List())
	  sucursales.foreach(_.enviosRealizados = List())
	  companias.foreach(_.sucursales = List())
	  SistemaExterno.distanciaTerrestre  = 1
	  SistemaExterno.distanciaAerea  = 0.0
	  SistemaExterno.cantidadPeajes  = 0
	  SistemaExterno.fechaActual.setDate(1)
	  estadisticas.companiasEnEstudio = List()
	  estadisticas.restriccionesEnvio = Set()
	  estadisticas.restriccionesPaquete = Set()
	}
    
	"Una sucursal" should "tener capacidad" in {
		sucursal10.notificarPaqueteASalir(paquete1)
		assert(sucursal10.capacidad == 9) //10 - 1 = 9
	}				

	it should " poder agregarse mas paquetes" in {
		sucursal10.notificarPaqueteASalir(paquete5)
		assert(sucursal10.capacidad == 5)
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
		var nuevoCamion = Despachante.agregarPedido(camion, paquete10)
		nuevoCamion = Despachante.agregarPedido(nuevoCamion, paquete20)

		assert(nuevoCamion.pedidos.size == 2)
		assert(nuevoCamion.capacidad == 15) //45 - 10 - 20 = 15
		
		nuevoCamion = Despachante.agregarPedido(nuevoCamion, paquete5)
		nuevoCamion = Despachante.agregarPedido(nuevoCamion, paquete10)

		assert(nuevoCamion.pedidos.size == 4)
		assert(nuevoCamion.capacidad == 0) //45 - 10 - 20 - 5 - 10 = 0
	}
	
	it should "no tener capacidad" in {
		
		var nuevoCamion = Despachante.agregarPedido(camion, paquete10)
		nuevoCamion = Despachante.agregarPedido(nuevoCamion, paquete20)

		assert(nuevoCamion.pedidos.size == 2)
		assert( nuevoCamion.capacidad == 15) //45 - 10 - 20 = 15

		intercept[TransporteSinCapacidad]{
		  nuevoCamion = Despachante.agregarPedido(nuevoCamion, paqueteConMuchoVolumen) //excedo la capacidad
		}
		
		assert(nuevoCamion.capacidad == 15)
		assert(nuevoCamion.pedidos.size == 2)
	}
	
	it should "no llevar paquetes de destinos diferentes" in {
		var nuevoCamion = Despachante.agregarPedido(camion, paquete1)
		
		intercept[PaquetesDestinoErroneo] {
		  nuevoCamion = Despachante.agregarPedido(nuevoCamion, paqueteInvertido1)//asigno paquetes iniciales con destino distinto
	    }
	   
		assert(nuevoCamion.pedidos.size == 1)

		nuevoCamion = Despachante.agregarPedido(nuevoCamion, paquete2)
		assert(nuevoCamion.pedidos.size == 2)
		assert(nuevoCamion.capacidad == 42) //45 - 1 - 2 = 42
	}
	
	it should "llevar tipos de paquetes especificados" in {
	  intercept[PaqueteTipoInvalido]{
	    Despachante.agregarPedido(furgoneta, paqueteUrgenteLiviano)//no puede llevar paquetes fragiles ni urgentes
	  }
	  var nuevaFurgoneta = Despachante.modificarTiposValidos(furgoneta, List(Normal, Urgente))
	  intercept[PaqueteTipoInvalido]{
	    Despachante.agregarPedido(nuevaFurgoneta, paqueteFragil)//aun no puede enviar el paquete fragil
	  }
	  nuevaFurgoneta = Despachante.modificarTiposValidos(furgoneta, List(Normal, Urgente, Fragil))
	  nuevaFurgoneta = Despachante.agregarPedido(nuevaFurgoneta, paqueteUrgenteLiviano)
	  nuevaFurgoneta = Despachante.agregarPedido(nuevaFurgoneta, paqueteFragil)

	  assert(nuevaFurgoneta.pedidos.size == 2)
	}
	
	it should "calcular costo del envio" in {
	  var nuevoCamion = Despachante.agregarPedido(camion, paquete10)
	  
	  assert(nuevoCamion.costoEnvio == 110)// 10 + 100
	}
	
	it should "calcular costo con adicionales" in {
	  var nuevoCamion = Despachante.agregarPedido(camion, paquete1)
	  
	  assert(nuevoCamion.costoEnvio === 112.22 +- 0.01)//10 + 100 * (1+ (45-44)/45)
	}
	
	it should "calcular la ganancia de un envio" in {
	  var nuevoCamion = Despachante.agregarPedido(camion, paquete10)
	  nuevoCamion = Despachante.agregarPedido(nuevoCamion, paquete20)

	  SistemaExterno.distanciaTerrestre = 0.5
	  SistemaExterno.cantidadPeajes  = 2
	  
	  assert(nuevoCamion.costoEnvio == 94)// 10+10 + 100 * 0.5 + 2*12
	  assert(nuevoCamion.gananciaEnvio == 66) // 160 - 94
	}
	
	it should "calcular la ganancia de un envio con distintos seguimientos" in {
	  var nuevoCamion = Despachante.agregarPedido(camion, paquete10)
	  nuevoCamion = Despachante.agregarPedido(nuevoCamion, paquete20)

	  nuevoCamion = Despachante.modificarServicioExtra(nuevoCamion, Some(SeguimientoSatelital))

	  SistemaExterno.distanciaTerrestre = 0.5
	  SistemaExterno.cantidadPeajes  = 2
	  assert(nuevoCamion.costoEnvio == 94.5)// 10 + 10 + 100*0.5 + 2*12 + 1*0.5
	  assert(nuevoCamion.gananciaEnvio == 65.5)//160 - 94.5
	  
	  nuevoCamion = Despachante.modificarServicioExtra(nuevoCamion, Some(SeguimientoSatelitalConVideo))

	  assert(nuevoCamion.costoEnvio == 97.74000000000001)// 10 + 10 + 100*0.5 + 2*12 + 1*3.74
	  assert(nuevoCamion.gananciaEnvio === 62.26 +- 0.01)//160 - 97.74
	}
	
	it should "calcular el costo de un envio dependiendo la infraestructura" in {
	  var nuevoCamion = Despachante.agregarPedido(camion, paquete10)
	  nuevoCamion = Despachante.agregarPedido(nuevoCamion, paquete20)

	  nuevoCamion = Despachante.modificarInfraestructura(nuevoCamion, Some(SustanciasPeligrosas))

	  SistemaExterno.distanciaTerrestre = 0.5
	  SistemaExterno.cantidadPeajes  = 2
	  
	  assert(nuevoCamion.costoEnvio == 694)//20 + 100*0.5 + 2*12 + 600
	
	  nuevoCamion = Despachante.modificarInfraestructura(nuevoCamion, Some(Animales))

	  //distancia menor a 100km
	  SistemaExterno.distanciaTerrestre = 50
	  
	  assert(nuevoCamion.costoEnvio == 5094)//20 + 100*50 + 2*12 + 50
	  
	  //distancia menor a 200km
	  SistemaExterno.distanciaTerrestre = 130
	  assert(nuevoCamion.costoEnvio == 13130)//20 + 100*130 + 2*12 + 86
	  
	  //distancia mayor a 200km
	  SistemaExterno.distanciaTerrestre = 240
	  assert(nuevoCamion.costoEnvio == 24181)//20 + 100*240 + 2*12 + 137
	}
	
	"Un camion" should "poder llevar paquetes con refrigeracion" in {
	  var nuevoCamion = Despachante.agregarPedido(camion, paqueteConRefrigeracion)
	  assert(nuevoCamion.pedidos.size == 1)
	}
	
	it should "calcular el costo de un envio con sustancias peligrosas y paquetes urgentes" in {
	  var nuevoCamion = Despachante.modificarTiposValidos(camion, List(Normal, Urgente))
	  nuevoCamion = Despachante.agregarPedido(nuevoCamion, paqueteUrgentePesado)
	  nuevoCamion = Despachante.agregarPedido(nuevoCamion, paqueteUrgentePesado)
	  nuevoCamion = Despachante.modificarInfraestructura(nuevoCamion, Some(SustanciasPeligrosas))
	  
	  assert(nuevoCamion.capacidad == 5)
	  assert(nuevoCamion.costoEnvio == 742.6666666666666)// 40 + 100 + 3*(40/45) + 600
	}
	
	it should "calcular costo de ultima semana del mes yendo a casa central" in {
	  var nuevoCamion = Despachante.agregarPedido(camion, paquete10CasaCentral)

	  SistemaExterno.fechaActual.setDate(24)
	  
	  assert(nuevoCamion.costoEnvio == 112.2)// (10+100) * 1.02
	  assert(nuevoCamion.sucursalDestino.esCasaCentral)
	}
	
	it should "calcular costo con adicional por volumen no aceptable" in {
	  var nuevoCamion = Despachante.agregarPedido(camion, paquete1)
	  
	  assert(nuevoCamion.costoEnvio == 112.22222222222221) //10 + 100 * (1 + (45-44)/45)
	  assert(nuevoCamion.volumen - nuevoCamion.capacidad == 1)
	  assert(nuevoCamion.volumen == 45)
	}
	
	"Un avion" should "no poder hacer viajes menor o igual a 1000 kilometros" in {

	  SistemaExterno.distanciaAerea = 900.0
	  
	  intercept[EnvioConDistanciaMenorA1000KM]{
	    Despachante.agregarPedido(avion, paquete1)
	  }
	  
	  SistemaExterno.distanciaAerea = 1000.0
	  
	  intercept[EnvioConDistanciaMenorA1000KM]{
	    Despachante.agregarPedido(avion, paquete1)
	  }	  
	  
	  SistemaExterno.distanciaAerea = 1100.0

	}
	
	it should "no poder llevar paquetes con refrigeracion" in {
	  intercept[PaqueteTipoInvalido]{
	    Despachante.agregarPedido(avion, paqueteConRefrigeracion)
	  }
	}
	
	it should "pagar 10% de impuesto si hace viajes internacionales" in {
	  SistemaExterno.distanciaAerea = 1500
	  var nuevoAvion = Despachante.agregarPedido(avion, paquete50nacional)

	  assert(nuevoAvion.costoEnvio == 750010)
	  nuevoAvion = Despachante.vaciarTransporte(nuevoAvion)
	  nuevoAvion = Despachante.agregarPedido(avion, paquete50internacional)

	  assert(nuevoAvion.costoEnvio == 825011.0000000001)
	}
	
	"Las estadisticas" should "ser parametrizables" in {
	  
	  val distanciaTotalEnvios : funcionCalculo = { envios => 
      (for {
    	  envio <- envios
      	} yield envio.distanciaRecorrida).sum
      }
	  
	  SistemaExterno.distanciaTerrestre = 200
	  var nuevoCamion = Despachante.modificarTiposValidos(camion, List(Normal, Urgente))
	  flechaBus.agregarSucursal(sucursal1000)
	  flechaBus.agregarSucursal(sucursal3000)
	  estadisticas agregarCompania(flechaBus)
	  sucursal1000.transportes = sucursal1000.transportes :+ nuevoCamion
	  cliente.generarPaquete(10, Normal)
	  cliente.pedirEnvio
	  
	  sucursal1000.despacharEnvios

	  assert(estadisticas.estadisticasSucursales(distanciaTotalEnvios).contains(sucursal1000,200))
	  
	  SistemaExterno.distanciaTerrestre = 500

	  cliente.generarPaquete(30, Urgente)
	  cliente.pedirEnvio

	  sucursal1000.despacharEnvios
	 
	  assert(estadisticas.estadisticasSucursales(distanciaTotalEnvios).contains(sucursal1000,700)) // (10+20)/2
	  
	  
	  SistemaExterno.distanciaTerrestre = 1500
	  var nuevaFurgoneta = Despachante.modificarTiposValidos(furgoneta, List(Normal, Urgente))

	  cliente.sucursalOrigen = sucursal3000
	  sucursal3000.transportes = sucursal3000.transportes :+ nuevaFurgoneta
	  cliente.sucursalDestino = sucursal1000
	  cliente.generarPaquete(2, Normal)
	  cliente.pedirEnvio

	  cliente.generarPaquete(3, Urgente)
	  cliente.pedirEnvio

	  sucursal3000.despacharEnvios
	  	  
	  cliente.generarPaquete(4, Normal)
	  cliente.pedirEnvio
	  
	  sucursal3000.despacharEnvios
	  
	  assert(estadisticas.estadisticasSucursales(distanciaTotalEnvios).contains(sucursal1000,700)) // (10+20)/2
	  assert(estadisticas.estadisticasSucursales(distanciaTotalEnvios).contains(sucursal3000,3000)) // (30+10)/2
	}
	
	it should "mostrar costo promedio de las sucursales en analisis" in {
	  flechaBus.agregarSucursal(sucursal1000)
	  flechaBus.agregarSucursal(sucursal3000)
	  estadisticas agregarCompania(flechaBus)
	  
	  var nuevoCamion = Despachante.modificarTiposValidos(camion, List(Normal, Urgente))
	  sucursal1000.transportes = sucursal1000.transportes :+ nuevoCamion
	  
	  cliente.generarPaquete(10, Normal)
	  cliente.pedirEnvio
	  
	  sucursal1000.despacharEnvios
	  
	  assert(estadisticas.estadisticasPromedioCostos.contains(sucursal1000,110)) //110

	  cliente.generarPaquete(30, Urgente)
	  cliente.pedirEnvio

	  sucursal1000.despacharEnvios
	  
	  assert(estadisticas.estadisticasPromedioCostos.contains(sucursal1000,115)) //(110 + 120) / 2
	  
	  var nuevaFurgoneta = Despachante.modificarTiposValidos(furgoneta, List(Normal, Urgente))

	  cliente.sucursalOrigen = sucursal3000
	  sucursal3000.transportes = sucursal3000.transportes :+ nuevaFurgoneta
	  cliente.sucursalDestino = sucursal1000
	  cliente.generarPaquete(2, Normal)
	  cliente.pedirEnvio
	  cliente.generarPaquete(3, Normal)
	  cliente.pedirEnvio

	  sucursal3000.despacharEnvios
	  
	  cliente.generarPaquete(4, Normal)
	  cliente.pedirEnvio
	  
	  sucursal3000.despacharEnvios

	  assert(estadisticas.estadisticasPromedioCostos.contains(sucursal1000,115)) // 
	  assert(estadisticas.estadisticasPromedioCostos.contains(sucursal3000,55)) // (60+50)/2
	}
	
	it should "mostrar ganancia promedio de todas las sucursales en analisis" in {
	  flechaBus.agregarSucursal(sucursal1000)
	  flechaBus.agregarSucursal(sucursal3000)
	  estadisticas agregarCompania(flechaBus)
	  var nuevoCamion = Despachante.modificarTiposValidos(camion, List(Normal, Urgente))
	  sucursal1000.transportes = sucursal1000.transportes :+ nuevoCamion
	  cliente.generarPaquete(10, Normal)
	  cliente.pedirEnvio
	  
	  sucursal1000.despacharEnvios
	 
	  assert(estadisticas.estadisticasPromedioGanancias.contains(sucursal1000,-30))// 80 - 110

	  cliente.generarPaquete(30, Urgente)
	  cliente.pedirEnvio
	  
	  sucursal1000.despacharEnvios
	  
	  assert(estadisticas.estadisticasPromedioGanancias.contains(sucursal1000,-20)) // (-30 + -10) / 2
	  
	  var nuevaFurgoneta = Despachante.modificarTiposValidos(furgoneta, List(Normal, Urgente))

	  cliente.sucursalOrigen = sucursal3000
	  sucursal3000.transportes = sucursal3000.transportes :+ nuevaFurgoneta
	  cliente.sucursalDestino = sucursal1000
	  cliente.generarPaquete(2, Normal)
	  cliente.pedirEnvio
	  cliente.generarPaquete(3, Urgente)
	  cliente.pedirEnvio
	  
	  sucursal3000.despacharEnvios
	  
	  cliente.generarPaquete(4, Normal)
	  cliente.pedirEnvio
	  
	  sucursal3000.despacharEnvios
	  
	  assert(estadisticas.estadisticasPromedioGanancias.contains(sucursal1000,-20))
	  assert(estadisticas.estadisticasPromedioGanancias.contains(sucursal3000,75)) // (120 + 30) / 2
	  
	}
	
	it should "mostrar tiempo promedio de todas las sucursales en analisis" in {
	  SistemaExterno.distanciaTerrestre = 50
	  flechaBus.agregarSucursal(sucursal1000)
	  flechaBus.agregarSucursal(sucursal3000)
	  estadisticas agregarCompania(flechaBus)
	  var nuevoCamion = Despachante.modificarTiposValidos(camion, List(Normal, Urgente))

	  sucursal1000.transportes = sucursal1000.transportes :+ nuevoCamion
	  cliente.generarPaquete(10, Normal)
	  cliente.pedirEnvio
	  
	  sucursal1000.despacharEnvios
	 
	  assert(estadisticas.estadisticasPromedioTiempos.contains(sucursal1000, 0.8333333333333334))
	  SistemaExterno.distanciaTerrestre = 100
	  
	  cliente.generarPaquete(30, Urgente)
	  cliente.pedirEnvio
	  
	  sucursal1000.despacharEnvios
	  assert(estadisticas.estadisticasPromedioTiempos.contains(sucursal1000,1.25)) // ((50/60)+(100/60))/2
	  
	  SistemaExterno.distanciaTerrestre = 150
	  var nuevaFurgoneta = Despachante.modificarTiposValidos(furgoneta, List(Normal, Urgente))

	  cliente.sucursalOrigen = sucursal3000
	  sucursal3000.transportes = sucursal3000.transportes :+ nuevaFurgoneta
	  cliente.sucursalDestino = sucursal1000
	  cliente.generarPaquete(2, Normal)
	  cliente.pedirEnvio
	  cliente.generarPaquete(3, Urgente)
	  cliente.pedirEnvio
	  
	  sucursal3000.despacharEnvios
	  
	  SistemaExterno.distanciaTerrestre = 100
	  cliente.generarPaquete(4, Normal)
	  cliente.pedirEnvio
	  
	  sucursal3000.despacharEnvios
	  
	  assert(estadisticas.estadisticasPromedioTiempos.contains(sucursal1000, 1.25)) // ((50/60)+(100/60))/2
	  assert(estadisticas.estadisticasPromedioTiempos.contains(sucursal3000, 1.5625)) // ((150/80)+(100/80))/2
	}
	
	it should "mostrar cantidad de paquetes enviados de todas las sucursales en analisis" in {
	  
	  flechaBus.agregarSucursal(sucursal1000)
	  flechaBus.agregarSucursal(sucursal3000)
	  estadisticas agregarCompania(flechaBus)
	  var nuevoCamion = Despachante.modificarTiposValidos(camion, List(Normal, Urgente))
	  sucursal1000.transportes = sucursal1000.transportes :+ nuevoCamion
	  cliente.generarPaquete(10, Normal)
	  cliente.pedirEnvio
	  
	  sucursal1000.despacharEnvios
	 
	  assert(estadisticas.estadisticasCantidadPaquetesEnviados.contains(sucursal1000,1))
	  
	  cliente.generarPaquete(30, Urgente)
	  cliente.pedirEnvio
	  
	  sucursal1000.despacharEnvios
	  assert(estadisticas.estadisticasCantidadPaquetesEnviados.contains(sucursal1000,2))
	  
	  var nuevaFurgoneta = Despachante.modificarTiposValidos(furgoneta, List(Normal, Urgente))

	  cliente.sucursalOrigen = sucursal3000
	  sucursal3000.transportes = sucursal3000.transportes :+ nuevaFurgoneta
	  cliente.sucursalDestino = sucursal1000
	  cliente.generarPaquete(2, Normal)
	  cliente.pedirEnvio
	  cliente.generarPaquete(3, Urgente)
	  cliente.pedirEnvio
	  
	  sucursal3000.despacharEnvios
	  
	  cliente.generarPaquete(4, Normal)
	  cliente.pedirEnvio
	  
	  sucursal3000.despacharEnvios
	  
	  assert(estadisticas.estadisticasCantidadPaquetesEnviados.contains(sucursal1000,2))
	  assert(estadisticas.estadisticasCantidadPaquetesEnviados.contains(sucursal3000,3)) 
	  
	}
	
	it should "mostrar cantidad de viajes de todas las sucursales en analisis" in {
	  
	  flechaBus.agregarSucursal(sucursal1000)
	  flechaBus.agregarSucursal(sucursal3000)
	  estadisticas agregarCompania(flechaBus)
	  var nuevoCamion = Despachante.modificarTiposValidos(camion, List(Normal, Urgente))
	  sucursal1000.transportes = sucursal1000.transportes :+ nuevoCamion
	  cliente.generarPaquete(10, Normal)
	  cliente.pedirEnvio
	  
	  sucursal1000.despacharEnvios
	 
	  assert(estadisticas.estadisticasCantidadViajes.contains(sucursal1000,1))

	  cliente.generarPaquete(30, Urgente)
	  cliente.pedirEnvio
	  
	  sucursal1000.despacharEnvios
	  assert(estadisticas.estadisticasCantidadViajes.contains(sucursal1000,2))
	  
	  var nuevaFurgoneta = Despachante.modificarTiposValidos(furgoneta, List(Normal, Urgente))

	  cliente.sucursalOrigen = sucursal3000
	  sucursal3000.transportes = sucursal3000.transportes :+ nuevaFurgoneta
	  cliente.sucursalDestino = sucursal1000
	  cliente.generarPaquete(2, Normal)
	  cliente.pedirEnvio
	  cliente.generarPaquete(3, Urgente)
	  cliente.pedirEnvio
	  
	  sucursal3000.despacharEnvios
	  
	  cliente.generarPaquete(4, Normal)
	  cliente.pedirEnvio
	  
	  sucursal3000.despacharEnvios
	  
	  cliente.generarPaquete(4, Normal)
	  cliente.pedirEnvio
	  
	  sucursal3000.despacharEnvios
	  
	  assert(estadisticas.estadisticasCantidadViajes.contains(sucursal1000,2))
	  assert(estadisticas.estadisticasCantidadViajes.contains(sucursal3000,3)) 
	}
	
	it should "mostrar facturacion total de todas las sucursales en analisis" in {
	  
	  flechaBus.agregarSucursal(sucursal1000)
	  flechaBus.agregarSucursal(sucursal3000)
	  estadisticas agregarCompania(flechaBus)
	  var nuevoCamion = Despachante.modificarTiposValidos(camion, List(Normal, Urgente))
	  sucursal1000.transportes = sucursal1000.transportes :+ nuevoCamion
	  cliente.generarPaquete(10, Normal)
	  cliente.pedirEnvio
	  
	  sucursal1000.despacharEnvios
	 
	  assert(estadisticas.estadisticasFacturacionTotalSucursales.contains(sucursal1000,-30)) // 80 - 10 = 70

	  cliente.generarPaquete(30, Urgente)
	  cliente.pedirEnvio
	  
	  sucursal1000.despacharEnvios
	  assert(estadisticas.estadisticasFacturacionTotalSucursales.contains(sucursal1000,-40))
	  
	  var nuevaFurgoneta = Despachante.modificarTiposValidos(furgoneta, List(Normal, Urgente))

	  cliente.sucursalOrigen = sucursal3000
	  sucursal3000.transportes = sucursal3000.transportes :+ nuevaFurgoneta
	  cliente.sucursalDestino = sucursal1000
	  cliente.generarPaquete(2, Normal)
	  cliente.pedirEnvio
	  cliente.generarPaquete(3, Urgente)
	  cliente.pedirEnvio
	  
	  sucursal3000.despacharEnvios
	  
	  cliente.generarPaquete(4, Normal)
	  cliente.pedirEnvio
	  
	  sucursal3000.despacharEnvios
	  
	  assert(estadisticas.estadisticasFacturacionTotalSucursales.contains(sucursal1000,-40))
	  assert(estadisticas.estadisticasFacturacionTotalSucursales.contains(sucursal3000,150)) 
	  
	}
	
	it should "filtrar envios por una restriccion de fecha" in {
	  var restriccionFecha = RestriccionPorFecha()
	  restriccionFecha.fechaDesde.setDate(9)
	  restriccionFecha.fechaHasta.setDate(15)
	  
	  SistemaExterno.fechaActual.setDate(11)
	  
	  estadisticas.restriccionesEnvio += restriccionFecha
	  flechaBus.agregarSucursal(sucursal1000)
	  estadisticas agregarCompania(flechaBus)
	  sucursal1000.transportes = sucursal1000.transportes :+ camion
	  cliente.generarPaquete(10, Normal)
	  cliente.pedirEnvio
	  
	  sucursal1000.despacharEnvios
	  
	  assert(estadisticas.estadisticasPromedioCostos.contains(sucursal1000,110))
	  
	  restriccionFecha.fechaDesde.setDate(12)

	  assert(estadisticas.estadisticasPromedioCostos.contains(sucursal1000,0))
	}
	
	it should "filtrar envios por una restriccion de tipo de envio" in {
	  var nuevoCamion = Despachante.modificarTiposValidos(camion, List(Normal, Urgente))

	  flechaBus.agregarSucursal(sucursal1000)
	  estadisticas agregarCompania(flechaBus)
	  sucursal1000.transportes = sucursal1000.transportes :+ nuevoCamion
	  cliente.generarPaquete(10, Normal)
	  cliente.pedirEnvio
	  cliente.generarPaquete(10, Normal)
	  cliente.pedirEnvio
	  cliente.generarPaquete(10, Urgente)
	  cliente.pedirEnvio
	  sucursal1000.despacharEnvios
	  
	  assert(estadisticas.estadisticasCantidadPaquetesEnviados.contains(sucursal1000,3))
	  
	  var restriccionPaquete = new RestriccionPorTipoPaquete(Urgente) //quiero solamente los paquetes urgentes
	  estadisticas.restriccionesPaquete += restriccionPaquete
	  
	  assert(estadisticas.estadisticasCantidadPaquetesEnviados.contains(sucursal1000,1))
	}
	
	it should "filtrar envios por una restriccion de tipo de transporte" in {
	  
	  estadisticas.restriccionesEnvio = Set(RestriccionPorCamion())
	  flechaBus.agregarSucursal(sucursal1000)
	  estadisticas agregarCompania(flechaBus)
	  sucursal1000.transportes = sucursal1000.transportes :+ camion
	  cliente.generarPaquete(10, Normal)
	  cliente.pedirEnvio
	  
	  sucursal1000.despacharEnvios
	  
	  assert(estadisticas.estadisticasFacturacionTotalSucursales.contains(sucursal1000,-30)) //80 - 10 = 70
	  
	  estadisticas.restriccionesEnvio = Set(RestriccionPorFurgoneta())
	  
	  assert(estadisticas.estadisticasFacturacionTotalSucursales.contains(sucursal1000,0))
	}
	
	it should "Dada una sucursal la cantidad de viajes segun cada tipos de transportes" in {
	  flechaBus.agregarSucursal(sucursal1000)
	  estadisticas agregarCompania(flechaBus)
	  SistemaExterno.distanciaAerea = 1100
	  
	  var nuevaFurgoneta = Despachante.modificarTiposValidos(furgoneta, List(Urgente))
	  var nuevoAvion = Despachante.modificarTiposValidos(avion, List(Fragil))
	  
	  sucursal1000.transportes = sucursal1000.transportes :+ camion
	  sucursal1000.transportes = sucursal1000.transportes :+ nuevaFurgoneta
	  sucursal1000.transportes = sucursal1000.transportes :+ nuevoAvion
	  
	  
	  cliente.generarPaquete(6, NecesitaRefrigeracion)
	  cliente.pedirEnvio
	  cliente.generarPaquete(10, NecesitaRefrigeracion)
	  cliente.pedirEnvio
	  sucursal1000.despacharEnvios
	  cliente.generarPaquete(10, NecesitaRefrigeracion)
	  cliente.pedirEnvio
	  sucursal1000.despacharEnvios//el camion hizo dos viajes
	  
	  cliente.generarPaquete(6, Urgente)
	  cliente.pedirEnvio
	  sucursal1000.despacharEnvios
	  cliente.generarPaquete(3, Urgente)
	  cliente.pedirEnvio
	  sucursal1000.despacharEnvios
	  cliente.generarPaquete(5, Urgente)
	  cliente.pedirEnvio
	  sucursal1000.despacharEnvios //la furgoneta hizo tres viajes
	  
	  cliente.generarPaquete(60, Fragil)
	  cliente.pedirEnvio
	  cliente.generarPaquete(30, Fragil)
	  cliente.pedirEnvio
	  cliente.generarPaquete(50, Fragil)
	  cliente.pedirEnvio
	  sucursal1000.despacharEnvios //el avion hizo un envio
	  
	  assert(estadisticas.estadisticasCantidadViajes.contains(sucursal1000,6))
	  estadisticas.restriccionesEnvio = Set(RestriccionPorCamion())
	  assert(estadisticas.estadisticasCantidadViajes.contains(sucursal1000,2))
	  estadisticas.restriccionesEnvio = Set(RestriccionPorFurgoneta())
	  assert(estadisticas.estadisticasCantidadViajes.contains(sucursal1000,3))
	  estadisticas.restriccionesEnvio = Set(RestriccionPorAvion())
	  assert(estadisticas.estadisticasCantidadViajes.contains(sucursal1000,1))
	}
	
	it should "la facturacion total (en un rango de fechas) para cada tipo de transporte para todo el sistema" in {
	  flechaBus.agregarSucursal(sucursal1000)
	  estadisticas agregarCompania(flechaBus)
	  SistemaExterno.fechaActual.setDate(18)
	  val restriccionFecha = RestriccionPorFecha()
	  restriccionFecha.fechaDesde.setDate(15)
	  restriccionFecha.fechaHasta.setDate(30)
	  estadisticas.restriccionesEnvio += restriccionFecha 
	  var nuevaFurgoneta = Despachante.modificarTiposValidos(furgoneta, List(Urgente))
	  var trans: List[Transporte] = List(camion,otroCamion,nuevaFurgoneta,avion)
	  sucursal1000.transportes ++= trans //todos los transportes del sistema

	  
	  cliente.generarPaquete(12, NecesitaRefrigeracion)
	  cliente.pedirEnvio
	  cliente.generarPaquete(10, NecesitaRefrigeracion)
	  cliente.pedirEnvio
	  sucursal1000.despacharEnvios //el camion hizo los viajes la fecha 18
	  
	  SistemaExterno.fechaActual = new Date()
	  SistemaExterno.fechaActual.setDate(27)
	  
	  cliente.generarPaquete(6, Urgente)
	  cliente.pedirEnvio
	  sucursal1000.despacharEnvios
	  cliente.generarPaquete(3, Urgente)
	  cliente.pedirEnvio
	  sucursal1000.despacharEnvios //la furgoneta hizo los envios la fecha 27
	  
	  assert(estadisticas.estadisticasFacturacionTotalTransportes == 260)
	  estadisticas.restriccionesEnvio = Set(RestriccionPorCamion()) //quiero filtrar por camiones
	  assert(estadisticas.estadisticasFacturacionTotalTransportes == 160)
	  estadisticas.restriccionesEnvio = Set(RestriccionPorFurgoneta()) //quiero filtrar por furgonetas
	  assert(estadisticas.estadisticasFacturacionTotalTransportes == 100)
	  
	  restriccionFecha.fechaHasta.setDate(25) //tomo las facturaciones hechas hasta el 25, la furgoneta no deberia incluirse
	  estadisticas.restriccionesEnvio = Set()
	  assert(estadisticas.estadisticasFacturacionTotalTransportes == 260)
	  estadisticas.restriccionesEnvio = Set(RestriccionPorCamion())
	  assert(estadisticas.estadisticasFacturacionTotalTransportes == 160)
	  estadisticas.restriccionesEnvio = Set(RestriccionPorFurgoneta())
	  assert(estadisticas.estadisticasFacturacionTotalTransportes == 100)
	}
	
	it should "El tiempo (o costo) promedio de cada tipo de transporte" in {
	  flechaBus.agregarSucursal(sucursal1000)
	  estadisticas agregarCompania(flechaBus)
	  SistemaExterno.distanciaTerrestre = 1000
	  var nuevaFurgoneta = Despachante.modificarTiposValidos(furgoneta, List(Urgente))
	  var trans: List[Transporte] = List(camion,otroCamion,nuevaFurgoneta,avion)
	  sucursal1000.transportes ++= trans //todos los transportes del sistema
	  
	  cliente.generarPaquete(12, NecesitaRefrigeracion)
	  cliente.pedirEnvio
	  sucursal1000.despacharEnvios
	  cliente.generarPaquete(10, NecesitaRefrigeracion)
	  cliente.pedirEnvio
	  sucursal1000.despacharEnvios
	  
	  cliente.generarPaquete(6, Urgente)
	  cliente.pedirEnvio
	  sucursal1000.despacharEnvios
	  cliente.generarPaquete(3, Urgente)
	  cliente.pedirEnvio
	  sucursal1000.despacharEnvios
	  
	  assert(estadisticas.estadisticasPromedioCostos.contains(sucursal1000,70050))
	  assert(estadisticas.estadisticasPromedioTiempos.contains(sucursal1000,14.583333333333334))
	  estadisticas.restriccionesEnvio = Set(RestriccionPorCamion()) //quiero filtrar por camiones
	  assert(estadisticas.estadisticasPromedioCostos.contains(sucursal1000,100080))
	  assert(estadisticas.estadisticasPromedioTiempos.contains(sucursal1000,16.666666666666668))
	  estadisticas.restriccionesEnvio = Set(RestriccionPorFurgoneta()) //quiero filtrar por furgonetas
	  assert(estadisticas.estadisticasPromedioCostos.contains(sucursal1000,40020))
	  assert(estadisticas.estadisticasPromedioTiempos.contains(sucursal1000,12.5))
	}
	
	it should "La facturacion total de cada compania por cada sucursal" in {
	  
	  flechaBus.agregarSucursal(sucursal1000)
	  flechaBus.agregarSucursal(sucursal2000)
	  chevallier.agregarSucursal(sucursal3000)
	  estadisticas agregarCompania(flechaBus)
	  estadisticas agregarCompania(chevallier)
	  sucursal1000.transportes = sucursal1000.transportes :+ camion
	  cliente.generarPaquete(10, Normal)
	  cliente.pedirEnvio
	  
	  sucursal1000.despacharEnvios
	  
	  assert(estadisticas.estadisticasFacturacionTotalCompanias.contains((flechaBus,(sucursal1000,-30))))
	  
	  sucursal2000.transportes = sucursal2000.transportes :+ otroCamion
	  cliente.sucursalOrigen = sucursal2000
	  cliente.generarPaquete(10, Normal)
	  cliente.sucursalDestino = sucursal3000
	  cliente.pedirEnvio
	  
	  sucursal2000.despacharEnvios
	  
	  assert(estadisticas.estadisticasFacturacionTotalCompanias.contains((flechaBus,(sucursal2000,-30))))
	  var nuevaFurgoneta = Despachante.modificarTiposValidos(furgoneta, List(Normal,Urgente))
	  cliente.sucursalOrigen = sucursal3000
	  sucursal3000.transportes = sucursal3000.transportes :+ furgoneta
	  cliente.sucursalDestino = sucursal1000
	  cliente.generarPaquete(2, Normal)
	  cliente.pedirEnvio
	  cliente.generarPaquete(3, Urgente)
	  cliente.pedirEnvio
	  
	  sucursal3000.despacharEnvios
	  
	  cliente.generarPaquete(4, Normal)
	  cliente.pedirEnvio
	  
	  sucursal3000.despacharEnvios
	  
	  assert(estadisticas.estadisticasFacturacionTotalCompanias.contains((flechaBus,(sucursal1000,-30))))
	  assert(estadisticas.estadisticasFacturacionTotalCompanias.contains((flechaBus,(sucursal2000,-30))))
	  assert(estadisticas.estadisticasFacturacionTotalCompanias.contains((chevallier,(sucursal3000,60)))) 
	  
	}
}