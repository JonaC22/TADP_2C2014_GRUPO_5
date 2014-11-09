package tadp_grupo5

import org.scalatest._

import scala.collection.mutable.Buffer
import scala.collection.mutable.Set

class TestsArgentinaExpress extends FlatSpec with BeforeAndAfter{
  
	object SistemaExterno extends CalculadorDistancia {
		var distanciaTerrestre : Double = 0.0
		var distanciaAerea :Double = 0.0
		var cantidadPeajes : Int = 0

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
	
	val sucursales = Buffer(sucursal10, sucursal20, sucursal30)
	
	val cliente = new Cliente(sucursal10, sucursal20)
	  
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
	val paquete20 = new Paquete(sucursal10, sucursal20,20, Normal)
	val paqueteConMuchoVolumen = new Paquete(sucursal10, sucursal20, 9999, Normal)
	val paqueteUrgente = new Paquete(sucursal10, sucursal20,1, Urgente) 
	val paqueteConRefrigeracion = new Paquete(sucursal10, sucursal20,10, NecesitaRefrigeracion)
	val paqueteFragil = new Paquete(sucursal10, sucursal20,2, Fragil)
	
	after{
	  cliente.paquete = null
	  cliente.sucursalOrigen = sucursal10
	  cliente.sucursalDestino = sucursal20
	  transportes.foreach(_.pedidos = Buffer())
	  transportes.foreach(_.servicioExtra = None)
	  transportes.foreach(_.infraestructura = None)
	  transportes.foreach(_.tipoDePaquetesValidos = Buffer(Normal))
	  sucursales.foreach(_.paquetesEnEntrar = Buffer())
	  sucursales.foreach(_.paquetesEnSalir = Buffer())
	  SistemaExterno.distanciaTerrestre  = 0.0
	  SistemaExterno.distanciaAerea  = 0.0
	  SistemaExterno.cantidadPeajes  = 0
	  estadisticas.transportesEnEstudio = Set()
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
	    furgoneta.asignarPaquete(paqueteUrgente) //no puede llevar paquetes fragiles ni urgentes
	  }
	  furgoneta.tipoDePaquetesValidos = Buffer(Normal, Urgente)
	  intercept[PaqueteTipoInvalido]{
	    furgoneta.asignarPaquete(paqueteFragil) //aun no puede enviar el paquete fragil
	  }
	  furgoneta.tipoDePaquetesValidos = Buffer(Normal, Urgente, Fragil)
	  furgoneta.asignarPaquete(paqueteUrgente)
	  furgoneta.asignarPaquete(paqueteFragil)
	  assert(furgoneta.pedidos.size == 2)
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
	  
	  assert(camion.gananciaEnvio == 62.25999999999999)
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
	  camion.asignarPaquete(paqueteUrgente)
	  camion.asignarPaquete(paqueteUrgente)
	  camion.infraestructura = Some(SustanciasPeligrosas)
	  SistemaExterno.distanciaTerrestre = 1
	  SistemaExterno.cantidadPeajes  = 2
	  
	  assert(camion.costoEnvioConAdicionales == 764.1333333333333)//40 + 100*0.5 + 2*12 + 600 + 3*((10+20)/45)
	  // ver si el calculo vol_paquete_urgente/vol_transporte es por paquete urgente o en conjunto
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
	  
	  assert(avion.gananciaEnvio == -1099930.0)
	}
	
	it should "no poder llevar paquetes con refrigeracion" in {
	  intercept[PaqueteTipoInvalido]{
	    avion.asignarPaquete(paqueteConRefrigeracion)
	  }
	}
	
	"Las estadisticas" should "mostrar ganancia total de todos los transportes en analisis" in {
	  
	  estadisticas agregarTransporte(camion)
	  estadisticas agregarTransporte(furgoneta)
	  sucursal10.transportes += camion
	  cliente.generarPaquete(1, Normal)
	  cliente.pedirEnvio
	  
	  camion.hacerEnvio
	  
	  assert(estadisticas.gananciaTotalDeTodosLosTransportes == 70) //80 - 10 = 70
	  
	  cliente.generarPaquete(3, Normal)
	  cliente.pedirEnvio
	  
	  camion.hacerEnvio
	  
	  assert(estadisticas.gananciaTotalDeTodosLosTransportes == 140) // 2*(80 - 10) = 140
	  
	  cliente.sucursalOrigen = sucursal30
	  sucursal30.transportes += furgoneta
	  cliente.sucursalDestino = sucursal10
	  cliente.generarPaquete(2, Normal)
	  cliente.generarPaquete(3, Normal)
	  cliente.generarPaquete(4, Normal)
	  cliente.pedirEnvio
	  furgoneta.hacerEnvio
	  
	  assert(estadisticas.gananciaTotalDeTodosLosTransportes == 350) //5*(80 - 10) = 350
	}
}