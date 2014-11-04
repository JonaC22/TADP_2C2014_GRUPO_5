package tadp_grupo5

import org.scalatest._

import scala.collection.mutable.Buffer

class TestsArgentinaExpress extends FlatSpec with BeforeAndAfter{
  
	object SistemaExterno extends CalculadorDistancia {
		var distanciaTerrestre : Double = 0.0
		var distanciaAerea :Double = 0.0
		var cantidadPeajes : Int = 0

		override def distanciaTerrestreEntre(sucursal1: Sucursal, sucursal2: Sucursal): Double = {
			distanciaTerrestre
		}

		override def distanciaAereaEntre(sucursal1: Sucursal, sucursal2: Sucursal): Double = {
			distanciaAerea
		}

		override def cantidadPeajesEntre(sucursal1: Sucursal, sucursal2: Sucursal): Int = {
			cantidadPeajes
		}
	}
	

	var sucursal1 = new Sucursal(10, "Argentina")
	var sucursal2 = new Sucursal(20, "Argentina")
    var sucursal3 = new Sucursal(10, "Uruguay")
	
	var cliente = new Cliente(sucursal1, sucursal2)
	  
	var estadisticas = new Estadisticas()
	
	var camion = new Camion(SistemaExterno)
	var avion = new Avion(SistemaExterno)
	var furgoneta = new Furgoneta(SistemaExterno)
	
	var paquetes = Buffer(new Paquete(sucursal1, sucursal2,10, Normal), new Paquete(sucursal1, sucursal2,20, Normal))
	
	after{
	  cliente.paquetes = Buffer()
	  cliente.sucursalOrigen = sucursal1
	  cliente.sucursalDestino = sucursal2
	  camion.pedidos = Buffer()
	  camion.servicioExtra = None
	  avion.pedidos = Buffer()
	  avion.servicioExtra = None
	  furgoneta.pedidos = Buffer()
	  furgoneta.servicioExtra = None
	  sucursal1.paquetesEnEntrar = Buffer()
	  sucursal1.paquetesEnSalir = Buffer()
	  sucursal2.paquetesEnEntrar = Buffer()
	  sucursal2.paquetesEnSalir = Buffer()
	  sucursal3.paquetesEnEntrar = Buffer()
	  sucursal3.paquetesEnSalir = Buffer()
	  SistemaExterno.distanciaTerrestre  = 0.0
	  SistemaExterno.distanciaAerea  = 0.0
	  SistemaExterno.cantidadPeajes  = 0
	}
    
	"Una sucursal" should "tener capacidad" in {
		var esperandoSalida = Buffer(new Paquete(sucursal1, sucursal2,1, Normal), new Paquete(sucursal1, sucursal2,3, Normal))
		sucursal3.notificarPaquetesASalir(esperandoSalida)
		assert(sucursal3.capacidad == 6) //10 - 1 - 3 = 6
	}				

	it should " poder agregarse mas paquetes" in {
	    var esperandoSalida = Buffer(new Paquete(sucursal1, sucursal2,1, Normal), new Paquete(sucursal1, sucursal2,3, Normal))
		sucursal3.notificarPaquetesASalir(esperandoSalida)
		assert(sucursal3.capacidad == 6)
		var esperandoEntrada = Buffer(new Paquete(sucursal3, sucursal2,1, Normal), new Paquete(sucursal3, sucursal2,5, Normal))
		sucursal3.notificarPaquetesAEntrar(esperandoEntrada)
		assert(sucursal3.capacidad == 0)
	}
	
	it should "no poder agregarse mas paquetes" in {
	   intercept[SucursalSinCapacidad]{
	     var esperandoEntrada = Buffer(new Paquete(sucursal3, sucursal2,10, Normal), new Paquete(sucursal3, sucursal2,5, Normal))
		 sucursal3.notificarPaquetesAEntrar(esperandoEntrada)
	   }
	}
	
	"Mock de Sistema Externo" should "responder a consultas" in {

	  SistemaExterno.distanciaTerrestre  = 250.5
	  SistemaExterno.distanciaAerea = 1200.5
	  SistemaExterno.cantidadPeajes = 4
	  
	  assert(SistemaExterno.distanciaTerrestreEntre(sucursal1, sucursal2) == 250.5 )
	  assert(SistemaExterno.distanciaAereaEntre(sucursal1, sucursal2) == 1200.5)
	  assert(SistemaExterno.cantidadPeajesEntre(sucursal1, sucursal2) == 4)
	}
	
	"Un transporte" should "tener capacidad" in {

		var paquetesYaAsignados = Buffer(new Paquete(sucursal1, sucursal2,10, Normal), new Paquete(sucursal1, sucursal2,20, Normal))

		camion.asignarPaquetes(paquetesYaAsignados)
		assert(camion.pedidos.size == 2)
		assert(camion.capacidad == 15) //45 - 10 - 20 = 15

		var paquetesNuevos = Buffer(new Paquete(sucursal1, sucursal2,5, Normal), new Paquete(sucursal1, sucursal2,10,Normal))	
		camion.asignarPaquetes(paquetesNuevos)
		assert(camion.pedidos.size == 4)
		assert(camion.capacidad == 0) //45 - 10 - 20 - 5 - 10 = 0
	}
	
	it should "no tener capacidad" in {

	  	var paquetesYaAsignados = Buffer(new Paquete(sucursal1, sucursal2,10, Normal), new Paquete(sucursal1, sucursal2,20, Normal))
	  	
		camion.asignarPaquetes(paquetesYaAsignados)
		assert(camion.pedidos.size == 2)
		assert( camion.capacidad == 15) //45 - 10 - 20 = 15

		var paquetesNuevos = Buffer(new Paquete(sucursal1, sucursal2,5, Normal), new Paquete(sucursal1, sucursal1,15,Normal))	

		intercept[TransporteSinCapacidad]{
		  camion.asignarPaquetes(paquetesNuevos) //excedo la capacidad
		}
		assert(camion.capacidad == 15)
		assert(camion.pedidos.size == 2)
	}
	
	it should "no llevar paquetes de destinos diferentes" in {

		var paquetesYaAsignados = Buffer(new Paquete(sucursal1, sucursal2,10, Normal), new Paquete(sucursal1, sucursal1,20, Normal))
	  
		intercept[PaquetesDestinoErroneo] {
			camion.asignarPaquetes(paquetesYaAsignados) //asigno paquetes iniciales con destino distinto
	    }
	  
		assert(camion.pedidos.size == 0)
		paquetesYaAsignados = Buffer(new Paquete(sucursal1, sucursal2,10, Normal), new Paquete(sucursal1, sucursal2,20, Normal))
		
		camion.asignarPaquetes(paquetesYaAsignados)
		assert(camion.pedidos.size == 2)
		assert(camion.capacidad == 15) //45 - 10 - 20 = 15
		
		var paquetesNuevos = Buffer(new Paquete(sucursal1, sucursal2,5, Normal), new Paquete(sucursal1, sucursal1,10,Normal))	
		
		intercept[PaquetesDestinoErroneo] {
			camion.asignarPaquetes(paquetesNuevos) //asigno mas paquetes pero uno con destino distinto
		}
		
		assert(camion.capacidad == 15)
		assert(camion.pedidos.size == 2)

    paquetesNuevos = Buffer(new Paquete(sucursal1, sucursal2, 5, Normal), new Paquete(sucursal1, sucursal2, 10, Normal))	
		
		camion.asignarPaquetes(paquetesNuevos)
		assert(camion.pedidos.size == 4)
		assert(camion.capacidad == 0) //45 - 10 - 20 - 5 - 10 = 15
	}
	
	it should "calcular la ganancia de un envio" in {

	  camion.asignarPaquetes(paquetes)

	  SistemaExterno.distanciaTerrestre = 0.5
	  SistemaExterno.cantidadPeajes  = 2
	  
	  assert(camion.gananciaEnvio == 66)
	}
	
	it should "calcular la ganancia de un envio con distintos seguimientos" in {

	  camion.asignarPaquetes(paquetes)
	  camion.servicioExtra = Some(SeguimientoSatelital)
	  SistemaExterno.distanciaTerrestre = 0.5
	  SistemaExterno.cantidadPeajes  = 2
	  
	  assert(camion.gananciaEnvio == 65.5)//160 - (20 + 100*0.5 + 2*12 + 0.5)

	  camion.servicioExtra = Some(SeguimientoSatelitalConVideo)
	  
	  assert(camion.gananciaEnvio == 62.25999999999999)
	}
	
	"Un avion" should "no poder hacer viajes menor o igual a 1000 kilometros" in {
	  avion.asignarPaquetes(paquetes)
	  SistemaExterno.distanciaAerea = 900.0
	  
	  intercept[EnvioConDistanciaMenorA1000KM]{
	    avion.gananciaEnvio
	  }
	  
	  SistemaExterno.distanciaAerea = 1000.0
	  
	  intercept[EnvioConDistanciaMenorA1000KM]{
	    avion.gananciaEnvio
	  }	  
	  
	  SistemaExterno.distanciaAerea = 1100.0
	  
	  assert(avion.gananciaEnvio == -1099860)
	}
	
	"Las estadisticas" should "mostrar ganancia total de todos los transportes en analisis" in {
	  
	  estadisticas agregarTransporte(camion)
	  estadisticas agregarTransporte(furgoneta)
	  
	  cliente.generarPaquete(1, Normal)
	  cliente.generarPaquete(3, Normal)
	  cliente.generarPaquete(2, Normal)
	  cliente.pedirEnvio(camion)
	  
	  camion.hacerEnvio
	  
	  assert(estadisticas.gananciaTotalDeTodosLosTransportes == 210) //80 + 80 + 80 - 10 - 10 - 10 = 210
	  
	  cliente.generarPaquete(1, Normal)
	  cliente.generarPaquete(1, Normal)
	  cliente.generarPaquete(1, Normal)
	  cliente.pedirEnvio(camion)
	  
	  camion.hacerEnvio
	  
	  assert(estadisticas.gananciaTotalDeTodosLosTransportes == 420) 
	  
	  cliente.sucursalOrigen = sucursal3
	  cliente.sucursalDestino = sucursal1
	  cliente.generarPaquete(2, Normal)
	  cliente.generarPaquete(3, Normal)
	  cliente.generarPaquete(4, Normal)
	  cliente.pedirEnvio(furgoneta)
	  
	  furgoneta.hacerEnvio
	  
	  assert(estadisticas.gananciaTotalDeTodosLosTransportes == 630)
	}
}