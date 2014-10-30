package tadp_grupo5

import org.scalatest._


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

	var sucursal1 = new Sucursal(10)
	var sucursal2 = new Sucursal(20)
    var sucursal3 = new Sucursal(10)
	
	after{
	  sucursal3.paquetesEnEntrar = Nil
	  sucursal3.paquetesEnSalir = Nil
	}
    
	"Una sucursal" should "tener capacidad" in {
		var esperandoSalida = Seq(new Paquete(sucursal1, sucursal2,1, Normal), new Paquete(sucursal1, sucursal2,3, Normal))
		sucursal3.notificarEnvios(esperandoSalida)
		assert(sucursal3.capacidad == 6) //10 - 1 - 3 = 6
	}				

	it should " poder agregarse mas paquetes" in {
	    var esperandoSalida = Seq(new Paquete(sucursal1, sucursal2,1, Normal), new Paquete(sucursal1, sucursal2,3, Normal))
		sucursal3.notificarEnvios(esperandoSalida)
		assert(sucursal3.capacidad == 6)
		var esperandoEntrada = Seq(new Paquete(sucursal3, sucursal2,1, Normal), new Paquete(sucursal3, sucursal2,5, Normal))
		sucursal3.notificarRecepcion(esperandoEntrada)
		assert(sucursal3.capacidad == 0)
	}
	
	it should "no poder agregarse mas paquetes" in {
	   intercept[SucursalSinCapacidad]{
	     var esperandoEntrada = Seq(new Paquete(sucursal3, sucursal2,10, Normal), new Paquete(sucursal3, sucursal2,5, Normal))
		 sucursal3.notificarRecepcion(esperandoEntrada)
	   }
	}
	
	"Mock de Sistema Externo" should "responder a consultas" in {

	  SistemaExterno.distanciaTerrestre  = 250.5
	  SistemaExterno.distanciaAerea = 200.5
	  SistemaExterno.cantidadPeajes = 4
	  
	  assert(SistemaExterno.distanciaTerrestreEntre(sucursal1, sucursal2) == 250.5 )
	  assert(SistemaExterno.distanciaAereaEntre(sucursal1, sucursal2) == 200.5)
	  assert(SistemaExterno.cantidadPeajesEntre(sucursal1, sucursal2) == 4)
	}
}

/*
class TestsArgentinaExpress {

  //fixture
	
	@Test
	def transporteTieneCapacidad(){
		var sucursal1 = new Sucursal(10)
		var sucursal2 = new Sucursal(20)
		var paquetesYaAsignados = Seq(new Paquete(sucursal1, sucursal2,10, Normal), new Paquete(sucursal1, sucursal2,20, Normal))
		
		var camion = new Camion()
		camion.asignarPaquetes(paquetesYaAsignados)
		Assert.assertEquals(2, camion.pedidos.size)
		Assert.assertEquals(15, camion.capacidad) //45 - 10 - 20 = 15
		
		var paquetesNuevos = Seq(new Paquete(sucursal1, sucursal2,5, Normal), new Paquete(sucursal1, sucursal2,10,Normal))	
		camion.asignarPaquetes(paquetesNuevos)
		Assert.assertEquals(4, camion.pedidos.size)
		Assert.assertEquals(0, camion.capacidad) //45 - 10 - 20 - 5 - 10 = 0
	}

	@Test
	def transporteNoTieneCapacidad{
		var sucursal1 = new Sucursal(10)
		var sucursal2 = new Sucursal(20)
		var paquetesYaAsignados = Seq(new Paquete(sucursal1, sucursal2,10, Normal), new Paquete(sucursal1, sucursal2,20, Normal))
		
		var camion = new Camion()
		camion.asignarPaquetes(paquetesYaAsignados)
		Assert.assertEquals(2, camion.pedidos.size)
		Assert.assertEquals(15, camion.capacidad) //45 - 10 - 20 = 15
		
		var paquetesNuevos = Seq(new Paquete(sucursal1, sucursal2,5, Normal), new Paquete(sucursal1, sucursal1,15,Normal))	
		
		try {
			camion.asignarPaquetes(paquetesNuevos) //excedo la capacidad
		} catch {
		case TransporteSinCapacidad() => {
		  Assert.assertEquals(15,camion.capacidad)
		}
		}
		Assert.assertEquals(2, camion.pedidos.size)
	}
	
	
	
	@Test
	def transporteNoLlevaPaquetesDeDestinosDiferentes{
		var sucursal1 = new Sucursal(10)
		var sucursal2 = new Sucursal(20)
		var paquetesYaAsignados = Seq(new Paquete(sucursal1, sucursal2,10, Normal), new Paquete(sucursal1, sucursal1,20, Normal))
		
		var camion = new Camion()
		try {
			camion.asignarPaquetes(paquetesYaAsignados) //asigno paquetes iniciales con destino distinto
		} catch {
		  case PaquetesDestinoErroneo() => {
		    Assert.assertEquals(0, camion.pedidos.size)
		  }
		}
		
		paquetesYaAsignados = Seq(new Paquete(sucursal1, sucursal2,10, Normal), new Paquete(sucursal1, sucursal2,20, Normal))
		
		camion.asignarPaquetes(paquetesYaAsignados)
		Assert.assertEquals(2, camion.pedidos.size)
		Assert.assertEquals(15, camion.capacidad) //45 - 10 - 20 = 15
		
		var paquetesNuevos = Seq(new Paquete(sucursal1, sucursal2,5, Normal), new Paquete(sucursal1, sucursal1,10,Normal))	
		
		try {
			camion.asignarPaquetes(paquetesNuevos) //asigno mas paquetes pero uno con destino distinto
		} catch {
		case PaquetesDestinoErroneo() => {
		  Assert.assertEquals(15,camion.capacidad)
		}
		}
		
		Assert.assertEquals(2, camion.pedidos.size)
		
		paquetesNuevos = Seq(new Paquete(sucursal1, sucursal2,5, Normal), new Paquete(sucursal1, sucursal2,10,Normal))	
		
		camion.asignarPaquetes(paquetesNuevos)
		Assert.assertEquals(4, camion.pedidos.size)
		Assert.assertEquals(0, camion.capacidad) //45 - 10 - 20 - 5 - 10 = 15
	}
}
*/