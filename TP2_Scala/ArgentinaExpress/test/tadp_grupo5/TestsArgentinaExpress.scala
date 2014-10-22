package tadp_grupo5

import org.junit.Test
import org.junit.Assert

class TestsArgentinaExpress {
  
	@Test
	def sucursalTieneCapacidad(){
		var sucursal1 = new Sucursal(10)
		var sucursal2 = new Sucursal(20)
		var sucursal3 = new Sucursal(10)
		var esperandoSalida = Seq(new Paquete(sucursal1, sucursal2,1, Normal), new Paquete(sucursal1, sucursal2,3, Normal))
		var esperandoEntrada = Seq(new Paquete(sucursal3, sucursal2,1, Normal), new Paquete(sucursal3, sucursal2,5, Normal))
		sucursal3.notificarEnvios(esperandoSalida)
		sucursal3.notificarRecepcion(esperandoEntrada)
		Assert.assertEquals(sucursal3.capacidad, 0)
	}

	@Test
	def sucursalNoTieneCapacidad{
		var sucursal1 = new Sucursal(10)
		var sucursal2 = new Sucursal(20)
		var sucursal3 = new Sucursal(10)
		
		var esperandoSalida = Seq(new Paquete(sucursal1, sucursal2,6, Normal), new Paquete(sucursal1, sucursal2,4, Normal))
		var esperandoEntrada = Seq(new Paquete(sucursal3, sucursal2,1, Normal), new Paquete(sucursal3, sucursal2,5, Normal))
		sucursal3.notificarEnvios(esperandoSalida)
		Assert.assertEquals(0, sucursal3.capacidad) // 10 - 6 - 4 = 0
		
		try {
			sucursal3.notificarRecepcion(esperandoEntrada)
		} catch {
		case SucursalSinCapacidad() => {
		  Assert.assertEquals(0, sucursal3.capacidad)
		}
		}
		
		Assert.assertEquals(0, sucursal3.capacidad)
		Assert.assertEquals(0, sucursal3.paquetesEnEntrar.size)
	}
	
	@Test
	def transporteTieneCapacidad(){
		var sucursal1 = new Sucursal(10)
		var sucursal2 = new Sucursal(20)
		var paquetesYaAsignados = Seq(new Paquete(sucursal1, sucursal2,10, Normal), new Paquete(sucursal1, sucursal2,20, Normal))
		
		var camion = new Camion()
		camion.asignarPaquetes(paquetesYaAsignados)
		Assert.assertEquals(2, camion.pedidos.size)
		Assert.assertEquals(15, camion.capacidad) //45 - 10 - 20 = 15
		
		var paquetesNuevos = Seq(new Paquete(sucursal1, sucursal2,5, Normal), new Paquete(sucursal1, sucursal1,10,Normal))	
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
	def mockSistemaExternoDevuelveDistancias {
	  
	  object SistemaExterno extends CalculadorDistancia {
	     override def distanciaTerrestreEntre(sucursal1: Sucursal, sucursal2: Sucursal): Double = {
	       250.5
	     }
	     
	     override def distanciaAereaEntre(sucursal1: Sucursal, sucursal2: Sucursal): Double = {
	       200.5
	     }
	     
	     override def cantidadPeajesEntre(sucursal1: Sucursal, sucursal2: Sucursal): Int = {
	       4
	     }
	  }
	  
	  var sucursal1 = new Sucursal(10)
	  var sucursal2 = new Sucursal(20)	 	  
	  
	  Assert.assertEquals(250.5, SistemaExterno.distanciaTerrestreEntre(sucursal1, sucursal2), 0)
	  Assert.assertEquals(200.5, SistemaExterno.distanciaAereaEntre(sucursal1, sucursal2), 0)
	  Assert.assertEquals(4, SistemaExterno.cantidadPeajesEntre(sucursal1, sucursal2))
	}
}