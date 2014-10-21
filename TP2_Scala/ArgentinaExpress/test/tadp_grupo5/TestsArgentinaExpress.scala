package tadp_grupo5

import org.junit.Test
import org.junit.Assert

class TestsArgentinaExpress {
  
	@Test
	def testSucursalTieneCapacidad(){
		var sucursal1 = new Sucursal(10)
		var sucursal2 = new Sucursal(20)
		var sucursal3 = new Sucursal(10)
		var esperandoSalida = Set(new Paquete(sucursal1, sucursal2,1, Normal), new Paquete(sucursal1, sucursal2,2, Normal))
		var esperandoEntrada = Set(new Paquete(sucursal3, sucursal2,1, Normal), new Paquete(sucursal3, sucursal2,5, Normal))
		sucursal3.notificarEnvios(esperandoSalida)
		sucursal3.notificarRecepcion(esperandoEntrada)
		Assert.assertEquals(sucursal3.volumenTotal, 1)
	}

	@Test
	def testSucursalNoTieneCapacidad{
		var sucursal1 = new Sucursal(10)
		var sucursal2 = new Sucursal(20)
		var sucursal3 = new Sucursal(10)
		try {
			var esperandoSalida = Set(new Paquete(sucursal1, sucursal2,6, Normal), new Paquete(sucursal1, sucursal2,4, Normal))
			var esperandoEntrada = Set(new Paquete(sucursal3, sucursal2,1, Normal), new Paquete(sucursal3, sucursal2,5, Normal))
			sucursal3.notificarEnvios(esperandoSalida)
			sucursal3.notificarRecepcion(esperandoEntrada)
			sucursal3.volumenTotal
		} catch {
		case SucursalSinCapacidad() => {
		  Assert.assertTrue(true)
		}
		}
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