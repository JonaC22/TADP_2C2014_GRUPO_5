package tadp_grupo5
import java.util.Date


//interfaz con el sistema externo
trait CalculadorDistancia {
	def distanciaTerrestreEntre(sucursal1: Sucursal, sucursal2: Sucursal): Double
	def distanciaAereaEntre(sucursal1: Sucursal, sucursal2: Sucursal): Double
	def cantidadPeajesEntre(sucursal1: Sucursal, sucursal2: Sucursal): Int
	var fechaActual : Date
}