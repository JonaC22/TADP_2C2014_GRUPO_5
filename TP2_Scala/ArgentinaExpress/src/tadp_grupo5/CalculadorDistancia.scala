package tadp_grupo5

//interfaz con el sistema externo
trait CalculadorDistancia {
	def distanciaTerrestreEntre(sucursal1: Sucursal, sucursal2: Sucursal): Double
	def distanciaAereaEntre(sucursal1: Sucursal, sucursal2: Sucursal): Double
	def cantidadPeajesEntre(sucursal1: Sucursal, sucursal2: Sucursal): Int
}