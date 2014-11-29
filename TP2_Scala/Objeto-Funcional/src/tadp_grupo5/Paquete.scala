package tadp_grupo5

case class Paquete (val sucursalOrigen : Sucursal, val sucursalDestino : Sucursal, val volumen : Int, val caracteristica : Caracteristica ) {
	def costo: Double = caracteristica.costo
	def precio: Double = caracteristica.precio
}