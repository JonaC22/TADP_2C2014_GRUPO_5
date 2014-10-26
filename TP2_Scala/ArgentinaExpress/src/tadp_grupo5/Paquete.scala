package tadp_grupo5

class Paquete (val sucursalOrigen : Sucursal, val sucursalDestino : Sucursal, val volumen : Int, val caracteristica : Caracteristica ) {
	def costoBase : Int = caracteristica.costoBase
}