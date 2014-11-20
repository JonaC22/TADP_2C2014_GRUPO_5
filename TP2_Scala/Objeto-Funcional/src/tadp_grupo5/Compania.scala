package tadp_grupo5

class Compania {
	var sucursales : List[Sucursal] = List()
	
	def agregarSucursal(sucursal : Sucursal) {
	  sucursales = sucursales :+ sucursal
	}
}