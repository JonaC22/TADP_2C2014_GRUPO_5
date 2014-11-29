package tadp_grupo5


case class Compania() {
	var sucursales : List[Sucursal] = List()
	
	def agregarSucursal(sucursal : Sucursal) {
	  sucursales = sucursales :+ sucursal
	}
}