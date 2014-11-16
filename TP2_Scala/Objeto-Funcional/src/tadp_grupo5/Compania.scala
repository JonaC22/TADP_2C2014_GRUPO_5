package tadp_grupo5

import scala.collection.mutable.Buffer

class Compania {
	var sucursales : List[Sucursal] = List()
	
	def agregarSucursal(sucursal : Sucursal) {
	  sucursales = sucursales :+ sucursal
	}
}