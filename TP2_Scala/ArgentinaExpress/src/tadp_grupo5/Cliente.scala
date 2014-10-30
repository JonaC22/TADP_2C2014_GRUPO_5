package tadp_grupo5

class Cliente {

	def generarEnvio(sucursalOrigen : Sucursal, sucursalDestino : Sucursal, volumenPaquete : Int, caracteristicaDePaquete : Caracteristica) : Unit = {
	  Seq(new Paquete(sucursalOrigen, sucursalDestino, volumenPaquete, caracteristicaDePaquete))
	  sucursalOrigen.notificarEnvios(Seq(new Paquete(sucursalOrigen, sucursalDestino, volumenPaquete, caracteristicaDePaquete)))
	  sucursalDestino.notificarRecepcion(Seq(new Paquete(sucursalOrigen, sucursalDestino, volumenPaquete, caracteristicaDePaquete)))
	}
}