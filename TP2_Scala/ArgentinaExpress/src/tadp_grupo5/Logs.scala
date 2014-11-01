package tadp_grupo5

import scala.collection.mutable.Queue

object Logs {
  
	var historicoEnvios : Queue[Envio] = Queue[Envio]()
	
	def registrarEnvio(envio : Envio) = historicoEnvios enqueue envio.deepCopy
	
}