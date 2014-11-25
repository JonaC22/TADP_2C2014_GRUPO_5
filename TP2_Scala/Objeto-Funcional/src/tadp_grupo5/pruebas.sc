package tadp_grupo5
import java.util.Date

object pruebas {

		object SistemaExterno extends CalculadorDistancia {
		var distanciaTerrestre : Double = 0.0
		var distanciaAerea : Double = 0.0
		var cantidadPeajes : Int = 0
		var fechaActual : Date = new Date()

		override def distanciaTerrestreEntre(sucursal10: Sucursal, sucursal20: Sucursal): Double = {
			distanciaTerrestre
		}

		override def distanciaAereaEntre(sucursal10: Sucursal, sucursal20: Sucursal): Double = {
			distanciaAerea
		}

		override def cantidadPeajesEntre(sucursal10: Sucursal, sucursal20: Sucursal): Int = {
			cantidadPeajes
		}
	}
	
  val sucursal10 = new Sucursal(10, "Argentina")  //> sucursal10  : tadp_grupo5.Sucursal = Sucursal(10,Argentina)
	val sucursal20 = new Sucursal(20, "Argentina")
                                                  //> sucursal20  : tadp_grupo5.Sucursal = Sucursal(20,Argentina)
    val sucursal30 = new Sucursal(30, "Uruguay")  //> sucursal30  : tadp_grupo5.Sucursal = Sucursal(30,Uruguay)
	val sucursal1000 = new Sucursal(1000, "Brasil")
                                                  //> sucursal1000  : tadp_grupo5.Sucursal = Sucursal(1000,Brasil)
	val sucursal2000 = new Sucursal(2000, "Brasil")
                                                  //> sucursal2000  : tadp_grupo5.Sucursal = Sucursal(2000,Brasil)
    val sucursal3000 = new Sucursal(3000, "Argentina")
                                                  //> sucursal3000  : tadp_grupo5.Sucursal = Sucursal(3000,Argentina)
	val casaCentral = new CasaCentral(20, "Brasil")
                                                  //> casaCentral  : tadp_grupo5.CasaCentral = Sucursal(20,Brasil)
	val sucursales = List(sucursal10, sucursal20, sucursal30, sucursal1000, sucursal2000, sucursal3000, casaCentral)
                                                  //> sucursales  : List[tadp_grupo5.Sucursal] = List(Sucursal(10,Argentina), Suc
                                                  //| ursal(20,Argentina), Sucursal(30,Uruguay), Sucursal(1000,Brasil), Sucursal(
                                                  //| 2000,Brasil), Sucursal(3000,Argentina), Sucursal(20,Brasil))
	
	val flechaBus = new Compania()            //> flechaBus  : tadp_grupo5.Compania = tadp_grupo5.Compania@726b37ad
	val chevallier = new Compania()           //> chevallier  : tadp_grupo5.Compania = tadp_grupo5.Compania@5d14798a
	val companias = List(flechaBus, chevallier)
                                                  //> companias  : List[tadp_grupo5.Compania] = List(tadp_grupo5.Compania@726b37a
                                                  //| d, tadp_grupo5.Compania@5d14798a)
	
	val cliente = new Cliente(sucursal1000, sucursal3000)
                                                  //> cliente  : tadp_grupo5.Cliente = tadp_grupo5.Cliente@7038ce7b
	  
	val estadisticas = new Estadisticas()     //> estadisticas  : tadp_grupo5.Estadisticas = tadp_grupo5.Estadisticas@354fed2
                                                  //| 
	
	val camion = new Camion(SistemaExterno)   //> camion  : tadp_grupo5.Camion = Camion(tadp_grupo5.pruebas$SistemaExterno$@7
                                                  //| f3f6786)
	val otroCamion = new Camion(SistemaExterno)
                                                  //> otroCamion  : tadp_grupo5.Camion = Camion(tadp_grupo5.pruebas$SistemaExtern
                                                  //| o$@7f3f6786)
	val avion = new Avion(SistemaExterno)     //> avion  : tadp_grupo5.Avion = Avion(tadp_grupo5.pruebas$SistemaExterno$@7f3f
                                                  //| 6786)
	val furgoneta = new Furgoneta(SistemaExterno)
                                                  //> furgoneta  : tadp_grupo5.Furgoneta = Furgoneta(tadp_grupo5.pruebas$SistemaE
                                                  //| xterno$@7f3f6786)
	val transportes = List(camion, otroCamion, avion, furgoneta)
                                                  //> transportes  : List[tadp_grupo5.Transporte with Product with Serializable] 
                                                  //| = List(Camion(tadp_grupo5.pruebas$SistemaExterno$@7f3f6786), Camion(tadp_gr
                                                  //| upo5.pruebas$SistemaExterno$@7f3f6786), Avion(tadp_grupo5.pruebas$SistemaEx
                                                  //| terno$@7f3f6786), Furgoneta(tadp_grupo5.pruebas$SistemaExterno$@7f3f6786))

	val paquete1 = new Paquete(sucursal10, sucursal20,1, Normal)
                                                  //> paquete1  : tadp_grupo5.Paquete = tadp_grupo5.Paquete@5fdb7adc
	val paqueteInvertido1 = new Paquete(sucursal20, sucursal10,1, Normal)
                                                  //> paqueteInvertido1  : tadp_grupo5.Paquete = tadp_grupo5.Paquete@425f32ae
	val paquete2 = new Paquete(sucursal10, sucursal20,2, Normal)
                                                  //> paquete2  : tadp_grupo5.Paquete = tadp_grupo5.Paquete@77f85f8c
	val paquete5 =new Paquete(sucursal30, sucursal20,5, Normal)
                                                  //> paquete5  : tadp_grupo5.Paquete = tadp_grupo5.Paquete@53491ddc
	val paquete10 = new Paquete(sucursal10, sucursal20,10, Normal)
                                                  //> paquete10  : tadp_grupo5.Paquete = tadp_grupo5.Paquete@6547813b
	val paquete10CasaCentral = new Paquete(sucursal10, casaCentral,10, Normal)
                                                  //> paquete10CasaCentral  : tadp_grupo5.Paquete = tadp_grupo5.Paquete@39757c6f
	val paquete20 = new Paquete(sucursal10, sucursal20,20, Normal)
                                                  //> paquete20  : tadp_grupo5.Paquete = tadp_grupo5.Paquete@503c78e1
	val paquete50nacional = new Paquete(sucursal1000, sucursal2000, 50, Normal)
                                                  //> paquete50nacional  : tadp_grupo5.Paquete = tadp_grupo5.Paquete@322424ee
	val paquete50internacional = new Paquete(sucursal1000, sucursal3000, 50, Normal)
                                                  //> paquete50internacional  : tadp_grupo5.Paquete = tadp_grupo5.Paquete@62f49ef
                                                  //| 9
	val paqueteConMuchoVolumen = new Paquete(sucursal10, sucursal20, 9999, Normal)
                                                  //> paqueteConMuchoVolumen  : tadp_grupo5.Paquete = tadp_grupo5.Paquete@29f1203
                                                  //| 0
	val paqueteUrgenteLiviano = new Paquete(sucursal10, sucursal20,1, Urgente)
                                                  //> paqueteUrgenteLiviano  : tadp_grupo5.Paquete = tadp_grupo5.Paquete@157844d3
                                                  //| 
	val paqueteUrgentePesado = new Paquete(sucursal10, sucursal20,20, Urgente)
                                                  //> paqueteUrgentePesado  : tadp_grupo5.Paquete = tadp_grupo5.Paquete@aee83a8
	val paqueteConRefrigeracion = new Paquete(sucursal10, sucursal20,10, NecesitaRefrigeracion)
                                                  //> paqueteConRefrigeracion  : tadp_grupo5.Paquete = tadp_grupo5.Paquete@310590
                                                  //| 33
	val paqueteFragil = new Paquete(sucursal10, sucursal20,2, Fragil)
                                                  //> paqueteFragil  : tadp_grupo5.Paquete = tadp_grupo5.Paquete@643e1d69
	 
	 
  var camion2 : Camion = camion.asignarPaquete(paquete1)
                                                  //> camion2  : tadp_grupo5.Camion = Camion(tadp_grupo5.pruebas$SistemaExterno$@
                                                  //| 7f3f6786)
  
  camion.pedidos.size                             //> res0: Int = 0
  camion2.pedidos.size                            //> res1: Int = 1
}