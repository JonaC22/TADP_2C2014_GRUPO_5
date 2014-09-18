require 'ostruct'


module Commons

  def poner_igual una_prop
   (quitar_arroba(una_prop).to_s+"=").to_sym
  end

  def quitar_arroba prop
    (prop.to_s.delete "@").to_sym
  end

  def este_metodo_es_un_attr(un_metodo)
    self.instance_variables.any? {|una_prop| (self.quitar_arroba(una_prop) == un_metodo) or (self.poner_igual(una_prop) == un_metodo) }
  end

  def metodo_ya_definido(un_metodo)
    self.singleton_methods.include? un_metodo
  end

  def es_un_attr_privado(atributo)
    atributo == :@interesados || atributo == :@procs || atributo == :@prototipos
  end


end

module Observable

  include Commons

  def agregar_a_lista_de_procs(nombre,procedimiento)
     m = OpenStruct.new({:name => nombre,:accion => procedimiento})
     self.procs << m
  end

  def agregar_metodo_a_interesados(nombre_metodo,accion)
    self.interesados.each { |un_interesado| unless un_interesado.metodo_ya_definido(nombre_metodo)
                                                     un_interesado.set_method(nombre_metodo,accion)
                                            end
                         }
  end

  def agregar_property_a_interesados(nombre_atributo)
    self.interesados.each { |un_interesado| un_interesado.set_property(nombre_atributo,nil) }
  end

  def setear_metodos_del_prototipo(un_prototipo)
  un_prototipo.procs.each { |proc| unless self.metodo_ya_definido(proc)
                                     self.set_method(proc.name,proc.accion)
                                   end
  }
  end

  def setear_propertys_del_prototipo(un_prototipo)
    un_prototipo.instance_variables.each { |una_property|
      unless  self.instance_variable_defined?(una_property)
        self.set_property(self.quitar_arroba(una_property),nil)
      end
    }
  end

  def copiar_estado()
    self.prototipo.instance_variables.each { |atributo|
      unless self.es_un_attr_privado(atributo)
        instance_variable_set(atributo, prototipo.instance_variable_get(atributo))
      end
    }
  end

end

module Prototyped
  def set_method nombre_metodo, accion
    self.agregar_a_lista_de_procs(nombre_metodo,accion)
    define_singleton_method(nombre_metodo,accion)
    self.agregar_metodo_a_interesados(nombre_metodo,accion)

  end

  def set_property nombre_atributo, valor
    instance_variable_set("@#{nombre_atributo}", valor) #define el atributo y le setea el valor
    self.singleton_class.send(:attr_accessor, nombre_atributo) #crea los getters y setters del atributo para solo la instancia del objeto
    self.agregar_property_a_interesados(nombre_atributo) #Si tiene prototipos le agrega los atributos
  end


  def set_prototype un_prototipo
    self.setear_metodos_del_prototipo(un_prototipo)
    self.setear_propertys_del_prototipo(un_prototipo)
    un_prototipo.interesados << self
    self.prototipo =  un_prototipo
  end
end

class PrototypedObject

  include Commons
  include Observable
  include Prototyped

  attr_accessor :interesados, # Todas las instancias de esta clase pueden proveer prototipos a otros objetos
                :procs,        #Lista de procedimientos . No estan bindeados, son los procs puros
                :prototipo,
                :extension
  def initialize
     @interesados = []
     @procs = []
     @extension = nil
  end

  def new(*mapa)
    if mapa.empty?
        self.copiar_estado()
        self
    else
      unless self.extension != nil
        atributos = mapa[0].keys
        valores = mapa[0].values
        atributos.each { |un_atributo| self.instance_variable_set("@#{un_atributo}", valores.shift) }
      else
        atributos_extension = mapa.shift
        valores = atributos_extension.values
        atributos_extension.keys.each { |un_atributo| self.instance_variable_set("@#{un_atributo}", valores.shift) }
        mapa.unshift(self)
        extension.call(mapa)
        self
      end
      self
    end

  end

  def extended &bloque
    extendido = PrototypedObject.new
    extendido.set_prototype(self.prototipo)
    extendido.extension = bloque
    extendido
  end
end

#A partir de aca esta incompleto
class PrototypedConstructor
  def PrototypedConstructor.new(prototipo)
    nuevo = PrototypedObject.new
    nuevo.set_prototype prototipo
    nuevo
  end

  def PrototypedConstructor.copy(prototipo)
     PrototypedConstructor.new(prototipo)
  end



end

