require 'ostruct'


module Conversiones

  def poner_igual una_prop
    x = quitar_arroba(una_prop)
    x = x.to_s+"="
    return x.to_sym
  end

  def quitar_arroba prop
    sin_arroba = prop.to_s.delete "@"
    return sin_arroba.to_sym
  end

  def este_metodo_es_un_attr(un_metodo)
    self.instance_variables.any? {|una_prop| (self.quitar_arroba(una_prop) == un_metodo) or (self.poner_igual(una_prop) == un_metodo) }
  end


end


module Observable

  def agregar_a_lista_de_procs(nombre,procedimiento)
   m = OpenStruct.new({:name => nombre,:accion => procedimiento})
   self.procs << m
  end

end



class PrototypedObject

  include Conversiones
  include Observable

  attr_accessor :interesados, # Todas las instancias de esta clase pueden proveer prototipos a otros objetos
                :procs        #Lista de procedimientos . No estan bindeados, son los procs puros
  def initialize
     @interesados = []
     @procs = []
  end


  def set_method nombre_metodo, accion
    self.agregar_a_lista_de_procs(nombre_metodo,accion)
    define_singleton_method(nombre_metodo,accion)
    self.interesados.each { |un_interesado| un_interesado.set_method(nombre_metodo,accion)} #Si tiene prototipos le agrega los metodos
  end



  def set_property nombre_atributo, valor
    instance_variable_set("@#{nombre_atributo}", valor) #define el atributo y le setea el valor
    self.singleton_class.send(:attr_accessor, nombre_atributo) #crea los getters y setters del atributo para solo la instancia del objeto
    self.interesados.each { |un_interesado| un_interesado.set_property(nombre_atributo,valor) } #Si tiene prototipos le agrega los atributos
  end




  def set_prototype un_prototipo

    un_prototipo.procs.each { |proc| self.set_method(proc.name,proc.accion)}

    un_prototipo.instance_variables.each { |una_property|
      if (not(self.instance_variable_defined?(una_property)))
        prop_sin_arroba = self.quitar_arroba(una_property)
        self.set_property(prop_sin_arroba.to_sym,nil)
      end
    }

    un_prototipo.interesados << self
  end

end