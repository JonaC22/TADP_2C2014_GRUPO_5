class PrototypedObject
  def set_property nombre_atributo, valor
    instance_variable_set("@#{nombre_atributo}", valor) #define el atributo y le setea el valor
    self.singleton_class.send(:attr_accessor, nombre_atributo) #crea los getters y setters del atributo para solo la instancia del objeto
  end

  def set_method nombre_metodo, accion
    define_singleton_method(nombre_metodo,accion)
  end
end