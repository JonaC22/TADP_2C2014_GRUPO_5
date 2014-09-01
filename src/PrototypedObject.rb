class PrototypedObject
  def set_property symbol, valor
      instance_variable_set("@#{symbol}", valor) #define el atributo y le setea el valor
      self.singleton_class.send(:attr_accessor, symbol) #crea los getters y setters del atributo para solo la instancia del objeto
  end
end