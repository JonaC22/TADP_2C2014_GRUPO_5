require 'ostruct'


module Commons

  def poner_igual una_prop
    (quitar_arroba(una_prop).to_s+"=").to_sym
  end

  def quitar_arroba prop
    (prop.to_s.delete "@").to_sym
  end

  def este_metodo_es_un_attr(un_metodo)
    self.instance_variables.any? { |una_prop| (self.quitar_arroba(una_prop) == un_metodo) or (self.poner_igual(una_prop) == un_metodo) }
  end

  def metodo_ya_definido(un_metodo)
    self.singleton_methods.include? un_metodo
  end

  def es_un_attr_privado(atributo)
    atributo == :@interesados || atributo == :@procs || atributo == :@prototypes
  end

  def metodo_anterior
    caller_locations(1,1)[0].label.to_sym
  end

end



module Observable

  include Commons

  def agregar_a_lista_de_procs(nombre, procedimiento)
    m = OpenStruct.new({:name => nombre, :accion => procedimiento})
    self.procs << m
  end

  def agregar_metodo_a_interesados(nombre_metodo, accion)
    self.interesados.each { |un_interesado|
      unless un_interesado.metodo_ya_definido(nombre_metodo)
        un_interesado.set_method(nombre_metodo, accion)
      end
    }
  end

  def agregar_property_a_interesados(nombre_atributo)
    self.interesados.each { |un_interesado| un_interesado.set_property(nombre_atributo, nil) }
  end

  def setear_metodos_del_prototipo(un_prototipo)
    un_prototipo.procs.each {
        |proc|
      unless self.metodo_ya_definido(proc)
        self.set_method(proc.name, proc.accion)
      end
    }
  end

  def setear_propertys_del_prototipo(un_prototipo)
    un_prototipo.instance_variables.each {
        |una_property|
      unless  self.instance_variable_defined?(una_property)
        self.set_property(self.quitar_arroba(una_property), nil)
      end
    }
  end

end

module Prototyped

  attr_accessor :stack_called_methods

  def initialize
    @stack_called_methods = []
  end

  def set_method nombre_metodo, accion
    self.agregar_a_lista_de_procs(nombre_metodo, wrappear_bloque(nombre_metodo, &accion))
    define_singleton_method(nombre_metodo, wrappear_bloque(nombre_metodo, &accion))
    self.agregar_metodo_a_interesados(nombre_metodo, wrappear_bloque(nombre_metodo, &accion))
  end

  def set_property nombre_atributo, valor
    instance_variable_set("@#{nombre_atributo}", valor) #define el atributo y le setea el valor
    self.singleton_class.send(:attr_reader, nombre_atributo) #crea los getters y setters del atributo para solo la instancia del objeto
    self.agregar_property_a_interesados(nombre_atributo) #Si tiene prototipos le agrega los atributos
  end


  def set_prototype un_prototipo
    self.setear_propertys_del_prototipo(un_prototipo)
    un_prototipo.interesados << self
    self.prototypes << un_prototipo
  end

  def set_prototypes protos
    protos.each { |proto| self.set_prototype(proto)}

  end

  def method_missing(simbolo, *argumentos, &bloque)
    alguien = self.quien_entiende_metodo simbolo,argumentos.length
    set_proc_as_method(argumentos, simbolo) if(!alguien.nil? && argumentos[0].is_a?(Proc) && self.hasEquals?(simbolo))
    if !alguien.nil?
      block = obtener_bloque(alguien, simbolo).accion
      self.instance_exec(*argumentos, &block)
    else #si no estoy redefiniendo nada, defino atributo o metodo dinamicamente
      if argumentos.at(0).is_a?(Comparable) && self.hasEquals?(simbolo)
        set_attribute(argumentos, simbolo)
      elsif argumentos.at(0).is_a?(Proc) && self.hasEquals?(simbolo)
          set_proc_as_method(argumentos, simbolo)
          if(self.prototypes.detect {|proto| proto.respond_to? splitEquals(simbolo)}) #si el prototipo asignado entiende el mensaje, es porque lo estoy redefiniendo
           # @stack_called_methods.push splitEquals(simbolo) #agrego a la lista de metodos que voy usar con call_next
          end
      else
        super
      end
    end
  end

  def obtener_bloque(prototipo, simbolo)
    un_proc = detectar_quien_entiende_mensaje(prototipo, simbolo)
    if un_proc.nil?
      prototipo.prototypes.each {
          |un_prototipo_de_prototipo|
        un_proc = obtener_bloque(un_prototipo_de_prototipo, simbolo)
        break if !un_proc.nil?
      }
    end
    un_proc
  end

  def detectar_quien_entiende_mensaje(prototipo, simbolo)
    prototipo.procs.detect { |proc| proc.name.to_sym == simbolo }
  end

  def hasEquals?(simbolo)
    simbolo.to_s.reverse[0] == "="
  end

  def wrappear_bloque(nombre_metodo, *args, &block)
    Proc.new {|*args|
      @stack_called_methods.push splitEquals(nombre_metodo)
      self.instance_exec(*args, &block) if block_given?
    }
  end

  def set_proc_as_method(argumentos, simbolo)
    self.set_method(splitEquals(simbolo), *argumentos)
  end

  def set_attribute(argumentos, simbolo)
    self.set_property(splitEquals(simbolo), *argumentos)
  end

  def quien_entiende_metodo simbolo, cantidad_argumentos
    self.prototypes.detect {
        |proto|
        proto.respond_to?(simbolo) &&
        self.aridad_correcta?(cantidad_argumentos, proto, simbolo)
    }
  end

  def aridad_correcta?(cantidad_argumentos, proto, simbolo)
    aridad = proto.method(splitEquals(simbolo)).arity.abs
    (simbolo.to_s.reverse[0] == "=" || cantidad_argumentos == 0) ? true : aridad == cantidad_argumentos
  end

  def splitEquals(simbolo)
    simbolo.to_s.split("=").at(0).to_sym
  end

  def respond_to_missing?(method_name, include_private = false)
    objeto_que_entiende_mensaje = self.prototypes.detect {|prototype| prototype.respond_to? method_name }
    (!objeto_que_entiende_mensaje.nil?) || super
  end

end

class PrototypedObject

  include Commons
  include Observable
  include Prototyped

  attr_accessor :interesados, # Todas las instancias de esta clase pueden proveer prototipos a otros objetos
                :procs, #Lista de procedimientos . No estan bindeados, son los procs puros
                :prototypes,
                :metodos_prototipados

  def initialize &block
    super
    self.interesados = []
    self.procs = []
    self.prototypes = []
    self.instance_eval &block if block_given?
  end

  def call_next
    simbolo = @stack_called_methods.last
    prototipo_asignado = self.prototypes.detect {|proto| proto.respond_to?(simbolo)}
    prototipo_asignado.send(stack_called_methods.last)
  end

end

class PrototypedConstructor
  include Commons
  include Observable

  attr_accessor :prototype, :proc_inicializacion, :block_inicializacion, :block_properties

  def initialize(prototipo, *args, &block)
    self.prototype = prototipo
    if (block && !args[0]) #inicializacion con bloque
      @block_inicializacion = block
    end
    if(!block && args[0]) #inicializacion simple con proc pasado por parametro
      @proc_inicializacion = args[0]
    end
  end

  def self.copy(prototipo)
    PrototypedConstructorCopy.new prototipo
  end

  def extended &bloque
    PrototypedConstructorExtended.new self.prototype, &bloque
  end

  def self.create &block
    prototipo = PrototypedObject.new
    prototipo.instance_eval &block
    instancia = PrototypedConstructor.new(prototipo)
    instancia
  end

  def with &block
    self.block_properties = block
    self
  end

  def with_properties properties
    instancia = PrototypedConstructorProperties.new(self.prototype)
    instancia.properties = properties
    instancia
  end

  def new *args
    nuevo = PrototypedObject.new
    nuevo.set_prototype self.prototype
    self.proc_inicializacion.call(nuevo,*args) if(self.proc_inicializacion) #ejecuta si es por el constructor simple
    if (self.block_inicializacion) #ejecuta para constructor con bloque
      nuevo.instance_exec(*args, &block_inicializacion)
    else
      unless (not (args[0].is_a? Hash) && args.length >0)
        hash = args[0]
        hash.each_pair {
         |key, value|
             nuevo.send("#{key}=", value)
        }
      else
        if block_properties
          raise RequiredArgumentMissingError if(args.empty?)
          bloque = self.block_properties
          nuevo.instance_exec(args, &bloque)
        end
      end
    end
    nuevo
  end

end

class PrototypedConstructorProperties < PrototypedConstructor
  attr_accessor :properties

  def new *args
    nuevo = PrototypedObject.new
    nuevo.set_prototype self.prototype
    self.properties.each { |propiedad| nuevo.set_property(propiedad,args.shift)}
    nuevo
  end
end

class PrototypedConstructorCopy < PrototypedConstructor
  def new
    nuevo = PrototypedObject.new
    nuevo.set_prototype self.prototype
    self.copiar_estado_a(nuevo)
  end

  def copiar_estado_a(instancia)
    self.prototype.instance_variables.each { |atributo|
      unless self.es_un_attr_privado(atributo)
        instancia.instance_variable_set(atributo, prototype.instance_variable_get(atributo))
      end
    }
    instancia
  end
end


class PrototypedConstructorExtended < PrototypedConstructor
  attr_accessor :extension

  def initialize prototipo, &bloque
    self.extension = bloque
    self.prototype = prototipo
  end

  def new *args
    extendido = PrototypedObject.new
    extendido.set_prototype(self.prototype)
    atributos_extension = args.shift
    valores = atributos_extension.values
    atributos_extension.keys.each { |un_atributo| extendido.instance_variable_set("@#{un_atributo}", valores.shift) }
    #args.unshift(extendido)
    #self.extension.call(args)
    extendido.instance_exec(args, &self.extension)
    extendido
  end
end