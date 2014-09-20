require 'rspec'
require_relative '../src/PrototypedObject'

describe 'Test de prototypes objects' do

  it 'Deberia definirse un atributo dinamicamente para una instancia unica' do

      guerrero = PrototypedObject.new
      guerrero.set_property(:fuerza,100)
      expect(guerrero.fuerza).to eq(100)

  end

  it 'Deberia crear un metodo dinamicamente para una instancia unica' do

    guerrero = PrototypedObject.new
    otro_guerrero = PrototypedObject.new

    guerrero.set_property(:fuerza,100)
    guerrero.set_method(:dup_fuerza,proc{self.fuerza*2})
    guerrero.set_method(:saludar,proc{"Hola"})

    expect(guerrero.dup_fuerza).to eq(200)
    expect(guerrero.saludar).to eq("Hola")
    expect(otro_guerrero).not_to respond_to(:saludar) #otro_guerrero no entiende el metodo
  end

  it 'Espadachin tiene las mismas propiedades que Guerrero' do

    guerrero = PrototypedObject.new
    espadachin = PrototypedObject.new

    guerrero.set_property(:fuerza,20)

    espadachin.set_prototype(guerrero)
    espadachin.fuerza = 50
    expect(espadachin.fuerza).to eq(50)

    #Si luego de setear el Prototipo espadachin, y a Guerrero se le agrega una nueva propiedad => a  Espadachin tambien se le agrega
    guerrero.set_property(:energia,60)
    espadachin.energia = 50
    expect(espadachin.energia).to eq(50)

    #Si a Espadachin se le agrega una propiedad a Guerrero no le pasa nada

    espadachin.set_property(:inteligencia,100)

    expect(espadachin.inteligencia).to eq(100)
    expect(guerrero.instance_variable_defined?(:@inteligencia)).to eq(false)

  end


  it 'Atacando a otros guerreros' do

    guerrero = PrototypedObject.new

    guerrero.set_property(:energia,100)
    expect(guerrero.energia).to eq(100)

    guerrero.set_property(:potencial_ofensivo,30)
    guerrero.set_property(:potencial_defensivo,10)

    guerrero.set_method(:atacar_a,
                        proc {
                            |otro_guerrero|
                          if(otro_guerrero.potencial_defensivo < self.potencial_ofensivo)
                            otro_guerrero.recibe_danio(self.potencial_ofensivo - otro_guerrero.potencial_defensivo)
                          end
                        });

    guerrero.set_method(:recibe_danio, proc {|impacto| self.energia = self.energia - impacto})

    otro_guerrero = guerrero.clone #clone es un metodo que ya viene definido en Ruby
    guerrero.atacar_a otro_guerrero
    expect(otro_guerrero.energia).to eq(80)

  end

  it 'metodos redefinidos no son afectados' do
    guerrero = PrototypedObject.new
    guerrero.set_property(:energia,100)
    guerrero.set_property(:potencial_ofensivo,30)
    guerrero.set_property(:potencial_defensivo,10)

    guerrero.set_method(:atacar_a,
                        proc {
                            |otro_guerrero|
                          if(otro_guerrero.potencial_defensivo < self.potencial_ofensivo)
                            otro_guerrero.recibe_danio(self.potencial_ofensivo - otro_guerrero.potencial_defensivo)
                          end
                        });

    guerrero.set_method(:recibe_danio, proc {|impacto| self.energia = self.energia - impacto})

    espadachin = PrototypedObject.new

    espadachin.set_prototype(guerrero)
    espadachin.set_property(:habilidad, 0.5)
    espadachin.set_property(:potencial_espada, 30)
    espadachin.energia = 130
    espadachin.potencial_ofensivo = 40
    espadachin.potencial_defensivo = 120

    espadachin.set_method(:potencial_ofensivo, proc {
      @potencial_ofensivo + self.potencial_espada * self.habilidad
    })

    expect(espadachin.potencial_ofensivo).to eq(55)

    guerrero.set_method(:potencial_ofensivo,proc{1000})

    expect(espadachin.potencial_ofensivo).to eq(55)
    expect(guerrero.potencial_ofensivo).to eq(1000)

  end

  it 'Guerrero no deberia entender nuevos metodos de espadachin' do
    guerrero = PrototypedObject.new
    guerrero.set_property(:energia,100)
    guerrero.set_property(:potencial_ofensivo,30)
    guerrero.set_property(:potencial_defensivo,10)

    guerrero.set_method(:atacar_a,
                        proc {
                            |otro_guerrero|
                          if(otro_guerrero.potencial_defensivo < self.potencial_ofensivo)
                            otro_guerrero.recibe_danio(self.potencial_ofensivo - otro_guerrero.potencial_defensivo)
                          end
                        });

    guerrero.set_method(:recibe_danio, proc {|impacto| self.energia = self.energia - impacto})

    espadachin = PrototypedObject.new

    espadachin.set_prototype(guerrero)

    espadachin.set_method(:sanar, proc {self.energia = self.energia + 10})
    espadachin.energia = 100
    espadachin.sanar
    expect(espadachin.energia).to eq(110)
    expect{guerrero.sanar}.to raise_error(NoMethodError)
  end

  it 'Probando atacar_a con un espadachin' do

    guerrero = PrototypedObject.new
    guerrero.set_property(:energia,100)
    guerrero.set_property(:potencial_ofensivo,30)
    guerrero.set_property(:potencial_defensivo,10)

    guerrero.set_method(:atacar_a,
                        proc {
                            |otro_guerrero|
                          if(otro_guerrero.potencial_defensivo < self.potencial_ofensivo)
                            otro_guerrero.recibe_danio(self.potencial_ofensivo - otro_guerrero.potencial_defensivo)
                          end
                        });

    guerrero.set_method(:recibe_danio, proc {|impacto| self.energia = self.energia - impacto})

    otro_guerrero = guerrero.clone

    espadachin = PrototypedObject.new

    espadachin.set_prototype(guerrero)
    espadachin.set_property(:habilidad, 0.5)
    espadachin.set_property(:potencial_espada, 30)
    espadachin.energia = 130
    espadachin.potencial_ofensivo = 40
    espadachin.potencial_defensivo = 120

    expect(espadachin.energia).to eq(130)
    expect(espadachin.potencial_ofensivo).to eq(40)
    expect(espadachin.potencial_defensivo).to eq(120)

    expect(guerrero.energia).to eq(100)
    expect(guerrero.potencial_ofensivo).to eq(30)
    expect(guerrero.potencial_defensivo).to eq(10)

    espadachin.set_method(:potencial_ofensivo, proc {
      @potencial_ofensivo + self.potencial_espada * self.habilidad
    })

    expect(espadachin.potencial_ofensivo).to eq(55)
    expect(otro_guerrero.energia).to eq(100)
    expect(otro_guerrero.potencial_defensivo).to eq(10)

    espadachin.atacar_a(otro_guerrero) #100 - (55 - 10)
    expect(otro_guerrero.energia).to eq(55)
  end

  it 'Constructor basico funciona bien' do
    guerrero = PrototypedObject.new
    guerrero.set_property(:energia,100)
    guerrero.set_property(:potencial_ofensivo,30)
    guerrero.set_property(:potencial_defensivo,10)

    guerrero.set_method(:atacar_a,
                        proc {
                            |otro_guerrero|
                          if(otro_guerrero.potencial_defensivo < self.potencial_ofensivo)
                            otro_guerrero.recibe_danio(self.potencial_ofensivo - otro_guerrero.potencial_defensivo)
                          end
                        });

    guerrero.set_method(:recibe_danio, proc {|impacto| self.energia = self.energia - impacto})

    Guerrero = PrototypedConstructor.new(guerrero, proc {
        |guerrero_nuevo, una_energia, un_potencial_ofensivo, un_potencial_defensivo|
      guerrero_nuevo.energia = una_energia
      guerrero_nuevo.potencial_ofensivo = un_potencial_ofensivo
      guerrero_nuevo.potencial_defensivo = un_potencial_defensivo
    })
    un_guerrero = Guerrero.new(100, 30, 10)
    expect(un_guerrero.energia).to eq(100)
  end

  it 'Constructor basico con mapa funciona bien' do

    pipe = PrototypedObject.new
    pipe.set_property(:energia,100)
    pipe.set_property(:potencial_ofensivo,30)
    pipe.set_property(:potencial_defensivo,10)

    Guerrero = PrototypedConstructor.new(pipe)

    julian = Guerrero.new(
        {energia: 100, potencial_ofensivo: 30, potencial_defensivo: 10}
    )

    expect(julian.energia).to eq(100)
    expect(julian.potencial_ofensivo).to eq(30)
    expect(julian.potencial_defensivo).to eq(10)



  end

  it 'Constructor copy funciona bien' do
    guerrero = PrototypedObject.new
    guerrero.set_property(:energia,100)
    guerrero.set_property(:potencial_ofensivo,30)
    guerrero.set_property(:potencial_defensivo,10)

    guerrero.set_method(:atacar_a,
                        proc {
                            |otro_guerrero|
                          if(otro_guerrero.potencial_defensivo < self.potencial_ofensivo)
                            otro_guerrero.recibe_danio(self.potencial_ofensivo - otro_guerrero.potencial_defensivo)
                          end
                        });

    guerrero.set_method(:recibe_danio, proc {|impacto| self.energia = self.energia - impacto})


    expect(guerrero.energia).to eq(100)
    expect(guerrero.potencial_ofensivo).to eq(30)
    expect(guerrero.potencial_defensivo).to eq(10)

    Guerrero = PrototypedConstructor.copy(guerrero)

    otro_guerrero = Guerrero.new

    expect(otro_guerrero.energia).to eq(100)
    expect(otro_guerrero.potencial_ofensivo).to eq(30)
    expect(otro_guerrero.potencial_defensivo).to eq(10)
    expect(otro_guerrero.respond_to?(:recibe_danio)).to eq(true)
  end

  # it 'Constructor extended funciona bien' do
  #   guerrero = PrototypedObject.new
  #   guerrero.set_property(:energia,100)
  #   guerrero.set_property(:potencial_ofensivo,30)
  #   guerrero.set_property(:potencial_defensivo,10)
  #
  #   guerrero.set_method(:atacar_a,
  #                       proc {
  #                           |otro_guerrero|
  #                         if(otro_guerrero.potencial_defensivo < self.potencial_ofensivo)
  #                           otro_guerrero.recibe_danio(self.potencial_ofensivo - otro_guerrero.potencial_defensivo)
  #                         end
  #                       });
  #
  #   guerrero.set_method(:recibe_danio, proc {|impacto| self.energia = self.energia - impacto})
  #
  #
  #   expect(guerrero.energia).to eq(100)
  #   expect(guerrero.potencial_ofensivo).to eq(30)
  #   expect(guerrero.potencial_defensivo).to eq(10)
  #
  #   Guerrero = PrototypedConstructor.copy(guerrero)
  #
  #   Espadachin = Guerrero.extended {
  #     |espadachin, habilidad, potencial_espada|
  #     espadachin.set_property(:habilidad, habilidad)
  #     espadachin.set_property(:potencial_espada, potencial_espada)
  #
  #     espadachin.set_method(:potencial_ofensivo, proc{
  #       @potencial_ofensivo + self.potencial_espada * self.habilidad
  #     })
  #   }
  #
  #   espadachin = Espadachin.new({energia: 100, potencial_ofensivo: 30, potencial_defensivo: 10}, 0.5, 30)
  #
  #   expect(espadachin.energia).to eq(100)
  #   expect(espadachin.potencial_ofensivo).to eq(45)
  #   expect(espadachin.potencial_defensivo).to eq(10)
  #
  # end

  it 'Azucar sintactico, deberia poder agregar atributos y metodos dinamicamente' do
    guerrero = PrototypedObject.new
    guerrero.energia = 100
    expect(guerrero.energia).to eq(100)

    guerrero.potencial_defensivo = 10
    guerrero.potencial_ofensivo = 30

    guerrero.potencial_ofensivo = proc{@potencial_ofensivo + 10}

    guerrero.atacar_a = proc {
                          |otro_guerrero|
                          if(otro_guerrero.potencial_defensivo < self.potencial_ofensivo)
                            otro_guerrero.recibe_danio(self.potencial_ofensivo - otro_guerrero.potencial_defensivo)
                          end
                        }

    guerrero.recibe_danio = proc {|impacto| self.energia = self.energia - impacto}

    expect(guerrero.respond_to? :atacar_a).to eq(true)
    otro_guerrero = guerrero.clone

    guerrero.atacar_a otro_guerrero
    expect(otro_guerrero.energia).to eq(80)
  end

  it 'Crear un prototipo con azucar sintactico funciona bien' do
    guerrero = PrototypedObject.new {
      self.energia = 100
      self.potencial_ofensivo = 30
      self.potencial_defensivo = 10
      self.atacar_a = proc {
                          |otro_guerrero|
                          if(otro_guerrero.potencial_defensivo < self.potencial_ofensivo)
                            otro_guerrero.recibe_danio(self.potencial_ofensivo - otro_guerrero.potencial_defensivo)
                          end
                      }
      self.recibe_danio = proc {|impacto| self.energia = self.energia - impacto}
    }

    expect(guerrero.energia).to eq(100)
    expect(guerrero.potencial_ofensivo).to eq(30)
    expect(guerrero.potencial_defensivo).to eq(10)
    expect(guerrero.respond_to? :atacar_a).to eq(true)
  end

  it 'azucar sintactico al constructor 1' do
    guerrero = PrototypedObject.new {
      self.energia = 100
      self.potencial_ofensivo = 30
      self.potencial_defensivo = 10
      self.atacar_a = proc {
          |otro_guerrero|
        if(otro_guerrero.potencial_defensivo < self.potencial_ofensivo)
          otro_guerrero.recibe_danio(self.potencial_ofensivo - otro_guerrero.potencial_defensivo)
        end
      }
      self.recibe_danio = proc {|impacto| self.energia = self.energia - impacto}
    }

    Guerrero = PrototypedConstructor.new(guerrero) do |una_energia, un_potencial_ofensivo, un_potencial_defensivo|
      self.energia = una_energia
      self.potencial_ofensivo = un_potencial_ofensivo
      self.potencial_defensivo = un_potencial_defensivo
    end

    otro_guerrero = Guerrero.new(70,40,50)
    
    expect(otro_guerrero.energia).to eq(70)
    expect(otro_guerrero.potencial_ofensivo).to eq(40)
    expect(otro_guerrero.potencial_defensivo).to eq(50)
    expect(otro_guerrero.respond_to? :atacar_a).to eq(true)
  end

  it 'azucar sintactico constructor create' do
    Guerrero = PrototypedConstructor.create {
      self.atacar_a = proc{|otro_guerrero|
        if(otro_guerrero.potencial_defensivo < self.potencial_ofensivo)
          otro_guerrero.recibe_danio(self.potencial_ofensivo - otro_guerrero.potencial_defensivo)
        end}
      self.recibe_danio = proc{|impacto| self.energia = self.energia - impacto}
    }.with{
      |una_energia, un_potencial_ofensivo, un_potencial_defensivo|
      self.energia = una_energia
      self.potencial_ofensivo = un_potencial_ofensivo
      self.potencial_defensivo = un_potencial_defensivo
    }

    atila = Guerrero.new(100,50,30)
    expect(atila.potencial_ofensivo).to eq(50)

    proto_guerrero = Guerrero.prototype
    proto_guerrero.potencial_ofensivo = proc {1000}

    expect(atila.potencial_ofensivo).to eq(1000)
  end

  it 'azucar sintactico constructor create segunda forma' do
    Guerrero = PrototypedConstructor.create {
      self.atacar_a = proc{|otro_guerrero|
        if(otro_guerrero.potencial_defensivo < self.potencial_ofensivo)
          otro_guerrero.recibe_danio(self.potencial_ofensivo - otro_guerrero.potencial_defensivo)
        end}
      self.recibe_danio = proc{|impacto| self.energia = self.energia - impacto}
    }.with_properties([:energia, :potencial_ofensivo, :potencial_defensivo])

    atila = Guerrero.new(100,50,30)
    expect(atila.potencial_ofensivo).to eq(50)

    proto_guerrero = Guerrero.prototype
    proto_guerrero.potencial_ofensivo = proc {1000}

    expect(atila.potencial_ofensivo).to eq(1000)
  end

  it 'Azucar sintactico constructor extended' do
    Guerrero = PrototypedConstructor.create {
      self.atacar_a = proc{|otro_guerrero|
        if(otro_guerrero.potencial_defensivo < self.potencial_ofensivo)
          otro_guerrero.recibe_danio(self.potencial_ofensivo - otro_guerrero.potencial_defensivo)
        end}
      self.recibe_danio = proc{|impacto| self.energia = self.energia - impacto}
      self.energia = 100
      self.potencial_ofensivo = 200
      self.potencial_defensivo = 10
    }

    Espadachin = Guerrero.extended {
        |una_habilidad, un_potencial_espada|
      self.habilidad = una_habilidad
      self.potencial_espada = un_potencial_espada

      self.potencial_ofensivo = proc{
        @potencial_ofensivo + self.potencial_espada * self.habilidad
      }
    }

    espadachin = Espadachin.new({energia: 100, potencial_ofensivo: 30, potencial_defensivo: 10}, 0.5, 30)

    expect(espadachin.energia).to eq(100)
    expect(espadachin.recibe_danio(10)).to eq(90)
    expect(espadachin.potencial_ofensivo).to eq(45)
    expect(espadachin.potencial_defensivo).to eq(10)

  end

  it 'Implementacion siguiente' do

    guerrero = PrototypedObject.new {
      self.energia = 100
      self.potencial_ofensivo = 30
      self.potencial_defensivo = 10
      self.atacar_a = proc {
          |otro_guerrero|
        if(otro_guerrero.potencial_defensivo < self.potencial_ofensivo)
          otro_guerrero.recibe_danio(self.potencial_ofensivo - otro_guerrero.potencial_defensivo)
        end
      }
      self.recibe_danio = proc {|impacto| self.energia = self.energia - impacto}
    }

    Guerrero = PrototypedConstructor.new(guerrero)

    Espadachin = Guerrero.extended {
      |una_habilidad, un_potencial_espada|
      self.habilidad = una_habilidad
      self.potencial_espada = un_potencial_espada

      self.potencial_ofensivo = proc {
        call_next + self.potencial_espada * self.habilidad
      }
    }
    espadachin = Espadachin.new({energia: 100, potencial_ofensivo: 30, potencial_defensivo: 10}, 0.5, 30)

    expect(espadachin.energia).to eq(100)
    expect(espadachin.potencial_ofensivo).to eq(45)
    expect(espadachin.potencial_defensivo).to eq(10)


  end

  it 'prototipos multiples' do
    Atacante = PrototypedConstructor.create {
      self.super_ataque_a = proc{|otro_guerrero|
        otro_guerrero.energia = otro_guerrero.energia - 10
      }
    }.with_properties([:potencial_ofensivo])

    proto_atacante = Atacante.new(800)

    Defensor = PrototypedConstructor.create {
      self.curarme = proc{
        self.energia = self.energia + 50}
    }

    proto_defensor = Defensor.new

    Guerrero = PrototypedConstructor.create {
      self.atacar_a = proc{|otro_guerrero|
        if(otro_guerrero.potencial_defensivo < self.potencial_ofensivo)
          otro_guerrero.recibe_danio(self.potencial_ofensivo - otro_guerrero.potencial_defensivo)
        end}
      self.recibe_danio = proc{|impacto| self.energia = self.energia - impacto}
    }.with_properties([:energia, :potencial_ofensivo, :potencial_defensivo])

    Guerrero.prototype.set_prototypes([proto_atacante, proto_defensor])
    expect(Guerrero.prototype.prototypes.length).to eq(2)

  end

end