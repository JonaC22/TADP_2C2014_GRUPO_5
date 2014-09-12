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

    otro_guerrero = guerrero.clone #clone es un metodo que ya viene definido en Ruby
    guerrero.atacar_a otro_guerrero

    espadachin = PrototypedObject.new

    espadachin.set_prototype(guerrero)
    espadachin.set_property(:habilidad, 0.5)
    espadachin.set_property(:potencial_espada, 30)
    espadachin.energia = 100

    espadachin.set_method(:potencial_ofensivo, proc {
      @potencial_ofensivo + self.potencial_espada * self.habilidad
    })

    expect(espadachin.potencial_ofensivo).to eq(45)
    expect(otro_guerrero.potencial_defensivo).to eq(10)

    espadachin.atacar_a(otro_guerrero)

    #Aca es donde esta el problema
    expect(otro_guerrero.energia).to eq(75) # Falla. Da 60 porque potenical ofensivo en vez de ser 45 es nuevamente 30 (que es el potencial ofensivo de guerrero)
  end

end