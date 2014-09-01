require 'rspec'
require_relative '../src/PrototypedObject'

describe 'Test de prototypes objects' do

  it 'Deberia crear un atributo dinamicamente para una instancia particular' do
    guerrero = PrototypedObject.new
    guerrero.set_property(:energia, 100)
    expect(guerrero.energia).to eq(100)
  end

  it 'Un atributo definido para una instancia no debe ser definida para otra' do
    guerrero = PrototypedObject.new
    otro_guerrero = PrototypedObject.new
    guerrero.set_property(:energia, 100)

    expect(otro_guerrero).not_to respond_to(:energia)
  end

  it 'Deberia crear un metodo dinamicamente para una instancia particular' do
    guerrero = PrototypedObject.new
    guerrero.set_property(:energia, 100)
    guerrero.set_property(:potencial_defensivo, 50)
    guerrero.set_property(:potencial_ofensivo, 70)
    guerrero.set_method(
        :atacar_a,
        proc { |otro_guerrero|
          if (otro_guerrero.potencial_defensivo < self.potencial_ofensivo)
            otro_guerrero.recibe_danio(self.potencial_ofensivo - otro_guerrero.potencial_defensivo)
          end
        });
    guerrero.set_method(:recibe_danio,
        proc { |diferencia|
            self.energia = self.energia - diferencia
        })
    otro_guerrero = guerrero.clone #clone es un metodo que ya viene definido en Ruby
    guerrero.atacar_a otro_guerrero
    expect(otro_guerrero.energia).to eq(80)
  end
end