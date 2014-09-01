require 'rspec'
require_relative '../src/PrototypedObject'

describe 'Test de prototypes objects' do

  it 'Deberia crear un atributo dinamicamente a un objeto y obtener su valor' do
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

end