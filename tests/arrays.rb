let fruits = ["apple", "pineapple", "orange"]

fruits.each do |i, v|
    puts "#{v}"
    fruits[i] = lambda do
        puts "I'm #{v}, fruit #{i}"
    end
end

fruits[2]()

let people = [
    {
        :name => "Cactus Bill",
        :sector => 7
    },

    {
        :name => "Duck, Donald",
        :sector => 15,
        :check => lambda do
            return "DESTROY_ON_SIGHT"
        end
    }
]

people.each do |_, person|
    puts "#{person.name}: Section #{person.sector}"
    if person.check
        puts "#{person.check()}"
    end
end