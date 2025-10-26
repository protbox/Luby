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

people.each do |person| # if you omit the index it translates to _, person
    puts "#{person.name}: Sector #{person.sector}"
    if person.check
        puts "#{person.check()}"
    end
end

people[1].each_pair do |k,v|
    puts "#{k} => #{v}"
end