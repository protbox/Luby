# a super contrived test

class People
    def initialize
    end

    def speak
        puts "..."
    end

    def mult(x, y)
        return x * y
    end
end

class Person < People
    def initialize
        super(self)
        @literal_test = "foo"
        @tags = {
            :m => \mult(2, 4)
        }

        @fruits = ['apple', 'orange']
        @fn = lambda do |msg|
            puts "#{msg}"
        end
    end

    def speak
        super(self)
        puts "Hey!"
    end

    def get_mult
        return \mult(5, 26) # -> self:mult
    end
end

let p = Person()
puts "#{p:get_mult()}"
p:speak()
puts "#{p.tags.m}"
p.fn("ok, goodbye.")