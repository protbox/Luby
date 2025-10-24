# this is a comment -> --

# key/value table
let t = {
    :foo => 'bar',
    :hello => "world",
    :nested => {
        :inside_nested => true
    }
}

# ->
# local t = {
#     foo = 'bar',
#     hello = "world",
#     nested = {
#         inside_nested = true
#     }
# }

# pairs
t.each_pair do |k, v| # -> for k, v in pairs(t)
    if type(v) == "table"
        v.each_pair do |a, b|
            puts "#{a} => #{tostring(b)}"
        end
    else
        puts "#{k} => #{v}"
    end
end

# if you prefer, the if/else can also be handled with
# case/when (more on this coming up)
t.each_pair do |k, v|
    case type(v)
    when "table"
        v.each_pair do |a, b|
            puts "#{a} => #{tostring(b)}"
        end
    when "thing"
        puts "it's thing"
    else
        puts "#{k} => #{v}"
    end
end

let a = { 'apple', 'pineapple', 'orange' }

# ipairs
a.each do |i,v| # -> for i,v in ipairs(a)
    puts "Fruit #{i}: #{v}"
end

# anonymous functions
let sum = lambda do |x, y| # -> local sum = function(x, y)
    return x + y
end

let no_args = lambda do # -> local no_args = function()
    return "8 and 5"
end

# interpolation
puts "The sum of #{no_args()} is #{sum(8, 5)}" # -> print("The sum of " .. no_args() .. " is " .. sum(8, 5))

# functions
def foo # -> function foo()
    puts "I am foo"
end

def foo2(some, args) # -> function foo2(some, args)
end

# local functions (just add : Local)
def foo_but_local : Local # -> local function foo_but_local()
    puts "halp I'm stuck in this file"
end

# case/when (basically if/elseif/else)
let str = "Snugglwomp"

case str
when "Snug%"
     # str begins with Snug
when "%lwomp"
    # str ends with lwomp
when "Snugglwomp"
    # matches Snugglwomp exactly
else
    # no case match
end

let n = 7

case n # -> removed
when 5..7 # -> if n >= 5 and n <= 5 then
    # n is within range of 5-7
when < 5 # non partial, exact or ranged expressions are written as-is with the case at the front -> n < 5
    # you can add any other expression. for example this will translate to n < 5
end

# if/elsif/else
let expr = false # just a placeholder so following example passes

if expr # -> if expr then
    puts "if matched"
elsif expr # -> elseif expr then
    puts "elsif matched"
    if expr # -> if expr then
        puts "nested if"
    elsif expr # -> elseif expr then
        puts "nested elsif"
    end
else
    puts "nothing matched"
end

# classes
# when a class is detected, luby throws in class implementation lua code

class Animal # -> local Animal = class:extend_as("Animal")
    def initialize(type) # -> function Animal:initialize(type)
        @type = type or "Unknown" # -> self.type = type or "Unknown"
    end

    def speak # -> function Animal:speak()
        puts "..."
    end
end # end class

# extending from a base class (inheritance)
# Class < ParentClass
class Duck < Animal # -> local Duck = Animal:extend_as("Duck")
    def speak # -> function Duck:speak()
        super(self) # -> Duck.super.speak(self)
        puts "The #{@__name} goes Quack!" # @__name refers to the classes name, ie: Duck
        puts "Parent class name is #{@super.__name}" # Animal
    end
end # end class

let duck = Duck()
duck:speak()

# unary operators
let x = 6
x -= 1 # -> x = x - 1
puts "Starting from: #{x}" # should be 5

while x <= 10
    puts "#{x}"
    x += 2 # x = x + 2
end