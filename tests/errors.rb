let t1 = {:foo => "foo"}

let t2 = {foo => "foo"}

when t1.foo
    case "foo"
end

case t1.foo
    when "foo"

# valid
case t1.foo
when "foo"
    puts "It's foo"
end

def no_end_in_sight