-- Simple assignments.
a,b,c,d,e = "abc",nil,12.2,{},true

d.key = 1
e = not e
a = b

local concat = function(a,b)
    return a .. b
end

c = concat(c, "somestring")

