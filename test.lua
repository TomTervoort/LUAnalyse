local c = 1

a = function(a, b)
    return a + b + c
end


a = 2

local a = 1

a = 2




--[[


c,d=d,c



TODO:

-- "abc" --> "\"abc\""
-- local vs global variables (make everything global)







a = true

return 1 and 2


-- Constructor.
a = {b = 10, [c] = 11, 12}

-- While loop.
a = 0
while a <= 10 do
    a = a + 1
end

-- Repeat loop
a = 0
repeat
    a = a + 1
    
    if a * a >= 4 then
        break
    end
until a >= 10

-- Simple assignments.
a,b,c = "abc",nil,12.2

a.b = 10 + 11
a[b] = 10
a.b.c = 10

-- If.
if a then
    b = 10
    d = b + a
    
    if a then
        b = 10
        d = b + a
    else
        c = 11
        d = c + a
        
        abc(c, d)
    end
else
    c = 11
    d = c + a
    
    abc()
end

-- elseif...

do
    return 1
end

--]]
