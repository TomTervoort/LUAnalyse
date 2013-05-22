


-- a,b,c = "abc",nil,12.2

-- false does not work

-- a.b = 10 + 11

-- a = 10

-- a[b] = 10

-- a.b.c = 10

a = 1
b = 2

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


