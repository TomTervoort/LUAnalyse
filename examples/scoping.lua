-- While loop.
local i = 0
local j = true
local k = 1

while i <= 10 do
    local j = i
    
    do
        local i = 1
        j = j + i
    end
    
    k = k + i
    j = j + 1
    i = j
end

print(k)
