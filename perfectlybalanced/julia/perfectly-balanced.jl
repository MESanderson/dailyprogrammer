



function perfectly_balanced_bonus(str_in)
    if str_in == ""
        return true
    end
    
    cntr = Dict()
    for i in str_in
        cntr[i] = (get(cntr, i, 0) + 1     ) 
    end

    vals = collect(values(cntr))
    return all( i -> i == vals[1], vals)
end

println(perfectly_balanced_bonus(ARGS[1]))
