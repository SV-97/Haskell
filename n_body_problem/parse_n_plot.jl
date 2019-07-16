using Plots

function parse_data(txt)
    b1 = split(txt, '\n')
    b2 = map((a)->a[2:end - 1], b1) # remove brackets
    bodies = []
    for body in b2[1:end - 1] # remove empty last line
        push!(bodies, [])
        c1 = split(body, "),(")
        c2 = map((x)->strip(x, ['(', ')']), c1)
        for (l, r) in map((x)->split(x, ','), c2)
            x = parse(Float64, strip(l))
            y = parse(Float64, strip(r))
            push!(bodies[end], [x, y])
        end
    end
    return bodies
end

function animate(bods)
    steps = zip(bods...)
    anim = @animate for step in steps
        (Rx, Ry) = collect(zip(step...))
        rx = [x for x in Rx]
        ry = [x for x in Ry]
        scatter(rx', ry', xlims = (-2.5, 2.5), ylims = (-2.5, 2.5), label = ["Sun", "Earth", "Mercury", "Mars", "Venus"])
    end every 2
    
    name = "Haskell"
    gif(anim, "Solar_$name.gif", fps = 30)
end

f = open("n_bod_prob_solution.txt", "r")
txt = read(f, String)
bods = parse_data(txt)
animate(bods)
close(f)