module Pari

using Printf

export pari_real_roots

function coefstr(c)
    n = numerator(c)
    d = denominator(c)
    d == 1 ? string(n) : "$(string(n))/$(string(d))"
end

function pari_real_roots(p::Vector{Rational{BigInt}}; gp="gp")
    d = length(p) - 1

    # PARI : pol = c₀ + c₁*x + ... + c_d*x^d
    poly = join(
        [
            if p[i] == 0
                ""
            elseif p[i] == 1 && i > 1
                "x^$(i-1)"
            elseif p[i] == -1 && i > 1
                "-x^$(i-1)"
            else
                str = coefstr(p[i])
                i == 1 ? str : "$(str)*x^$(i-1)"
            end
            for i in eachindex(p) if p[i] != 0
                ],
        "+"
    )
    isempty(poly) && (poly = "0")

    # Corrige les +- devant les coefficients négatifs
    poly = replace(poly, "+-" => "-")

    script = """
    p = $poly;
    Nb = max(1, 4000 \\ poldegree(p)^2);
    t = gettime();
    for(i = 1, Nb, r = polrootsreal(p));
    t = max(0.00001, gettime()/(1000.0 * Nb));
    print(#r, "|", t, "|", Nb)
    """
    output = read(pipeline(`$gp -q`, stdin=IOBuffer(script)), String)
    output = replace(output, r" " => "")
    str = split(strip(output), "|")
    nroots, elapsed, N = str
    return parse(Int, nroots), parse(Float64, elapsed)
end

end

using .Pari

function coef_chebyshev(n)
    T0 = [BigInt(1)]
    if (n == 0) return T0 end
    T1 = [BigInt(0), BigInt(1)]

    for _ in 2:n
        T0, T1 = T1, vcat([0], 2 .* T1) .- vcat(T0, [0, 0])
    end
    return T1
end
#p = coef_chebyshev(94)

#println(Pari.pari_real_roots(p))
