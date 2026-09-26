Base.exit_on_sigint(false)

using Printf
include("./pari.jl")
using .Pari
using RS
using MPFI

#const Flt = BigFloat
#setprecision(128)
const Flt = Float64


struct Hermite3
    fa::Flt
    A::Flt
    B::Flt
    C::Flt
    a::Flt
end

struct DFun{F,G}
    f::F
    fdf::G
    A::Flt
    B::Flt
    coefs::Union{Nothing,Vector{Rational{BigInt}}}
    nb_roots::Int
end

function norm_fun(F::DFun; coef=2)
    function fdf(x::Flt)
        fx, dfx = F.fdf(x)
        fx = fx/coef
        return (coef * asinh(fx), dfx/sqrt(fx*fx + 1))
    end
    return DFun(F.f,fdf,F.A,F.B,nothing,F.nb_roots)
end

function hermite3_make(a::Flt, fa, dfa,
                       b::Flt, fb, dfb)

    if (abs(fa) > abs(fb))
        (a,fa,dfa,b,fb,dfb) = (b,fb,dfb,a,fa,dfa)
    end
    c = b-a
    A = dfa
    B = (3*(fb-fa)-c*(2*dfa+dfb))/(c*c)
    C = (c*(dfa+dfb)-2*(fb-fa))/(c*c*c)

    return Hermite3(fa, A, B, C, a)
end

function hermite3_critical(H::Hermite3)
    D = H.B*H.B - 3*H.A*H.C
    if (D >= 0)
        if (H.B > 0)
	    tmp = -H.B - sqrt(D)
        else
            tmp = -H.B + sqrt(D)
        end
	x1 = H.a + tmp/(3*H.C)
        x2 = H.a + H.A/tmp;
        if (x1 > x2)
            (x1,x2) = (x2,x1)
        end
        return (x1,x2)
    end
    return nothing
end

function hermite3_fdf(H::Hermite3,x::Flt)
    u = (x - H.a)
    fx = ((H.C * u + H.B) * u + H.A) * u + H.fa
    dfx = (3*H.C * u + 2*H.B) * u + H.A
    (fx, dfx)
end

function WH(fn::DFun, H::Hermite3, x::Flt, root::Bool,  bound::Flt)
    fx, dfx = fn.fdf(x)
    hx, dhx = hermite3_fdf(H, x)
    R1 = abs(hx-fx)/(abs(fx)+abs(hx))
    R2 = root ? 1.0 : abs(dhx-dfx)/(abs(dfx)+abs(dhx))
#    R1 = abs(hx-fx)/hypot(fx,hx)
#    R2 = root ? 1.0 : abs(dhx-dfx)/hypot(dfx,dhx)
#    R1 = abs((hx - fx)/(hx + fx))
#    R2 = abs((dhx - dfx)/(dhx + dfx))
    C = isnan(R2) ? R1 : isnan(R1) ? R2 : R1*R2
    res = C < bound*bound
    #println(res, " R1: ", R1, " R2: ", R2, " x: ", x,
    #        " " , fx, " ", dfx, " ", hx, " ", dhx)

    (res, C, fx, dfx)

end

function dicho(fn::DFun, a::Flt, fa::Flt, b::Flt, fb::Flt)
    c = a
    @assert fa *fb < 0
    while true
        c = (a+b)/2
        if (a == c || b == c)
            break
        end
        fc = fn.f(c)
        if (fc == 0.0)
            break
        end
        if (fa * fc < 0)
            b = c; fb = fc
        else
            a = c; fa = fc
        end
    end
    return c
end

default_refine = false

function isolate(fn::DFun, A::Flt, B::Flt;
                 bound = .1, nb_samples = 3, alea=1e-2, refine=default_refine)
    roots = Union{Flt,Tuple{Flt,Flt}}[]
    bound = Flt(bound)
    count = 0
    function push_or_refine!(a,fa,b,fb)
        if (refine)
            push!(roots, dicho(fn,a,fa,b,fb))
        else
            push!(roots,(a,b))
        end
    end

    function loop(a::Flt, fa::Flt, dfa,
                  b::Flt, fb::Flt, dfb)
        #println("loop a:", a, " ", fa, " ", dfa, " b: ", b," ", fb, dfb)
        @assert a < b "$a $b"
        H = hermite3_make(a,fa,dfa,b,fb,dfb)
        (x1, x2) = something(hermite3_critical(H), (Flt(NaN), Flt(NaN)))
        C = -Inf
        G = true
        best = (a+b)/2
        fbest = 0
        dfbest = 0
        if (a < x1 && x1 < b)
            G, C2, fx1, dfx1 = WH(fn,H,x1,true,bound)
            if (!G && !(C > C2))
                C, best, fbest, dfbest = C2, x1, fx1, dfx1
            end
        end
        if (a < x2 && x2 < b)
            G1, C2, fx2, dfx2 = WH(fn,H,x2,true,bound)
            G = G && G1
            if (!G1 && !(C > C2))
                C, best, fbest, dfbest = C2, x2, fx2, dfx2
            end
        end
        for j in 1:nb_samples
#	    t3 = j/(nb_samples+1);
	    t3 = (cos(pi*j/(nb_samples+1)) + 1)/2.0;
            xn = a + t3*(b - a)
            @assert a <= xn && xn <= b "$a $xn $b"
            G1, Cn, fxn, dfxn = WH(fn,H,xn,false,bound)
            G = G && G1
            if (!G1 && !(C > Cn))
                C, best, fbest, dfbest = Cn, xn, fxn, dfxn
            end
        end
        # if (a < x1 && x1 < b)
        #     nb -= 1
        # end
        # if (a < x2 && x2 < b)
        #     nb -= 1
        # end
        #println("xs:",x1," ", fx1, " ", R1, "\n",
        #          x2," ", fx2, " ", R2, "\n",
        #          x3," ", fx3, " ", R3)
        if (!G && (a < best < b))
             loop(a,fa,dfa,best,fbest,dfbest)
             loop(best,fbest,dfbest,b,fb,dfb)
        else
            count += 1
            if (a < x1 && x1 < b)
                if (fa*fx1 < 0)
                    push_or_refine!(a,fa,x1,fx1)
                elseif (fx1 == 0.0)
                    push!(roots, x1)
                end
                if (a < x2 && x2 < b)
                    if (fx1*fx2 < 0)
                        push_or_refine!(x1,fx1,x2,fx2)
                    elseif (fx2 == 0.0)
                        push!(roots, x2)
                    end
                    if (fx2*fb < 0)
                        push_or_refine!(x2,fx2,b,fb)
                    end
                elseif (fx1 * fb < 0)
                    push_or_refine!(x1,fx1,b,fb)
                end
            elseif (a < x2 && x2 < b)
                if (fa*fx2 < 0)
                    push_or_refine!(a,fa,x2,fx2)
                elseif (fx2 == 0.0)
                    push!(roots, x2)
                end
                if (fx2*fb < 0)
                    push_or_refine!(x2,fx2,b,fb)
                end
            elseif (fa * fb < 0)
                push_or_refine!(a,fa,b,fb)
            end
            if (fb == 0.0)
                push!(roots, b)
            end
        end
    end
    fa,dfa = fn.fdf(A)
    fb,dfb = fn.fdf(B)
    loop(A,fa,dfa,B,fb,dfb)
    return roots,  length(roots), count
end

using Plots
using Statistics

total_tests = 0
total_errors = 0

function benchmark_isolate(msg, make_poly, ns; bound=.1, nb_samples = 3, pert=0.02)
    F = make_poly(ns[1])
    isolate(F,F.A,F.B;refine=true)
    isolate(F,F.A,F.B;refine=false)
    if (F.coefs != nothing) RS.rs_isolate(F.coefs) end
    println(msg)
    global total_tests
    global total_errors
    leaves = Float64[]
    times = Float64[]
    times_refine = Float64[]
    times_pari = Float64[]
    times_rs = Float64[]
    nbroots = Int[]

    nb_tests = 0
    nb_errors = 0

    for n in ns
        counts = Int[]
        ts = Float64[]
        tsr = Float64[]
        tsp = Float64[]
        tsrs = Float64[]
        local_errors = 0
        r = 0
        for i in 1:10
            f = make_poly(n)
            a = f.A * (1 + pert*(rand() - 0.5))
            b = f.B * (1 + pert*(rand() - 0.5))
            e = f.nb_roots
            t = @elapsed begin
                r, nr, count = isolate(f, a, b; bound, nb_samples)
            end
            nb_tests += 1
            if length(r) != e
                nb_errors += 1
                local_errors += 1
            end
            tr = @elapsed begin
                r, nr, count = isolate(f, a, b; refine=true, bound, nb_samples)
            end

            if (f.coefs != nothing)
                nr, tp = Pari.pari_real_roots(f.coefs)
                @assert e == nr
                push!(tsp,tp)
                trs = @elapsed begin
                    RS.rs_isolate(f.coefs)
                end
                push!(tsrs,trs)
            end
            nb_tests += 1
            if length(r) != e
                nb_errors += 1
                local_errors += 1
            end
            push!(ts,t)
            push!(tsr,tr)
            push!(counts,count)
        end
        t = mean(ts)
        tr = mean(tsr)
        count = mean(counts)
        push!(leaves, count)
        push!(times, t*1000.0)
        push!(times_refine, tr*1000.0)
        tp = 0.0; trs = 0.0;
        if (length(tsp) > 0)
            tp = mean(tsp)
            push!(times_pari, tp*1000.0)
        end
        if (length(tsrs) > 0)
            trs = mean(tsrs)
            push!(times_rs, trs*1000.0)
        end
        rs = length(r)
        @printf("n=%d  roots=%d leaves=%.1f  isol=%.3fms, refine=%.3fms, pari=%.3fms, rs=%.3fms, #errors=%d\n",
                n, rs, count, t*1000.0 ,tr*1000.0, tp*1000.0, trs*1000.0, local_errors)
    end

    p = plot(ns, leaves,
             xlabel="degree n",
             ylabel="subdivisions",
             marker=:circle,
             markersize=3,
             label="subdivisions",
             legend=:topleft)

    plots = [times, times_refine]
    labels = ["ours" "ours+dicho" "pari" "rs"]
    colors = [:blue :red :green :yellow]

    if (length(times_pari) > 0)
        push!(plots, times_pari)
    end
    if (length(times_rs) > 0)
        push!(plots, times_rs)
    end
    lo, hi = extrema(Iterators.flatten(plots))
    ticks = [i * 10.0^j for i in [1,2,5] for j in -4:4]
    ticks = sort(filter(x -> lo < x < hi, ticks))
    plot!(twinx(),
          ns, plots,
          ylabel="time (ms)",
          yscale=:log10,
          yticks=ticks,
          yformatter = x -> string(round(x, sigdigits=2)),
          marker=:square,
          markersize=2,
          color=colors,
          label=labels,
          legend=:bottomright)

    savefig(p, "../article/images/$(msg).png")
    total_tests += nb_tests
    total_errors += nb_errors
    println(msg, " errors: ", nb_errors, "/", nb_tests)
    return p
end

function coef_chebyshev(n)
    T0 = [BigInt(1)]
    if (n == 0) return T0 end
    T1 = [BigInt(0), BigInt(1)]

    for _ in 2:n
        T0, T1 = T1, vcat([0], 2 .* T1) .- vcat(T0, [0, 0])
    end
    return Vector{Rational{BigInt}}(T1)
end

function chebyshev(n)
    function f(x::Flt)::Flt
        if abs(x) <= 1
            return cos(n * acos(x))
        elseif x > 1
            return cosh(n * acosh(x))
        elseif x < -1
            return (-1)^n * cosh(n * acosh(-x))
        end
    end
    function fdf(x::Flt)::Tuple{Flt,Flt}
        if abs(x) <= 1
            ac = acos(x)
            return cos(n * ac), n * sin(n * ac) / sqrt(1 - x*x)
        elseif x > 1
            ac = acosh(x)
            return cosh(n * ac), n * sinh(n * ac) / sqrt(x*x - 1)
        elseif x < -1
            ac = acosh(-x)
            return (-1)^n * cosh(n * ac), (-1)^(n-1) * n * sinh(n * ac) / sqrt(x*x - 1)
        end
    end
    return DFun(f,fdf,Flt(-10.0),Flt(10.0),coef_chebyshev(n),n)
end

function mignotte_bound(n,p)
    return max(Flt(2),2^((2*p+3)/Flt(n-2)))
end


function mignotte(n,p)
    function f(x::Flt)::Flt
        return x^n - 2*(2^p*x - 1)^2
    end
    function fdf(x::Flt)::Tuple{Flt,Flt}
        return (x^n - 2*(2^p*x - 1)^2, n*x^(n-1) - 4*2^p*(2^p*x - 1))
    end
    c = zeros(BigInt, n + 1)
    c[1] = -2
    c[2] = BigInt(2)^(p + 2)
    c[3] = -BigInt(2)^(2p + 1)
    c[n + 1] += 1
    DFun(f,fdf,
         -mignotte_bound(n,p),
         mignotte_bound(n,p),
         Vector{Rational{BigInt}}(c),
         (n % 2 == 0) ? 4 : 3)
end

function coef_wilkinson(n)
    p = BigInt[1]

    for k in 1:n
        q = zeros(BigInt, length(p) + 1)

        for i in eachindex(p)
            q[i]   -= p[i]
            q[i+1] += BigInt(k) * p[i]
        end

        p = q
    end

    Vector{Rational{BigInt}}(p)
end

function wilkinson(n)
    function f(x::Flt)::Flt
        p = 1.0
        for k in 1:n
            p *= x - k
        end
        return p
    end
    function fdf(x::Flt)::Tuple{Flt,Flt}
        p = 1.0
        dp = 0.0
        for k in 1:n
            dp = p + dp*(x-k)
            p *= x - k
        end
        return p,dp
    end

    return DFun(f,fdf,Flt(-n),Flt(2*n),coef_wilkinson(n),n)
end

function coef_geometric(n)
    p = BigInt[1]

    for k in 0:n-1
        d = BigInt(1) << k
        q = zeros(BigInt, length(p) + 1)

        for i in eachindex(p)
            q[i]   -= p[i]
            q[i+1] += d * p[i]
        end

        p = q
    end

    Vector{Rational{BigInt}}(p)
end

function geometric(n)
    function f(x::Flt)::Flt
        p = 1.0
        r = 1.0
        for k in 1:n
            p *= x-r
            r *= 0.5
        end
        p
    end
    function fdf(x::Flt)::Tuple{Flt,Flt}
        p = 1.0
        dp = 0.0
        r = 1.0
        for k in 1:n
            dp = dp*(x-r) + p
            p *= x-r
            r *= 0.5
        end
        p,dp
    end

    DFun(f,fdf,-30.0,30.0,coef_geometric(n),n)
end

using Distributions

function coef_random_r(roots)
    p = [Rational{BigInt}(1)]
    roots = [ rationalize(r; tol = 0) for r in roots]
    for r in roots
        q = zeros(Rational{BigInt}, length(p) + 1)
        for i in eachindex(p)
            q[i]   -= r * p[i]
            q[i+1] += p[i]
        end
        p = q
    end

    p
end

function random_roots(n; σ=2.0)
    roots = rand(Normal(0, σ), n)
    lo, hi = extrema(roots)
    lo = min(-1.0,lo)
    hi = max(hi, 1.0)
    function f(x::Flt)::Flt
        p = 1.0
        for k in 1:n
            p *= x-roots[k]
        end
        p
    end
    function fdf(x::Flt)::Tuple{Flt,Flt}
        p = 1.0
        dp = 0.0
        for k in 1:n
            dp = dp*(x-roots[k]) + p
            p *= x-roots[k]
        end
        p,dp
    end

    return DFun(f,fdf, 3*lo, 3*hi, coef_random_r(roots), n)
end

function coef_random(n; scale=1.0)
    r = [ 0.0 for i in 0:n+1]
    c = 2.0^(-n/2)
    for i in 1:n+1
        r[i] = (2*rand()-1) * scale * c
        c *= sqrt((n-i+1) / i)
    end
    r
end

function random(n; scale=1.0)
    coef = coef_random(n; scale)
    coef_rat = Vector{Rational{BigInt}}([ rationalize(r; tol = 0) for r in coef])
    function f(x::Flt)::Flt
        r = 0.0
        for i in n+1:-1:1
            r = r * x + coef[i]
        end
        @assert isfinite(r)
        r
    end
    function fdf(x::Flt)::Tuple{Flt,Flt}
        r = 0.0
        dr = 0.0
        for i in n+1:-1:2
            r = r * x + coef[i]
            dr = dr * x + coef[i] * (i-1)
        end
        r = r * x + coef[1]
        @assert isfinite(r)
        @assert isfinite(dr)
        r,dr
    end
    A = scale
    B = -scale
    roots=  RS.rs_isolate(coef_rat)
    nr = length(roots)
    A = min(-1,nr == 0 ? -10.0 : left(roots[1][1]) * 3.0)
    B = max(1,nr == 0 ? 10.0 : right(roots[nr][1]) * 3.0)
    DFun(f,fdf, Flt(A), Flt(B), coef_rat, nr)
end

function coef_legendre(n)
    p0 = BigInt[1]       # 0! P_0
    n == 0 && return p0

    p1 = BigInt[0, 1]    # 1! P_1
    n == 1 && return p1

    for k in 2:n
        p2 = zeros(BigInt, k + 1)

        # (2k-1) x p1
        for i in eachindex(p1)
            p2[i + 1] += (2k - 1) * p1[i]
        end

        # -(k-1)^2 p0
        for i in eachindex(p0)
            p2[i] -= (k - 1)^2 * p0[i]
        end

        p0, p1 = p1, p2
    end

    Vector{Rational{BigInt}}(p1)
end

function legendre(n)
    function f(x::Flt)::Flt
        if n == 0
            return 1.0
        elseif n == 1
            return x
        end

        # calcul de P_(n-1)
        p0 = 1.0
        p1 = x

        for k in 2:n
            p2 = ((2k-1)*x*p1 - (k-1)*p0)/k
            p0 = p1
            p1 = p2
        end

        return p1
    end
    function fdf(x::Flt)::Tuple{Flt,Flt}
        if n == 0
            return 1.0, 0.0
        elseif n == 1
            return x, 1.0
        end

        # calcul de P_(n-1)
        p0 = 1.0
        p1 = x

        for k in 2:n
            p2 = ((2k-1)*x*p1 - (k-1)*p0)/k
            p0 = p1
            p1 = p2
        end

        if abs(x) == 1.0
            return p1, n*(n+1)/2 * (x > 0 ? 1 : (-1)^(n+1))
        end

        return p1, n*(x*p1 - p0)/(x*x-1)
    end
    DFun(f,fdf,Flt(-30.0),Flt(30.0),coef_legendre(n),n)
end

#F = norm_fun(wilkinson(10); coef=10000)
#F = wilkinson(10)
#F = norm_fun(chebyshev(100); coef=1000)
#F = chebyshev(100)
#F = random(3)
#F = mignotte(20,16)
#r, nr, cc = isolate(F, F.A, F.B; refine=true, bound = 0.2, nb_samples = 3)
#println(F)
#println(nr, " ", cc, " ", r)
#STOP

# count_try = 0
# while(true)
#     B = mignotte_bound(9,16)
#     r, nr, count = isolate(mignotte(9,16), -B, B; refine=false, bound = 0.1, nb_samples = 3)
#     println("RESULT: ", nr, " ", count, " ", r)
#     global count_try += 1
#     if (nr < 3) break end
# end
# println(count_try)
# exit(1)

p = benchmark_isolate(
    "random",
    random,
    collect(3:1:80);
    bound=0.15
)

readline()

p = benchmark_isolate(
    "mignote_16",
    n->mignotte(n,16),
    collect(3:1:60);
    bound=0.15
)

readline()

p = benchmark_isolate(
    "mignote_32",
    n->mignotte(n,32),
    collect(3:1:30);
    bound=0.15
)

readline()

p = benchmark_isolate(
    "chebyshev",
    chebyshev,
    collect(2:1:100);
    bound=0.2
)

readline()

p = benchmark_isolate(
    "asinh_chebyshev",
    n->norm_fun(chebyshev(n); coef=10),
    collect(2:1:100);
    bound=0.1, nb_samples = 6
)

readline()

p = benchmark_isolate(
    "legendre",
    legendre,
    collect(2:1:100);
    bound=0.2
)

readline()

p = benchmark_isolate(
    "asinh_legendre",
    n->norm_fun(legendre(n); coef=10),
    collect(2:1:100);
    bound=0.1, nb_samples = 6
)

readline()

p = benchmark_isolate(
    "geometric",
    geometric,
    collect(2:1:33);
    bound=0.2
)

readline()

p = benchmark_isolate(
    "asinh_geometric",
    n->norm_fun(geometric(n); coef=10),
    collect(2:1:33);
    bound=0.15
)

readline()

p = benchmark_isolate(
    "wilkinson",
    wilkinson,
    collect(3:1:100);
    bound=0.2
)

readline()

p = benchmark_isolate(
    "asinh_wilkinson",
    n->norm_fun(wilkinson(n); coef=1e20),
    collect(2:1:100);
    bound=0.015, nb_samples = 3
)

readline()

p = benchmark_isolate(
    "random_roots",
    random_roots,
    collect(3:1:100);
    bound=0.1
)

readline()

println("errors: ", total_errors, "/", total_tests)
