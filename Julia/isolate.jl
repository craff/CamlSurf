struct Hermite3
    fa::Float64
    A::Float64
    B::Float64
    C::Float64
end

struct DFun{F,G}
    f::F
    df::G
end

function hermite3_make(fn::DFun, a::Float64, fa, dfa,
                                 b::Float64, fb, dfb)

    c = b-a

    A = dfa
    B = (3*(fb-fa)-c*(2*dfa+dfb))/(c*c)
    C = (c*(dfa+dfb)-2*(fb-fa))/(c*c*c)

    return Hermite3(fa, A, B, C)
end

function WH(fn::DFun, f3, x::Float64, a::Float64, b::Float64)
    fx = fn.f(x)
    dfx = fn.df(x)
    u = (x - a)
    f3x = ((f3.C * u + f3.B) * u + f3.A) * u + f3.fa
    df3x = (3*f3.C * u + 2*f3.B) * u + f3.A
    D = abs(fx) + abs(f3x)
    R1 = abs(f3x-fx)/D
    R2 = abs(df3x-dfx)*(b-a)/D
    #W = abs((df3x-dfx)/fx)
    return R1, R2, fx, dfx
end

function isolate(fn::DFun, A::Float64, B::Float64;
                 bound1 = .33, bound2 = 2., alea=1e-2)
    roots = Tuple{Float64,Float64}[]
    count = 0
    function loop(a::Float64, fa::Float64, dfa,
                  b::Float64, fb::Float64, dfb)
        #println("loop:", a, " ", fa," ", b," ", fb)
        H = hermite3_make(fn,a,fa,dfa,b,fb,dfb)
        D = H.B*H.B - 3*H.A*H.C;
        x1 = 0.0; x2 = 0.0
	if (H.B > 0 && D >= 0)
	    tmp = -H.B - sqrt(D);
	    x1 = a + tmp/(3*H.C); x2 = a + H.A/tmp;
        elseif (D >= 0)
	    tmp = -H.B + sqrt(D);
	    x1 = a + tmp/(3*H.C); x2 = a + H.A/tmp;
	end
        t = (rand()-0.5)*alea + 0.5
        x3 = t*a + (1-t)*b
        if (x1 > x2)
            (x1,x2) = (x2,x1)
        end
        test1 = !(a < x1 && x1 < b)
        test2 = !(a < x2 && x2 < b)
        if (test1 && test2)
            t = (rand()-0.5)*alea + 0.75
            x1 = t*a + (1-t)*b
            t = (rand()-0.5)*alea + 0.25
            x2 = t*a + (1-t)*b
        end
        if (test1)
            t = (rand()-0.5)*alea + ((x2 < x3) ? 0.25 : 0.75)
            x1 = t*a + (1-t)*b
        end
        if (test2)
            t = (rand()-0.5)*alea + ((x1 < x3) ? 0.25 : 0.75)
            x2 = t*a + (1-t)*b
        end
        if (x1 > x2)
            (x1,x2) = (x2,x1)
        end
        R11, R12, fx1, dfx1 = WH(fn,H,x1,a,b)
        R21, R22, fx2, dfx2 = WH(fn,H,x2,a,b)
        R31, R32, fx3, dfx3 = WH(fn,H,x3,a,b)
        #println("xs:",x1," ", fx1, " ", R1, "\n",
        #          x2," ", fx2, " ", R2, "\n",
        #          x3," ", fx3, " ", R3)
        bad = !(R11 < bound1) || !(R12 < bound2) ||
              !(R21 < bound1) || !(R22 < bound2) ||
              !(R31 < bound1) || !(R32 < bound2)
        if (bad && (x3 != a && x3 != b))
            # if abs(fx1) > abs(fa) && abs(fx1) > abs(fb)
            #     if abs(fx2) > abs(fx1)
            #         loop(a,fa,x2,fx2)
            #         loop(x2,fx2,b,fb)
            #     else
            #         loop(a,fa,x1,fx1)
            #         loop(x1,fx1,b,fb)
            #     end
            # elseif abs(fx2) > abs(fa) && abs(fx2) > abs(fb)
            #     loop(a,fa,x2,fx2)
            #     loop(x2,fx2,b,fb)
            # else
                loop(a,fa,dfa,x3,fx3,dfx3)
                loop(x3,fx3,dfx3,b,fb,dfb)
            #end
        else
            count += 1
            if (a < x1 && x1 < b)
                if (fa*fx1 < 0)
                    push!(roots, (a,x1))
                end
                if (fx1 == 0.0)
                    push!(roots, (x1,x1))
                end
                if (a < x2 && x2 < b)
                    if (fx1*fx2 < 0)
                        push!(roots, (x1,x2))
                    end
                    if (fx2 == 0.0)
                        push!(roots, (x2,x2))
                    end
                    if (fx2*fb < 0)
                        push!(roots, (x2,b))
                    end
                elseif (fx1 * fb < 0)
                    push!(roots, (x1,b))
                end
            elseif (a < x2 && x2 < b)
                if (fa*fx2 < 0)
                    push!(roots, (a,x2))
                end
                if (fx2 == 0.0)
                    push!(roots, (x2,x2))
                end
                if (fx2*fb < 0)
                    push!(roots, (x2,b))
                end
            elseif (fa * fb < 0)
                push!(roots, (a,b))
            end
            if (fb == 0.0)
                push!(roots, (b,b))
            end
        end
    end

    loop(A,fn.f(A),fn.df(A),
         B,fn.f(B),fn.df(B))
    return roots,  length(roots), count
end

using Plots
using Statistics

total_tests = 0
total_errors = 0

function benchmark_isolate(msg,make_poly, ns, a, b, expect)
    println(msg)
    global total_tests
    global total_errors
    leaves = Float64[]
    times = Float64[]
    nbroots = Int[]

    f = make_poly(ns[1])
    isolate(f, a(ns[1]), b(ns[1]))

    nb_tests = 0
    nb_errors = 0

    for n in ns
        f = make_poly(n)

        counts = Int[]
        ts = Float64[]
        r = 0
        e = expect(n)
        for i in 1:10
            t = @elapsed begin
                r, nr, count = isolate(f, a(n), b(n))
            end
            nb_tests += 1
            if length(r) != e
                nb_errors += 1
            end
            push!(ts,t)
            push!(counts,count)
        end
        t = median(ts)
        count = median(counts)
        push!(leaves, count)
        push!(times, t*1000.0)
        rs = length(r)
        println("n=$n  roots=$rs leaves=$count  time=$t")
    end

    p = plot(ns, leaves,
             xlabel="degree n",
             ylabel="subdivisions",
             marker=:circle,
             label="subdivisions")

    plot!(twinx(),
          ns, times,
          ylabel="time (ms)",
          marker=:square,
          label="time")

    total_tests += nb_tests
    total_errors += nb_errors
    println("errors: ", nb_errors, "/", nb_tests)
    return p
end

function chebyshev(n)
    function f(x::Float64)::Float64
        if abs(x) <= 1
            return cos(n * acos(x))
        elseif x > 1
            return cosh(n * acosh(x))
        else
            return (-1)^n * cosh(n * acosh(-x))
        end
    end

    function df(x::Float64)::Float64
        if abs(x) < 1
            return n * sin(n * acos(x)) / sqrt(1 - x*x)
        elseif x > 1
            return n * sinh(n * acosh(x)) / sqrt(x*x - 1)
        elseif x < -1
            return (-1)^(n-1) * n * sinh(n * acosh(-x)) / sqrt(x*x - 1)
        elseif x == 1
            return n*n
        else # x == -1
            return (-1)^(n-1) * n*n
        end
    end
    return DFun(f,df)
end

function mignotte(n,p)
    function f(x::Float64)::Float64
        return x^n - 2*(2^p*x - 1)^2
    end

    function df(x::Float64)::Float64
        return n*x^(n-1) - 4*2^p*(2^p*x - 1)
    end
    return DFun(f,df)
end

function wilkinson(n)
    function f(x::Float64)::Float64
        s = 1.0
        for k in 1:n
            s *= x-k
        end
        return s
    end

    function df(x::Float64)::Float64
        p = 1.0
        dp = 0.0
        for k in 1:n
            dp += p + dp*(x-k)
            p *= x - k
        end
        return dp
    end

    return DFun(f,df)
end

function geometric(n)
    function f(x::Float64)::Float64
        p = 1.0
        r = 1.0
        for k in 1:n
            p *= x-r
            r *= 0.5
        end
        p
    end

    function df(x::Float64)::Float64
        p = 1.0
        dp = 0.0
        r = 1.0
        for k in 1:n
            dp = dp*(x-r) + p
            p *= x-r
            r *= 0.5
        end
        dp
    end

    DFun(f,df)
end

function legendre(n)
    function f(x::Float64)::Float64
        if n == 0
            return 1.0
        elseif n == 1
            return x
        end

        p0 = 1.0
        p1 = x

        for k in 2:n
            p2 = ((2k-1)*x*p1 - (k-1)*p0)/k
            p0 = p1
            p1 = p2
        end
        return p1
    end

    function df(x::Float64)::Float64
        if n == 0
            return 0.0
        elseif n == 1
            return 1.0
        end

        pn = f(x)

        # calcul de P_(n-1)
        p0 = 1.0
        p1 = x

        for k in 2:n-1
            p2 = ((2k-1)*x*p1 - (k-1)*p0)/k
            p0 = p1
            p1 = p2
        end

        pm1 = p1

        if abs(x) == 1.0
            return n*(n+1)/2 * (x > 0 ? 1 : (-1)^(n+1))
        end

        return n*(x*pn - pm1)/(x*x-1)
    end

    DFun(f,df)
end

function almost_double(eps)
    f(x)=(x^2-1)^2+eps*x
    df(x)=4*x*(x^2-1)+eps
    return DFun(f,df)
end

p = benchmark_isolate(
    "chebyshev",
    chebyshev,
    collect(2:1:100),
    n->-30.0,
    n-> 30.0,
    n->n
)

display(p)
readline()

p = benchmark_isolate(
    "legendre",
    legendre,
    collect(2:1:100),
    n->-30.0,
    n-> 30.0,
    n->n
)

display(p)
readline()

p = benchmark_isolate(
    "geometric",
    geometric,
    collect(2:1:33),
    n->-30.0,
    n-> 30.0,
    n->n
)

display(p)
readline()



function mignotte_bound(n,p)
    return max(2,2^((2*p+3)/(n-2)))
end

p = benchmark_isolate(
    "mignote_16: x^n - 2*(2^16 x - 1)^2",
    n->mignotte(n,16),
    collect(5:1:60),
    n -> -mignotte_bound(n,16),
    n -> mignotte_bound(n,16),
    n -> (n % 2 == 0) ? 4 : 3
)

display(p)
readline()

p = benchmark_isolate(
    "mignote_32: x^n - 2*(2^32 x - 1)^2",
    n->mignotte(n,32),
    collect(5:1:30),
    n -> -mignotte_bound(n,32),
    n -> mignotte_bound(n,32),
    n -> (n % 2 == 0) ? 4 : 3
)

display(p)
readline()

p = benchmark_isolate(
    "wilkinson",
    wilkinson,
    collect(5:1:100),
    n->float(-n),
    n->float(n),
    n->n
)

display(p)
readline()


println("errors: ", total_errors, "/", total_tests)
