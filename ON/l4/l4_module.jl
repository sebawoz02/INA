# Sebastian Woźniak 268491
# Moduł z funkcjami z zadań 1, 2, 3, 4.

module L4_Module
export ilorazyRoznicowe, warNewton, naturalna, rysujNnfx 

    using Plots

    function ilorazyRoznicowe(x::Vector{Float64}, f::Vector{Float64})
        # Liczy ilorazy różnicowe wielomianu interpolującego funkcję w punktach x
        #
        # @param x - wektor o długości n+1 zawierający węzły x_1, x_2,..., x_{n+1}
        # @param f - wektor o długości n+1 zawierający wartości interpolowanej funkcji 
        #            w węzłach f(x0), . . . , f(xn)
        #
        # @return fx - wektor o długości n+1 zawierające obliczone ilorazy różnicowe
        n = length(x)
        fx = copy(f)
        
        for i in 2:n
            for j in n:-1:i
                fx[j] = (fx[j]-fx[j-1])/(x[j]-x[j-i+1])
            end
        end
        return fx
    end

    function warNewton(x::Vector{Float64}, fx::Vector{Float64}, t::Float64)
        # Liczy wartość wielomianu interpolacyjnego w postaci Newtona 
        # w punkcie t za pomocą schematu Hornera.
        #
        # @param x  - wektor o długości n+1 zawierający węzły x_0,..., x_n
        # @param fx - wektor długości n+1 zawierający ilorazy różnicowe
        # @param t  - punkt, w którym należy obliczyć wartość wielomianu
        #
        # @return nt - wartość wielomianu w punkcie t
        
        n = length(x)
        nt = fx[n]
        
        for i in n-1:-1:1
            nt = fx[i] + (t-x[i]) * nt
        end
    
        return nt
    end

    function naturalna(x::Vector{Float64}, fx::Vector{Float64})
        # Liczy współczynniki postaci naturalnej interpolowanego wielomianu na 
        # podstawie węzłów i ilorazów w czasie O(n^2)
        #
        # @param x  - wektor długości n+1 zawierający węzły x_0, x_1,..., x_n
        # @param fx - wektor długości n+1 zawierający ilorazy różnicowe
        #
        # @return a - wektor długości n+1 zawierający obliczone współczynniki 
        #             postaci naturalnej
        n = length(x)
        a = zeros(n)
        a[n] = fx[n]
        for i in (n-1):-1:1
            a[i] = fx[i] - x[i] * a[i+1]
            for j in (i+1):(n-1)
                a[j] += -x[i] * a[j+1]
            end
        end
        return a
    end

    function rysujNnfx(f,a::Float64,b::Float64,n::Int)
        # Interpoluje zadaną funkcję f(x) w przedziale [a, b] za pomocą wielomianu 
        # interpolacyjnego stopnia n w postaci Newtona. Następnie rysuje wielomian 
        # interpolacyjny i interpolowaną funkcje.
        #
        # @param f - funkcja f(x) zadana jako anonimowa funkcja
        # @param a - początek przedziału interpolacji
        # @param b - koniec przedziału interpolacji
        # @param n - stopień wielomianu interpolacyjnego
        if a > b
            return
        end

        x = zeros(n+1)  # węzły
        y = zeros(n+1)  # wartość funkcji f w węzłach
        h = (b-a)/n     # odległość między węzłami
        for k in 0:n
            x[k+1] = a + k*h
            y[k+1] = f(x[k+1])
        end

        c = ilorazyRoznicowe(x, y)
        no_points = 30 *n
        h = (b-a)/(no_points)

        plot_x = zeros(no_points+1)
        intp = zeros(no_points+1)
        func = zeros(no_points+1)

        plot_x[1] = a
        intp[1] = func[1] = y[1]
        for i in 2:no_points+1
            plot_x[i] = plot_x[i-1] + h
            intp[i] = warNewton(x, c, plot_x[i])
            func[i] = f(plot_x[i])
        end

        plot(
            plot(plot_x, intp, label="wielomian interpolacyjny", title="n = $n", color="green"),
            plot(plot_x, func, label="funkcja", color="blue"),
            plot(plot_x, [intp func], label=["wielomian interpolacyjny" "funkcja"], color=["green" "blue"]),
            layout = (3, 1),
            legend = true
        )
        savefig("my_plot_$n.png")
    end
end