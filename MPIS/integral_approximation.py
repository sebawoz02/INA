from math import sin
from math import pi
from random import random
import numpy as np
import matplotlib.pyplot as plt


def f1(x):
    return x ** (1 / 3)


def f2(x):
    return sin(x)


def f3(x):
    return 4 * x * (1 - x) ** 3


def integral_approx(a, b, M, fx, n):
    """
    Funkcja obliczaja metoda probabilystczną aproksymacje calki - metoda Monte Carlo.

    :param a: dolna granica całkowania
    :param b: górna granica całkowania
    :param M: górna granica funkcji
    :param fx: funkcja całkowana
    :param n: ilośc punktów losowych
    :return aproksymacja calki
    """
    below = 0
    for i in range(n):
        x = a + (b - a) * random()
        y = M * random()
        if y < fx(x):
            below += 1
    return (below / n) * (b - a) * M


def plott(function, real_val, a, b, M):
    """
    Funkcja tworzaca wykres eksperymentu(50 powtorzen dla punktow o ilosci ze zbioru[50,100,...,5000]).

    :param function: funkcja na ktorej bedzie przeprowadzany eksperyment
    :param real_val: Prawdziwa wartosc całki z funkcji
    :param a: dolna granica całki
    :param b: górna granica całki
    :param M: gorna granica funkcji
    """
    f1_approx = []
    plot2x_array = []
    plot1x_array = []
    avg_values = []

    for m in range(50, 5001, 50):
        table = []
        table2 = []
        for k in range(50):
            table.append(integral_approx(a, b, M, function, m))
            table2.append(m)
        plot2x_array.append(table2)
        plot1x_array.append(m)
        f1_approx.append(table)
        avg_values.append(sum(table) / 50)

    plot1x_array = np.array(plot1x_array)
    f1_approx = np.array(f1_approx)
    n_array = np.array(plot2x_array)
    x1 = np.array([0, 5000])
    y1 = np.array([real_val, real_val])

    plt.scatter(n_array, f1_approx, 0.3)
    plt.scatter(plot1x_array, avg_values, 10, "#ff2800")
    plt.plot(x1, y1, "g-")
    plt.show()


plott(f1, 12, 0, 8, 2)  # wykres dla funkcji nr1
plott(f2, 2, 0, pi, 1)  # wykres dla funkcji nr2
plott(f3, 0.2, 0, 1, 27 / 64)  # wykres dla funkcji nr3
