from math import pi
from math import sqrt
from random import random
import numpy as np
import matplotlib.pyplot as plt


def pi_approx(n):
    """ Funkcja obliczaja metoda probabilystczną aproksymacje liczby pi. Losowane sa punkty w kwadracie z
    wpisanym okregiem ze srodkiem w punkcie (1,1). Iloraz punktow w okregu podzielona przez liczbe wszystkich punktow
    pomnozony przez 4 da przyblizona wartosc liczby pi.

    :param n: ilosc losowanych punktow. Im wiecej punktow tym dokladniejszy wynik.
    :return: przyblizona wartosc liczby pi
    """
    inside = 0      # punkty ktore byly wewnatrz okregu
    for i in range(n):
        x = 2 * random()
        y = 2 * random()
        distance = sqrt(((1-x)**2)+(1-y)**2)
        if distance <= 1:
            inside += 1
    return 4*(inside/n)


approx = []
secondplotx_array = []
firstplotx_array = []
avg_values = []

for m in range(50, 5001, 50):
    table = []
    table2 = []
    for k in range(50):
        table.append(pi_approx(m))
        table2.append(m)
    secondplotx_array.append(table2)
    firstplotx_array.append(m)
    approx.append(table)
    avg_values.append(sum(table) / 50)

firstplotx_array = np.array(firstplotx_array)
approx = np.array(approx)
n_array = np.array(secondplotx_array)
x1 = np.array([0, 5000])
y1 = np.array([pi, pi])

plt.scatter(n_array, approx, 0.3)
plt.scatter(firstplotx_array, avg_values, 10, "#ff2800")
plt.plot(x1, y1, "g-")
plt.show()

