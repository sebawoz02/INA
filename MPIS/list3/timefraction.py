from random import random
from time import time
import matplotlib.pyplot as plt
import scipy.stats as ss
import numpy as np


def approx_pn(_n):
    S = 0
    _L = 0
    for N in range(1, _n+1):
        x = 0
        while x == 0:
            x = random() * 2 - 1
        if x < 0:
            x = -1
        else:
            x = 1
        S += x
        if S >= 0:
            _L += 1
    return _L


p200000 = []
for k in range(10000):
    print(k)
    L = approx_pn(200000)
    p200000.append(L / 200000)
rv = ss.arcsine()
distribution = np.linspace(0, np.minimum(rv.dist.b, 3))

plt.hist(p200000, bins=20, density=True)
plt.title('P200000 PDF')
plt.plot(distribution, rv.pdf(distribution))
plt.show()
