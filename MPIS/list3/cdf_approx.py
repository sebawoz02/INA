from random import random
import matplotlib.pyplot as plt
import numpy as np
import scipy.stats as ss


def cdf_approx(_n):
    allsn = []
    for k in range(5000):
        sn = 0
        for n in range(_n):
            x = 0
            while x == 0:
                x = random() * 2 - 1
            if x < 0:
                x = -1
            else:
                x = 1
            sn += x
        allsn.append(sn)
    sndict = {}
    for s in allsn:
        if s in sndict.keys():
            sndict[s] += 1
        else:
            sndict[s] = 1

    # histograM
    plt.hist(allsn, density=True, cumulative=True, bins=len(sndict.keys()))
    x2 = np.linspace(min(allsn), max(allsn))
    y2 = ss.norm(loc=0, scale=(max(allsn)-min(allsn))/8).cdf(x2)
    plt.plot(x2, y2, color='red')
    plt.title(f"CDF  S_{_n}")
    plt.show()


'''
for N in range(5, 31, 5):
    cdf_approx(N)
'''
cdf_approx(10000)

