from scipy.stats import kendalltau
import pandas as pd

data = pd.read_csv('./data/data.csv')
x = data.PBoldog
y = data.PEgészs

kendalltau(x,y)
x = [1,2,3,4,5,6,7]
y = [1,3,6,2,7,4,5]

def sqrt(x):
    return x**0.5
def ConDis(x,y):
    n = len(x)
    conc = 0
    disc= 0
    x_ties = 0
    y_ties = 0

    for i in range(len(x)):
        for j in range(i+1, len(x)):
            if (x[i] < x[j]) and (y[i] < y[j]) or (x[i] > x[j]) and (y[i] > y[j]):
                conc = conc + 1
            elif (x[i] == x[j]) and (y[i] != y[j]):
                x_ties = x_ties + 1
            elif (x[i] != x[j]) and (y[i] == y[j]):
                y_ties = y_ties + 1
            elif (x[i] == x[j]) and (y[i] == y[j]):
                pass

            else:
                disc = disc + 1

    tau = (conc - disc) / sqrt((conc + disc + x_ties) * (conc + disc + y_ties))
    tau2, __  = kendalltau(x,y)

    tau2 == tau




