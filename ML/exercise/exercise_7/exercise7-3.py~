import random
import numpy as np
import matplotlib.pyplot as plt

# matplotlib init
fig = plt.figure()
ax = fig.add_axes([0,0,1,1])
possibleThrows = np.arange(11) + 2 # [2..12] 

def rollDice(n):
    rolls = [random.randint(1,6) + random.randint(1,6) for x in range(n)]
    plt.title(str(n) + "rolls")
    plt.hist(rolls, density=False, bins=21)
    plt.show()

# Rolling dice
rollDice(10)
rollDice(50)
rollDice(100)
rollDice(250)
rollDice(500)
rollDice(1000)
rollDice(10000)
