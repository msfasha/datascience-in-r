import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import poisson

# Values of lambda (average rate)
lambdas = [1, 3, 5]

# Values of k (number of events)
k_values = np.arange(0, 15)

# Plot the Poisson distribution for each lambda
for lam in lambdas:
    pmf = poisson.pmf(k_values, lam)
    plt.plot(k_values, pmf, label=f'λ={lam}')

plt.title('Poisson Distribution')
plt.xlabel('Number of Events (k)')
plt.ylabel('Probability')
plt.legend()
plt.grid(True)
plt.show()
