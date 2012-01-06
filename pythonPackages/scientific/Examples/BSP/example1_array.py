from Scientific.BSP import ParSequence, ParFunction, ParRootFunction
import Numeric; N = Numeric
import operator

# The local computation function.
def square(numbers):
    return numbers**2

# The global computation function.
global_square = ParFunction(square)

# The local output function
def output(result):
    print result

# The global output function - active on processor 0 only.
global_output = ParRootFunction(output)

# A list of numbers distributed over the processors.
items = ParSequence(N.arange(100))

# Computation.
results = global_square(items)

# Collect results on processor 0.
all_results = results.reduce(lambda a, b: N.concatenate((a, b)), N.zeros((0,)))

# Output from processor 0.
global_output(all_results)
