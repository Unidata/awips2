from Scientific.BSP import ParSequence, ParFunction, ParRootFunction
import operator

# The local computation function.
def square(numbers):
    return [x*x for x in numbers]

# The global computation function.
global_square = ParFunction(square)

# The local output function
def output(result):
    print result

# The global output function - active on processor 0 only.
global_output = ParRootFunction(output)

# A list of numbers distributed over the processors.
items = ParSequence(range(100))

# Computation.
results = global_square(items)

# Collect results on processor 0.
all_results = results.reduce(operator.add, [])

# Output from processor 0.
global_output(all_results)
