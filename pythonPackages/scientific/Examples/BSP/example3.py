from Scientific.BSP import ParFunction, ParRootFunction, ParMessages, \
                           ParConstant, ParIterator, ParIndexIterator, \
                           numberOfProcessors
import operator, string

# The local and global input functions.
def input():
    data = open('numbers').readlines()
    numbers = map(string.atoi, map(string.strip, data))
    chunk_size = (len(numbers)+numberOfProcessors-1)/numberOfProcessors
    chunks = []
    for i in range(numberOfProcessors):
        chunks.append((i, numbers[i*chunk_size:(i+1)*chunk_size]))
    return chunks
def empty():
    return []
global_input = ParRootFunction(input, empty)

# The local and global computation functions.
def square(x):
    return x*x
global_square = ParFunction(square)

# The local and global output functions.
def output(results):
    file = open('results', 'a')
    for value in results:
        file.write(`value` + '\n')
    file.close()
global_output = ParRootFunction(output)

# Read input data.
data = global_input()

# Distribute input data.
items = ParMessages(data).exchange()[0]

# Computation and output loop.
for item in ParIterator(items):
    result = global_square(item)
    collected_results = result.put(ParConstant([0]))
    global_output(collected_results)
