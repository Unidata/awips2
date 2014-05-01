from Scientific.BSP import ParData, ParSequence, ParAccumulator, \
                           ParFunction, ParRootFunction, numberOfProcessors
import operator

# Local and global computation functions.
def makepairs(sequence1, sequence2):
    pairs = []
    for item1 in sequence1:
        for item2 in sequence2:
            pairs.append((item1, item2))
    return pairs
global_makepairs = ParFunction(makepairs)

# Local and global output functions.
def output(result):
    print result
global_output = ParRootFunction(output)

# A list of data items (here letters) distributed over the processors.
my_items = ParSequence('abcdef')

# The number of the neighbour to the right (circular).
neighbour_pid = ParData(lambda pid, nprocs: [(pid+1)%nprocs])

# Loop to construct all pairs.
pairs = ParAccumulator(operator.add, [])
pairs.addValue(global_makepairs(my_items, my_items))
other_items = my_items
for i in range(numberOfProcessors-1):
    other_items = other_items.put(neighbour_pid)[0]
    pairs.addValue(global_makepairs(my_items, other_items))

# Collect results on processor 0.
all_pairs = pairs.calculateTotal()

# Output results from processor 0.
global_output(all_pairs)
