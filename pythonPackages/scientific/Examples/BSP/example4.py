from Scientific.BSP import ParClass, ParBase, numberOfProcessors
import Numeric, operator

class DistributedMatrix(ParBase):

    def __parinit__(self, pid, nprocs, matrix, distribution_mode):
        self.full_shape = matrix.shape
        self.mode = distribution_mode
        if distribution_mode == 'row':
            chunk_size = (matrix.shape[0]+numberOfProcessors-1) \
                         / numberOfProcessors
            self.submatrix = matrix[pid*chunk_size:(pid+1)*chunk_size, :]
        elif distribution_mode == 'column':
            chunk_size = (matrix.shape[1]+numberOfProcessors-1) \
                         / numberOfProcessors
            self.submatrix = matrix[:, pid*chunk_size:(pid+1)*chunk_size]
        else:
            raise ValueError, "undefined mode " + distribution_mode

    def __init__(self, local_submatrix, full_shape, distribution_mode):
        self.submatrix = local_submatrix
        self.full_shape = full_shape
        self.mode = distribution_mode

    def __repr__(self):
        return "\n" + repr(self.submatrix)

    def __add__(self, other):
        if self.full_shape == other.full_shape and self.mode == other.mode:
            return DistributedMatrix(self.submatrix+other.submatrix,
                                     self.full_shape, self.mode)
        else:
            raise ValueError, "incompatible matrices"

    def __mul__(self, other):
        if self.full_shape[1] != other.full_shape[0]:
            raise ValueError, "incompatible matrix shapes"
        if self.mode == 'row' or other.mode == 'column':
            raise ValueError, "not implemented"
        product = Numeric.dot(self.submatrix, other.submatrix)
        full_shape = product.shape
        chunk_size = (full_shape[0]+numberOfProcessors-1)/numberOfProcessors
        messages = []
        for i in range(numberOfProcessors):
            messages.append((i, product[i*chunk_size:(i+1)*chunk_size, :]))
        data = self.exchangeMessages(messages)
        sum = reduce(operator.add, data, 0)
        return DistributedMatrix(sum, full_shape, 'row')

gDistributedMatrix = ParClass(DistributedMatrix)

m = Numeric.resize(Numeric.arange(10), (8, 8))
v = Numeric.ones((8, 1))
matrix = gDistributedMatrix(m, 'column')
vector = gDistributedMatrix(v, 'row')
product = matrix*vector

print product

