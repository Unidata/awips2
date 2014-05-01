import Scientific_numerics_package_id
package = Scientific_numerics_package_id.getNumericsPackageName()
del Scientific_numerics_package_id

if package == "Numeric":
    from Numeric import *
    def int_sum(a, axis=0):
        return add.reduce(a, axis)
    def zeros_st(shape, other):
        return zeros(shape, other.typecode())
    array_type = ArrayType

elif package == "NumPy":

    from numpy.oldnumeric import *
    def int_sum(a, axis=0):
        return add.reduce(a, axis)
    def zeros_st(shape, other):
        return zeros(shape, dtype=other.dtype)
    from numpy import ndarray as array_type

elif package == "Numarray":

    from numarray import *
    def int_sum(a, axis=0):
        return add.reduce(a, axis, type=Int)
    def zeros_st(shape, other):
        return zeros(shape, other.typecode())

else:

    raise ImportError("Unknown numerics package " + package)
