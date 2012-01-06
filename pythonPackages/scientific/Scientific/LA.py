import Scientific_numerics_package_id
package = Scientific_numerics_package_id.getNumericsPackageName()
del Scientific_numerics_package_id

if package == "Numeric":

    from LinearAlgebra import *

elif package == "NumPy":

    from numpy.oldnumeric.linear_algebra import *

elif package == "Numarray":

    from numarray.linear_algebra import *

else:

    raise ImportError("Unknown numerics package " + package)
