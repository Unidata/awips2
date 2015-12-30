import Scientific_numerics_package_id
package = Scientific_numerics_package_id.getNumericsPackageName()
del Scientific_numerics_package_id

if package == "Numeric":

    from RNG import *

elif package == "NumPy":

    from numpy.oldnumeric.rng import *

else:

    raise ImportError("Unknown numerics package " + package)
