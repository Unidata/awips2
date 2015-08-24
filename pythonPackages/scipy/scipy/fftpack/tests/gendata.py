<<<<<<< HEAD
import numpy as np
from scipy.io import loadmat

m = loadmat('test.mat', squeeze_me=True,  struct_as_record=True,
=======
from __future__ import division, print_function, absolute_import

import numpy as np
from scipy.io import loadmat

m = loadmat('test.mat', squeeze_me=True, struct_as_record=True,
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        mat_dtype=True)
np.savez('test.npz', **m)
