# Subpackage Scientific.Geometry
#
# Written by: Konrad Hinsen <hinsen@cnrs-orleans.fr>
# Last revision: 2006-5-26
#

"""
Geometrical quantities and objects

The geometrical quantities are vectors and tensors, transformations,
and quaternions as descriptions of rotations. There are also tensor
fields, which were included here (rather than in
L{Scientific.Functions}) because they are most often used in a
geometric context. Finally, there are classes for elementary
geometrical objects such as spheres and planes.

@undocumented: VectorModule*
@undocumented: TensorModule*
"""

# Pretend that Vector and Tensor are defined directly
# in Scientific.Geometry.

try:
    import Scientific_vector
    Vector = Scientific_vector.Vector
    isVector = Scientific_vector.isVector
    del Scientific_vector
except ImportError:
    import VectorModule
    Vector = VectorModule.Vector
    isVector = VectorModule.isVector
    del VectorModule

import TensorModule
Tensor = TensorModule.Tensor
isTensor = TensorModule.isTensor
del TensorModule

# Some useful constants
ex = Vector(1., 0., 0.)
ey = Vector(0., 1., 0.)
ez = Vector(0., 0., 1.)
nullVector = Vector(0., 0., 0.)
delta = Tensor([[1, 0, 0],
                [0, 1, 0],
                [0, 0, 1]])
epsilon = Tensor([[[ 0,  0,  0],
                   [ 0,  0,  1],
                   [ 0, -1,  0]],
                  [[ 0,  0, -1],
                   [ 0,  0,  0],
                   [ 1,  0,  0]],
                  [[ 0,  1,  0],
                   [-1,  0,  0],
                   [ 0,  0,  0]]])


import sys
if sys.modules.has_key('epydoc'):
    import VectorModule, TensorModule
    Vector = VectorModule.Vector
    isVector = VectorModule.isVector
    vm_name = VectorModule.__name__
    tm_name = TensorModule.__name__
    Vector.__module__ = 'Scientific.Geometry'
    Tensor.__module__ = 'Scientific.Geometry'
    isVector.func_globals['__name__'] = 'Scientific.Geometry'
    isTensor.func_globals['__name__'] = 'Scientific.Geometry'
    VectorModule.__name__ = vm_name
    TensorModule.__name__ = tm_name
    del VectorModule
    del TensorModule
    del vm_name
    del tm_name
del sys
