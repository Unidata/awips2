# Definitions to help evaluating potentials and their
# gradients using DerivVars and DerivVectors.
#
# Written by: Konrad Hinsen <hinsen@cnrs-orleans.fr>
# Last revision: 2006-6-12
# 

"""
Potential energy functions with automatic gradient evaluation

This module offers two strategies for automagically calculating the
gradients (and optionally force constants) of a potential energy
function (or any other function of vectors, for that matter).  The
more convenient strategy is to create an object of the class
PotentialWithGradients. It takes a regular Python function object
defining the potential energy and is itself a callable object
returning the energy and its gradients with respect to all arguments
that are vectors.

Example::

  >>>def _harmonic(k,r1,r2):
  >>>    dr = r2-r1
  >>>    return k*dr*dr
  >>>harmonic = PotentialWithGradients(_harmonic)
  >>>energy, gradients = harmonic(1., Vector(0,3,1), Vector(1,2,0))
  >>>print energy, gradients

prints::

  >>>3.0
  >>>[Vector(-2.0,2.0,2.0), Vector(2.0,-2.0,-2.0)]

The disadvantage of this procedure is that if one of the arguments is a
vector parameter, rather than a position, an unnecessary gradient will
be calculated. A more flexible method is to insert calls to two
function from this module into the definition of the energy
function. The first, DerivVectors(), is called to indicate which
vectors correspond to gradients, and the second, EnergyGradients(),
extracts energy and gradients from the result of the calculation.
The above example is therefore equivalent to::

  >>>def harmonic(k, r1, r2):
  >>>    r1, r2 = DerivVectors(r1, r2)
  >>>    dr = r2-r1
  >>>    e = k*dr*dr
  >>>    return EnergyGradients(e,2)

To include the force constant matrix, the above example has to be
modified as follows::

  >>>def _harmonic(k,r1,r2):
  >>>    dr = r2-r1
  >>>    return k*dr*dr
  >>>harmonic = PotentialWithGradientsAndForceConstants(_harmonic)
  >>>energy, gradients, force_constants = harmonic(1.,Vector(0,3,1),Vector(1,2,0))
  >>>print energy
  >>>print gradients
  >>>print force_constants

The force constants are returned as a nested list representing a
matrix. This can easily be converted to an array for further
processing if the numerical extensions to Python are available.
"""

from Scientific import N; Numeric = N
from Scientific.Geometry import Vector, isVector
from Scientific.Functions import FirstDerivatives, Derivatives


def DerivVectors(*args):
    index = 0
    list = []
    for a in args:
        list.append(FirstDerivatives.DerivVector(a.x(),a.y(),a.z(),index))
        index = index + 3
    return tuple(list)

def EnergyGradients(e, n):
    energy = e[0]
    deriv = e[1]
    deriv = deriv + (3*n-len(deriv))*[0]
    gradients = []
    i = 0
    for j in range(n):
        gradients.append(Vector(deriv[i],deriv[i+1],deriv[i+2]))
        i = i+3
    return (energy, gradients)

def EnergyGradientsForceConstants(e, n):
    energy = e[0]
    deriv = e[1]
    deriv = deriv + (3*n-len(deriv))*[0]
    gradients = []
    i = 0
    for j in range(n):
        gradients.append(Vector(deriv[i],deriv[i+1],deriv[i+2]))
        i = i+3
    return (energy, gradients, Numeric.array(e[2]))


# Potential function class with gradients

class PotentialWithGradients:

    def __init__(self, func):
        self.func = func

    def __call__(self, *args):
        arglist = []
        nvector = 0
        index = 0
        for a in args:
            if isVector(a):
                arglist.append(FirstDerivatives.DerivVector(a.x(),a.y(),a.z(),
                                                            index))
                nvector = nvector + 1
                index = index + 3
            else:
                arglist.append(a)
        e = apply(self.func, tuple(arglist))
        return EnergyGradients(e,nvector)


# Potential function class with gradients and force constants

class PotentialWithGradientsAndForceConstants:

    def __init__(self, func):
        self.func = func

    def __call__(self, *args):
        arglist = []
        nvector = 0
        index = 0
        for a in args:
            if isVector(a):
                arglist.append(Derivatives.DerivVector(a.x(),a.y(),a.z(),
                                                       index, 2))
                nvector = nvector + 1
                index = index + 3
            else:
                arglist.append(a)
        e = apply(self.func, tuple(arglist))
        return EnergyGradientsForceConstants(e, nvector)
