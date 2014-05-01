# This module provides a class representing scalar, vector, and tensor fields.
#
# Written by Konrad Hinsen <hinsen@cnrs-orleans.fr>
# last revision: 2007-6-12
#

"""
Vector and tensor fields with derivatives
"""

from Scientific import N; Numeric = N
from Scientific.Geometry import Vector, Tensor
from Scientific.indexing import index_expression
from Scientific.Functions import Interpolation

#
# General tensor field base class
#
class TensorField(Interpolation.InterpolatingFunction):

    """Tensor field of arbitrary rank

    A tensor field is described by a tensor at each point of
    a three-dimensional rectangular grid. The grid spacing must
    be uniform. Tensor fields are implemented as a subclass
    of InterpolatingFunction from the module
    Scientific.Functions.Interpolation and thus share all methods
    defined in that class.

    Evaluation:

     - 'tensorfield(x, y, z)'   (three coordinates)
     - 'tensorfield(coordinates)'  (sequence containing three coordinates)
    """

    def __init__(self, rank, axes, values, default = None,
                 period = (None, None, None), check = True):
        """
        @param rank: the tensor rank
        @type rank: C{int}

        @param axes: three arrays specifying the axis ticks for the three
                     axes
        @type axes: sequence of C{Numeric.array} of rank 1

        @param values: an array containing the field values. Its first
                       three dimensions correspond to the x, y, z
                       directions and must have lengths compatible with
                       the axis arrays. The remaining dimensions must
                       have length 3.
        @type values: C{Numeric.array} of rank+3 dimensions

        @param default: the value of the field for points outside the grid.
                        A value of 'None' means that an exception will be
                        raised for an attempt to evaluate the field outside
                        the grid. Any other value must a tensor of the
                        correct rank.
        @type default: L{Scientific.Geometry.Tensor} or C{NoneType}

        @param period: the period for each of the variables, or C{None} for
            variables in which the function is not periodic.
        @type period: sequence of length three

        @raise ValueError: if the arguments are not consistent
        """
        if check:
            if len(axes) != 3:
                raise ValueError('Field must have three axes')
            if len(period) != 3:
                raise ValueError('Field requires three period values')
            if len(values.shape) != 3 + rank:
                raise ValueError('Values must have rank ' + `rank`)
            if values.shape[3:] != rank*(3,):
                raise ValueError('Values must have dimension 3')
        self.rank = rank
        self.spacing = []
        for axis in axes:
            d = axis[1:]-axis[:-1]
            self.spacing.append(d[0])
            if check:
                dmin = Numeric.minimum.reduce(d)
                if abs(dmin-Numeric.maximum.reduce(d)) > 0.0001*dmin:
                    raise ValueError('Grid must be equidistant')
        Interpolation.InterpolatingFunction.__init__(self, axes, values,
                                                     default, period)

    def __call__(self, *points):
        if len(points) == 1:
            points = tuple(points[0])
        value = apply(Interpolation.InterpolatingFunction.__call__,
                      (self, ) + points)
        if self.rank == 0:
            return value
        elif self.rank == 1:
            return Vector(value)
        else:
            return Tensor(value)

    def __getitem__(self, index):
        if isinstance(index, int):
            index = (index,)
        rank = self.rank - len(index)
        if rank < 0:
            raise ValueError('Number of indices too large')
        index = index_expression[...] + index + rank*index_expression[::]
        try: default = self.default[index]
        except TypeError: default = None
        if rank == 0:
            return ScalarField(self.axes, self.values[index], default,
                               self.period, False)
        elif rank == 1:
            return VectorField(self.axes, self.values[index], default,
                               self.period, False)
        else:
            return TensorField(self.axes, rank, self.values[index], default,
                               self.period, False)

    def zero(self):
        """
        @returns: a tensor of the same rank as the field values
                  with all elements equal to zero
        @rtype: L{Scientifc.Geometry.Tensor}
        """
        if self.rank == 0:
            return 0.
        else:
            return Tensor(Numeric.zeros(self.rank*(3,), Numeric.Float))

    def derivative(self, variable):
        """
        @param variable: 0 for x, 1 for y, 2 for z
        @type variable: C{int}
        @returns: the derivative with respect to variable
        @rtype: L{TensorField}
        """
        period = self.period[variable]
        if period is None:
            ui = variable*index_expression[::] + \
                 index_expression[2::] + index_expression[...]
            li = variable*index_expression[::] + \
                 index_expression[:-2:] + index_expression[...]
            d_values = 0.5*(self.values[ui]-self.values[li]) \
                       /self.spacing[variable]
            diffaxis = self.axes[variable][1:-1]
            d_axes = self.axes[:variable]+[diffaxis]+self.axes[variable+1:]
        else:
            u = N.take(self.values, range(2, len(self.axes[variable]))+[0, 1],
                       axis=variable)
            l = self.values
            d_values = (u-l)/self.spacing[variable]
            d_axes = self.axes
        d_default = None
        if self.default is not None:
            d_default = Numeric.zeros(self.rank*(3,), Numeric.Float)
        return self._constructor(d_axes, d_values, d_default,
                                 self.period, False)

    def allDerivatives(self):
        """
        @returns: all three derivatives (x, y, z) on equal-sized grids
        @rtype: (L{TensorField}, L{TensorField}, L{TensorField})
        """
        x = self.derivative(0)
        y = self.derivative(1)
        z = self.derivative(2)
        if self.period[0] is None:
            y._reduceAxis(0)
            z._reduceAxis(0)
        if self.period[1] is None:
            x._reduceAxis(1)
            z._reduceAxis(1)
        if self.period[2] is None:
            x._reduceAxis(2)
            y._reduceAxis(2)
        return x, y, z

    def _reduceAxis(self, variable):
        self.axes[variable] = self.axes[variable][1:-1]
        i = variable*index_expression[::] + \
            index_expression[1:-1:] + index_expression[...]
        self.values = self.values[i]

    def _checkCompatibility(self, other):
        if self.period != other.period:
            raise ValueError("Periods don't match")

    def __add__(self, other):
        self._checkCompatibility(other)
        if self.default is None or other.default is None:
            default = None
        else:
            default = self.default + other.default
        return self._constructor(self.axes, self.values+other.values,
                                 default, self.period, False)

    def __sub__(self, other):
        self._checkCompatibility(other)
        if self.default is None or other.default is None:
            default = None
        else:
            default = self.default - other.default
        return self._constructor(self.axes, self.values-other.values,
                                 default, self.period, False)

#
# Scalar field class definition
#
class ScalarField(TensorField):

    """Scalar field (tensor field of rank 0)

    A subclass of TensorField.
    """

    def __init__(self, axes, values, default = None,
                 period = (None, None, None), check = True):
        """
        @param axes: three arrays specifying the axis ticks for the three
                     axes
        @type axes: sequence of C{Numeric.array} of rank 1

        @param values: an array containing the field values. The
                       three dimensions correspond to the x, y, z
                       directions and must have lengths compatible with
                       the axis arrays.
        @type values: C{Numeric.array} of 3 dimensions

        @param default: the value of the field for points outside the grid.
                        A value of 'None' means that an exception will be
                        raised for an attempt to evaluate the field outside
                        the grid. Any other value must a tensor of the
                        correct rank.
        @type default: number or C{NoneType}

        @param period: the period for each of the variables, or C{None} for
            variables in which the function is not periodic.
        @type period: sequence of length three

        @raise ValueError: if the arguments are not consistent
        """
        TensorField.__init__(self, 0, axes, values, default, period, check)

    def gradient(self):
        """
        @returns: the gradient
        @rtype: L{VectorField}
        """
        x, y, z = self.allDerivatives()
        grad = Numeric.transpose(Numeric.array([x.values, y.values, z.values]),
                                 [1,2,3,0])
        if self.default is None:
            default = None
        else:
            default = Numeric.zeros((3,), Numeric.Float)
        return VectorField(x.axes, grad, default, self.period, False)

    def laplacian(self):
        """
        @returns: the laplacian (gradient of divergence)
        @rtype L{ScalarField}
        """
        return self.gradient().divergence()

ScalarField._constructor = ScalarField

#
# Vector field class definition
#
class VectorField(TensorField):

    """Vector field (tensor field of rank 1)

    A subclass of TensorField.
    """

    def __init__(self, axes, values, default = None,
                 period = (None, None, None), check = True):
        """
        @param axes: three arrays specifying the axis ticks for the three
                     axes
        @type axes: sequence of C{Numeric.array} of rank 1

        @param values: an array containing the field values. Its first
                       three dimensions correspond to the x, y, z
                       directions and must have lengths compatible with
                       the axis arrays. The fourth dimension must
                       have length 3.
        @type values: C{Numeric.array} of four dimensions

        @param default: the value of the field for points outside the grid.
                        A value of 'None' means that an exception will be
                        raised for an attempt to evaluate the field outside
                        the grid. Any other value must a vector
        @type default: L{Scientific.Geometry.Vector} or C{NoneType}

        @param period: the period for each of the variables, or C{None} for
            variables in which the function is not periodic.
        @type period: sequence of length three

        @raise ValueError: if the arguments are not consistent
        """
        TensorField.__init__(self, 1, axes, values, default, period, check)

    def zero(self):
        return Vector(0., 0., 0.)

    def _divergence(self, x, y, z):
        return x[0] + y[1] + z[2]

    def _curl(self, x, y, z):
        curl_x = y.values[..., 2] - z.values[..., 1]
        curl_y = z.values[..., 0] - x.values[..., 2]
        curl_z = x.values[..., 1] - y.values[..., 0]
        curl = Numeric.transpose(Numeric.array([curl_x, curl_y, curl_z]),
                                 [1,2,3,0])
        if self.default is None:
            default = None
        else:
            default = Numeric.zeros((3,), Numeric.Float)
        return VectorField(x.axes, curl, default, self.period, False)

    def _strain(self, x, y, z):
        strain = Numeric.transpose(Numeric.array([x.values, y.values,
                                                  z.values]), [1,2,3,0,4])
        strain = 0.5*(strain+Numeric.transpose(strain, [0,1,2,4,3]))
        trace = (strain[..., 0,0] + strain[..., 1,1] + strain[..., 2,2])/3.
        strain = strain - trace[..., Numeric.NewAxis, Numeric.NewAxis] * \
                 Numeric.identity(3)[Numeric.NewAxis, Numeric.NewAxis,
                                     Numeric.NewAxis, :, :]
        if self.default is None:
            default = None
        else:
            default = Numeric.zeros((3, 3), Numeric.Float)
        return TensorField(2, x.axes, strain, default, self.period, False)
        
    def divergence(self):
        """
        @returns: the divergence
        @rtype L{ScalarField}
        """
        x, y, z = self.allDerivatives()
        return self._divergence(x, y, z)

    def curl(self):
        """
        @returns: the curl
        @rtype L{VectorField}
        """
        x, y, z = self.allDerivatives()
        return self._curl(x, y, z)

    def strain(self): 
        """
        @returns: the strain
        @rtype L{TensorField} of rank 2
        """
        x, y, z = self.allDerivatives()
        return self._strain(x, y, z)

    def divergenceCurlAndStrain(self):
        """
        @returns: all derivative fields: divergence, curl, and strain
        @rtype (L{ScalarField}, L{VectorField}, L{TensorField})
        """
        x, y, z = self.allDerivatives()
        return self._divergence(x, y, z), self._curl(x, y, z), \
               self._strain(x, y, z)

    def laplacian(self):
        """
        @returns: the laplacian
        @rtype L{VectorField}
        """
        x, y, z = self.allDerivatives()
        return self._divergence(x, y, z).gradient()-self._curl(x, y, z).curl()

    def length(self):
        """
        @returns: a scalar field corresponding to the length (norm) of
        the vector field.
        @rtype: L{ScalarField}
        """
        l = Numeric.sqrt(Numeric.add.reduce(self.values**2, -1))
        if self.default is None:
            default = None
        else:
            try:
                default = Numeric.sqrt(Numeric.add.reduce(self.default))
            except ValueError:
                default = None
        return ScalarField(self.axes, l, default, self.period, False)

VectorField._constructor = VectorField

#
# Test code
#
if __name__ == '__main__':

    from Numeric import *
    axis = arange(0., 1., 0.1)
    values = zeros((10,10,10,3), Float)

    zero = VectorField(3*(axis,), values)
    div = zero.divergence()
    curl = zero.curl()
    strain = zero.strain()

    zero = VectorField(3*(axis,), values, period=(1.1, 1.1, 1.1))
    div = zero.divergence()
    curl = zero.curl()
    strain = zero.strain()
