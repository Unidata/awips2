# Copyright (C) 2003-2005 Peter J. Verveer
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
#
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
#
# 2. Redistributions in binary form must reproduce the above
#    copyright notice, this list of conditions and the following
#    disclaimer in the documentation and/or other materials provided
#    with the distribution.
#
# 3. The name of the author may not be used to endorse or promote
#    products derived from this software without specific prior
#    written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
# OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
# DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
# GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
# WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

<<<<<<< HEAD
import math
import numpy
import _ni_support
import _nd_image
from scipy.misc import doccer

_input_doc = \
"""input : array-like
    input array to filter"""
_axis_doc = \
"""axis : integer, optional
    axis of ``input`` along which to calculate. Default is -1"""
_output_doc = \
"""output : array, optional
    The ``output`` parameter passes an array in which to store the
=======
from __future__ import division, print_function, absolute_import

import math
import numpy
from . import _ni_support
from . import _nd_image
from scipy.misc import doccer
from scipy._lib._version import NumpyVersion

__all__ = ['correlate1d', 'convolve1d', 'gaussian_filter1d', 'gaussian_filter',
           'prewitt', 'sobel', 'generic_laplace', 'laplace',
           'gaussian_laplace', 'generic_gradient_magnitude',
           'gaussian_gradient_magnitude', 'correlate', 'convolve',
           'uniform_filter1d', 'uniform_filter', 'minimum_filter1d',
           'maximum_filter1d', 'minimum_filter', 'maximum_filter',
           'rank_filter', 'median_filter', 'percentile_filter',
           'generic_filter1d', 'generic_filter']


_input_doc = \
"""input : array_like
    Input array to filter."""
_axis_doc = \
"""axis : int, optional
    The axis of `input` along which to calculate. Default is -1."""
_output_doc = \
"""output : array, optional
    The `output` parameter passes an array in which to store the
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    filter output."""
_size_foot_doc = \
"""size : scalar or tuple, optional
    See footprint, below
footprint : array, optional
<<<<<<< HEAD
    Either ``size`` or ``footprint`` must be defined.  ``size`` gives
    the shape that is taken from the input array, at every element
    position, to define the input to the filter function.
    ``footprint`` is a boolean array that specifies (implicitly) a
    shape, but also which of the elements within this shape will get
    passed to the filter function.  Thus ``size=(n,m)`` is equivalent
    to ``footprint=np.ones((n,m))``.  We adjust ``size`` to the number
    of dimensions of the input array, so that, if the input array is
    shape (10,10,10), and ``size`` is 2, then the actual size used is
    (2,2,2).
"""
_mode_doc = \
"""mode : {'reflect','constant','nearest','mirror', 'wrap'}, optional
    The ``mode`` parameter determines how the array borders are
    handled, where ``cval`` is the value when mode is equal to
    'constant'. Default is 'reflect'"""
_cval_doc = \
"""cval : scalar, optional
    Value to fill past edges of input if ``mode`` is 'constant'. Default
    is 0.0"""
_origin_doc = \
"""origin : scalar, optional
The ``origin`` parameter controls the placement of the filter. Default 0"""
=======
    Either `size` or `footprint` must be defined.  `size` gives
    the shape that is taken from the input array, at every element
    position, to define the input to the filter function.
    `footprint` is a boolean array that specifies (implicitly) a
    shape, but also which of the elements within this shape will get
    passed to the filter function.  Thus ``size=(n,m)`` is equivalent
    to ``footprint=np.ones((n,m))``.  We adjust `size` to the number
    of dimensions of the input array, so that, if the input array is
    shape (10,10,10), and `size` is 2, then the actual size used is
    (2,2,2).
"""
_mode_doc = \
"""mode : {'reflect', 'constant', 'nearest', 'mirror', 'wrap'}, optional
    The `mode` parameter determines how the array borders are
    handled, where `cval` is the value when mode is equal to
    'constant'. Default is 'reflect'"""
_cval_doc = \
"""cval : scalar, optional
    Value to fill past edges of input if `mode` is 'constant'. Default
    is 0.0"""
_origin_doc = \
"""origin : scalar, optional
    The `origin` parameter controls the placement of the filter.
    Default 0.0."""
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
_extra_arguments_doc = \
"""extra_arguments : sequence, optional
    Sequence of extra positional arguments to pass to passed function"""
_extra_keywords_doc = \
"""extra_keywords : dict, optional
    dict of extra keyword arguments to pass to passed function"""

docdict = {
<<<<<<< HEAD
    'input':_input_doc,
    'axis':_axis_doc,
    'output':_output_doc,
    'size_foot':_size_foot_doc,
    'mode':_mode_doc,
    'cval':_cval_doc,
    'origin':_origin_doc,
    'extra_arguments':_extra_arguments_doc,
    'extra_keywords':_extra_keywords_doc,
=======
    'input': _input_doc,
    'axis': _axis_doc,
    'output': _output_doc,
    'size_foot': _size_foot_doc,
    'mode': _mode_doc,
    'cval': _cval_doc,
    'origin': _origin_doc,
    'extra_arguments': _extra_arguments_doc,
    'extra_keywords': _extra_keywords_doc,
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    }

docfiller = doccer.filldoc(docdict)

<<<<<<< HEAD
@docfiller
def correlate1d(input, weights, axis = -1, output = None, mode = "reflect",
                cval = 0.0, origin = 0):
=======

@docfiller
def correlate1d(input, weights, axis=-1, output=None, mode="reflect",
                cval=0.0, origin=0):
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    """Calculate a one-dimensional correlation along the given axis.

    The lines of the array along the given axis are correlated with the
    given weights.

    Parameters
    ----------
    %(input)s
    weights : array
<<<<<<< HEAD
        one-dimensional sequence of numbers
=======
        One-dimensional sequence of numbers.
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    %(axis)s
    %(output)s
    %(mode)s
    %(cval)s
    %(origin)s
    """
    input = numpy.asarray(input)
    if numpy.iscomplexobj(input):
<<<<<<< HEAD
        raise TypeError, 'Complex type not supported'
    output, return_value = _ni_support._get_output(output, input)
    weights = numpy.asarray(weights, dtype=numpy.float64)
    if weights.ndim != 1 or weights.shape[0] < 1:
        raise RuntimeError, 'no filter weights given'
    if not weights.flags.contiguous:
        weights = weights.copy()
    axis = _ni_support._check_axis(axis, input.ndim)
    if ((len(weights) // 2 + origin < 0) or
        (len(weights) // 2 + origin > len(weights))):
        raise ValueError, 'invalid origin'
=======
        raise TypeError('Complex type not supported')
    output, return_value = _ni_support._get_output(output, input)
    weights = numpy.asarray(weights, dtype=numpy.float64)
    if weights.ndim != 1 or weights.shape[0] < 1:
        raise RuntimeError('no filter weights given')
    if not weights.flags.contiguous:
        weights = weights.copy()
    axis = _ni_support._check_axis(axis, input.ndim)
    if (len(weights) // 2 + origin < 0) or (len(weights) // 2 +
                                            origin > len(weights)):
        raise ValueError('invalid origin')
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    mode = _ni_support._extend_mode_to_code(mode)
    _nd_image.correlate1d(input, weights, axis, output, mode, cval,
                          origin)
    return return_value


@docfiller
<<<<<<< HEAD
def convolve1d(input, weights, axis = -1, output = None, mode = "reflect",
               cval = 0.0, origin = 0):
=======
def convolve1d(input, weights, axis=-1, output=None, mode="reflect",
               cval=0.0, origin=0):
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    """Calculate a one-dimensional convolution along the given axis.

    The lines of the array along the given axis are convolved with the
    given weights.

    Parameters
    ----------
    %(input)s
    weights : ndarray
<<<<<<< HEAD
        one-dimensional sequence of numbers
=======
        One-dimensional sequence of numbers.
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    %(axis)s
    %(output)s
    %(mode)s
    %(cval)s
    %(origin)s
<<<<<<< HEAD
=======

    Returns
    -------
    convolve1d : ndarray
        Convolved array with same shape as input

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    """
    weights = weights[::-1]
    origin = -origin
    if not len(weights) & 1:
        origin -= 1
    return correlate1d(input, weights, axis, output, mode, cval, origin)


@docfiller
<<<<<<< HEAD
def gaussian_filter1d(input, sigma, axis = -1, order = 0, output = None,
                      mode = "reflect", cval = 0.0):
=======
def gaussian_filter1d(input, sigma, axis=-1, order=0, output=None,
                      mode="reflect", cval=0.0, truncate=4.0):
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    """One-dimensional Gaussian filter.

    Parameters
    ----------
    %(input)s
    sigma : scalar
        standard deviation for Gaussian kernel
    %(axis)s
    order : {0, 1, 2, 3}, optional
        An order of 0 corresponds to convolution with a Gaussian
        kernel. An order of 1, 2, or 3 corresponds to convolution with
        the first, second or third derivatives of a Gaussian. Higher
        order derivatives are not implemented
    %(output)s
    %(mode)s
    %(cval)s
<<<<<<< HEAD
=======
    truncate : float, optional
        Truncate the filter at this many standard deviations.
        Default is 4.0.

    Returns
    -------
    gaussian_filter1d : ndarray

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    """
    if order not in range(4):
        raise ValueError('Order outside 0..3 not implemented')
    sd = float(sigma)
<<<<<<< HEAD
    # make the length of the filter equal to 4 times the standard
    # deviations:
    lw = int(4.0 * sd + 0.5)
=======
    # make the radius of the filter equal to truncate standard deviations
    lw = int(truncate * sd + 0.5)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    weights = [0.0] * (2 * lw + 1)
    weights[lw] = 1.0
    sum = 1.0
    sd = sd * sd
    # calculate the kernel:
    for ii in range(1, lw + 1):
        tmp = math.exp(-0.5 * float(ii * ii) / sd)
        weights[lw + ii] = tmp
        weights[lw - ii] = tmp
        sum += 2.0 * tmp
    for ii in range(2 * lw + 1):
        weights[ii] /= sum
    # implement first, second and third order derivatives:
<<<<<<< HEAD
    if order == 1 : # first derivative
=======
    if order == 1:  # first derivative
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        weights[lw] = 0.0
        for ii in range(1, lw + 1):
            x = float(ii)
            tmp = -x / sd * weights[lw + ii]
            weights[lw + ii] = -tmp
            weights[lw - ii] = tmp
<<<<<<< HEAD
    elif order == 2: # second derivative
=======
    elif order == 2:  # second derivative
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        weights[lw] *= -1.0 / sd
        for ii in range(1, lw + 1):
            x = float(ii)
            tmp = (x * x / sd - 1.0) * weights[lw + ii] / sd
            weights[lw + ii] = tmp
            weights[lw - ii] = tmp
<<<<<<< HEAD
    elif order == 3: # third derivative
=======
    elif order == 3:  # third derivative
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        weights[lw] = 0.0
        sd2 = sd * sd
        for ii in range(1, lw + 1):
            x = float(ii)
<<<<<<< HEAD
            tmp = (3.0 - x * x / sd) * x * weights[lw + ii] / sd / sd
=======
            tmp = (3.0 - x * x / sd) * x * weights[lw + ii] / sd2
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
            weights[lw + ii] = -tmp
            weights[lw - ii] = tmp
    return correlate1d(input, weights, axis, output, mode, cval, 0)


@docfiller
<<<<<<< HEAD
def gaussian_filter(input, sigma, order = 0, output = None,
                  mode = "reflect", cval = 0.0):
    """Multi-dimensional Gaussian filter.
=======
def gaussian_filter(input, sigma, order=0, output=None,
                  mode="reflect", cval=0.0, truncate=4.0):
    """Multidimensional Gaussian filter.
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    Parameters
    ----------
    %(input)s
    sigma : scalar or sequence of scalars
<<<<<<< HEAD
        standard deviation for Gaussian kernel. The standard
=======
        Standard deviation for Gaussian kernel. The standard
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        deviations of the Gaussian filter are given for each axis as a
        sequence, or as a single number, in which case it is equal for
        all axes.
    order : {0, 1, 2, 3} or sequence from same set, optional
        The order of the filter along each axis is given as a sequence
        of integers, or as a single number.  An order of 0 corresponds
        to convolution with a Gaussian kernel. An order of 1, 2, or 3
        corresponds to convolution with the first, second or third
        derivatives of a Gaussian. Higher order derivatives are not
        implemented
    %(output)s
    %(mode)s
    %(cval)s
<<<<<<< HEAD

    Notes
    -----
    The multi-dimensional filter is implemented as a sequence of
=======
    truncate : float
        Truncate the filter at this many standard deviations.
        Default is 4.0.

    Returns
    -------
    gaussian_filter : ndarray
        Returned array of same shape as `input`.

    Notes
    -----
    The multidimensional filter is implemented as a sequence of
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    one-dimensional convolution filters. The intermediate arrays are
    stored in the same data type as the output. Therefore, for output
    types with a limited precision, the results may be imprecise
    because intermediate results may be stored with insufficient
    precision.
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    """
    input = numpy.asarray(input)
    output, return_value = _ni_support._get_output(output, input)
    orders = _ni_support._normalize_sequence(order, input.ndim)
    if not set(orders).issubset(set(range(4))):
        raise ValueError('Order outside 0..4 not implemented')
    sigmas = _ni_support._normalize_sequence(sigma, input.ndim)
<<<<<<< HEAD
    axes = range(input.ndim)
=======
    axes = list(range(input.ndim))
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    axes = [(axes[ii], sigmas[ii], orders[ii])
                        for ii in range(len(axes)) if sigmas[ii] > 1e-15]
    if len(axes) > 0:
        for axis, sigma, order in axes:
            gaussian_filter1d(input, sigma, axis, order, output,
<<<<<<< HEAD
                              mode, cval)
=======
                              mode, cval, truncate)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
            input = output
    else:
        output[...] = input[...]
    return return_value


@docfiller
<<<<<<< HEAD
def prewitt(input, axis = -1, output = None, mode = "reflect", cval = 0.0):
=======
def prewitt(input, axis=-1, output=None, mode="reflect", cval=0.0):
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    """Calculate a Prewitt filter.

    Parameters
    ----------
    %(input)s
    %(axis)s
    %(output)s
    %(mode)s
    %(cval)s
    """
    input = numpy.asarray(input)
    axis = _ni_support._check_axis(axis, input.ndim)
    output, return_value = _ni_support._get_output(output, input)
    correlate1d(input, [-1, 0, 1], axis, output, mode, cval, 0)
    axes = [ii for ii in range(input.ndim) if ii != axis]
    for ii in axes:
        correlate1d(output, [1, 1, 1], ii, output, mode, cval, 0,)
    return return_value


@docfiller
<<<<<<< HEAD
def sobel(input, axis = -1, output = None, mode = "reflect", cval = 0.0):
=======
def sobel(input, axis=-1, output=None, mode="reflect", cval=0.0):
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    """Calculate a Sobel filter.

    Parameters
    ----------
    %(input)s
    %(axis)s
    %(output)s
    %(mode)s
    %(cval)s
    """
    input = numpy.asarray(input)
    axis = _ni_support._check_axis(axis, input.ndim)
    output, return_value = _ni_support._get_output(output, input)
    correlate1d(input, [-1, 0, 1], axis, output, mode, cval, 0)
    axes = [ii for ii in range(input.ndim) if ii != axis]
    for ii in axes:
        correlate1d(output, [1, 2, 1], ii, output, mode, cval, 0)
    return return_value


@docfiller
<<<<<<< HEAD
def generic_laplace(input, derivative2, output = None, mode = "reflect",
                    cval = 0.0,
                    extra_arguments = (),
                    extra_keywords = None):
    """Calculate a multidimensional laplace filter using the provided
    second derivative function.
=======
def generic_laplace(input, derivative2, output=None, mode="reflect",
                    cval=0.0,
                    extra_arguments=(),
                    extra_keywords = None):
    """N-dimensional Laplace filter using a provided second derivative function
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    Parameters
    ----------
    %(input)s
    derivative2 : callable
        Callable with the following signature::
<<<<<<< HEAD
            derivative2(input, axis, output, mode, cval,
                        *extra_arguments, **extra_keywords)
        See ``extra_arguments``, ``extra_keywords`` below
=======

            derivative2(input, axis, output, mode, cval,
                        *extra_arguments, **extra_keywords)

        See `extra_arguments`, `extra_keywords` below.
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    %(output)s
    %(mode)s
    %(cval)s
    %(extra_keywords)s
    %(extra_arguments)s
    """
    if extra_keywords is None:
        extra_keywords = {}
    input = numpy.asarray(input)
    output, return_value = _ni_support._get_output(output, input)
<<<<<<< HEAD
    axes = range(input.ndim)
=======
    axes = list(range(input.ndim))
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    if len(axes) > 0:
        derivative2(input, axes[0], output, mode, cval,
                    *extra_arguments, **extra_keywords)
        for ii in range(1, len(axes)):
            tmp = derivative2(input, axes[ii], output.dtype, mode, cval,
                              *extra_arguments, **extra_keywords)
            output += tmp
    else:
        output[...] = input[...]
    return return_value


@docfiller
<<<<<<< HEAD
def laplace(input, output = None, mode = "reflect", cval = 0.0):
    """Calculate a multidimensional laplace filter using an estimation
    for the second derivative based on differences.
=======
def laplace(input, output=None, mode="reflect", cval=0.0):
    """N-dimensional Laplace filter based on approximate second derivatives.
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    Parameters
    ----------
    %(input)s
    %(output)s
    %(mode)s
    %(cval)s
    """
    def derivative2(input, axis, output, mode, cval):
        return correlate1d(input, [1, -2, 1], axis, output, mode, cval, 0)
    return generic_laplace(input, derivative2, output, mode, cval)


@docfiller
<<<<<<< HEAD
def gaussian_laplace(input, sigma, output = None, mode = "reflect",
                     cval = 0.0):
    """Calculate a multidimensional laplace filter using gaussian
    second derivatives.
=======
def gaussian_laplace(input, sigma, output=None, mode="reflect",
                     cval=0.0, **kwargs):
    """Multidimensional Laplace filter using gaussian second derivatives.
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    Parameters
    ----------
    %(input)s
    sigma : scalar or sequence of scalars
        The standard deviations of the Gaussian filter are given for
        each axis as a sequence, or as a single number, in which case
<<<<<<< HEAD
        it is equal for all axes..
    %(output)s
    %(mode)s
    %(cval)s
    """
    input = numpy.asarray(input)
    def derivative2(input, axis, output, mode, cval, sigma):
        order = [0] * input.ndim
        order[axis] = 2
        return gaussian_filter(input, sigma, order, output, mode, cval)
    return generic_laplace(input, derivative2, output, mode, cval,
                           extra_arguments = (sigma,))


@docfiller
def generic_gradient_magnitude(input, derivative, output = None,
                mode = "reflect", cval = 0.0,
                extra_arguments = (), extra_keywords = None):
    """Calculate a gradient magnitude using the provided function for
    the gradient.
=======
        it is equal for all axes.
    %(output)s
    %(mode)s
    %(cval)s
    Extra keyword arguments will be passed to gaussian_filter().
    """
    input = numpy.asarray(input)

    def derivative2(input, axis, output, mode, cval, sigma, **kwargs):
        order = [0] * input.ndim
        order[axis] = 2
        return gaussian_filter(input, sigma, order, output, mode, cval,
                               **kwargs)

    return generic_laplace(input, derivative2, output, mode, cval,
                           extra_arguments=(sigma,),
                           extra_keywords=kwargs)


@docfiller
def generic_gradient_magnitude(input, derivative, output=None,
                mode="reflect", cval=0.0,
                extra_arguments=(), extra_keywords = None):
    """Gradient magnitude using a provided gradient function.
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    Parameters
    ----------
    %(input)s
    derivative : callable
        Callable with the following signature::
<<<<<<< HEAD
            derivative(input, axis, output, mode, cval,
                        *extra_arguments, **extra_keywords)
        See ``extra_arguments``, ``extra_keywords`` below
        ``derivative`` can assume that ``input`` and ``output`` are
        ndarrays.
        Note that the output from ``derivative`` is modified inplace;
=======

            derivative(input, axis, output, mode, cval,
                       *extra_arguments, **extra_keywords)

        See `extra_arguments`, `extra_keywords` below.
        `derivative` can assume that `input` and `output` are ndarrays.
        Note that the output from `derivative` is modified inplace;
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        be careful to copy important inputs before returning them.
    %(output)s
    %(mode)s
    %(cval)s
    %(extra_keywords)s
    %(extra_arguments)s
    """
    if extra_keywords is None:
        extra_keywords = {}
    input = numpy.asarray(input)
    output, return_value = _ni_support._get_output(output, input)
<<<<<<< HEAD
    axes = range(input.ndim)
=======
    axes = list(range(input.ndim))
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    if len(axes) > 0:
        derivative(input, axes[0], output, mode, cval,
                   *extra_arguments, **extra_keywords)
        numpy.multiply(output, output, output)
        for ii in range(1, len(axes)):
            tmp = derivative(input, axes[ii], output.dtype, mode, cval,
                             *extra_arguments, **extra_keywords)
            numpy.multiply(tmp, tmp, tmp)
            output += tmp
<<<<<<< HEAD
        numpy.sqrt(output, output)
=======
        # This allows the sqrt to work with a different default casting
        if NumpyVersion(numpy.__version__) > '1.6.1':
            numpy.sqrt(output, output, casting='unsafe')
        else:
            numpy.sqrt(output, output)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    else:
        output[...] = input[...]
    return return_value


@docfiller
<<<<<<< HEAD
def gaussian_gradient_magnitude(input, sigma, output = None,
                mode = "reflect", cval = 0.0):
    """Calculate a multidimensional gradient magnitude using gaussian
    derivatives.
=======
def gaussian_gradient_magnitude(input, sigma, output=None,
                mode="reflect", cval=0.0, **kwargs):
    """Multidimensional gradient magnitude using Gaussian derivatives.
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    Parameters
    ----------
    %(input)s
    sigma : scalar or sequence of scalars
        The standard deviations of the Gaussian filter are given for
        each axis as a sequence, or as a single number, in which case
        it is equal for all axes..
    %(output)s
    %(mode)s
    %(cval)s
<<<<<<< HEAD
    """
    input = numpy.asarray(input)
    def derivative(input, axis, output, mode, cval, sigma):
        order = [0] * input.ndim
        order[axis] = 1
        return gaussian_filter(input, sigma, order, output, mode, cval)
    return generic_gradient_magnitude(input, derivative, output, mode,
                            cval, extra_arguments = (sigma,))
=======
    Extra keyword arguments will be passed to gaussian_filter().
    """
    input = numpy.asarray(input)

    def derivative(input, axis, output, mode, cval, sigma, **kwargs):
        order = [0] * input.ndim
        order[axis] = 1
        return gaussian_filter(input, sigma, order, output, mode,
                               cval, **kwargs)

    return generic_gradient_magnitude(input, derivative, output, mode,
                                      cval, extra_arguments=(sigma,),
                                      extra_keywords=kwargs)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b


def _correlate_or_convolve(input, weights, output, mode, cval, origin,
                           convolution):
    input = numpy.asarray(input)
<<<<<<< HEAD
    if numpy.iscomplexobj(int):
        raise TypeError, 'Complex type not supported'
=======
    if numpy.iscomplexobj(input):
        raise TypeError('Complex type not supported')
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    origins = _ni_support._normalize_sequence(origin, input.ndim)
    weights = numpy.asarray(weights, dtype=numpy.float64)
    wshape = [ii for ii in weights.shape if ii > 0]
    if len(wshape) != input.ndim:
<<<<<<< HEAD
        raise RuntimeError, 'filter weights array has incorrect shape.'
=======
        raise RuntimeError('filter weights array has incorrect shape.')
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    if convolution:
        weights = weights[tuple([slice(None, None, -1)] * weights.ndim)]
        for ii in range(len(origins)):
            origins[ii] = -origins[ii]
            if not weights.shape[ii] & 1:
                origins[ii] -= 1
    for origin, lenw in zip(origins, wshape):
        if (lenw // 2 + origin < 0) or (lenw // 2 + origin > lenw):
<<<<<<< HEAD
            raise ValueError, 'invalid origin'
=======
            raise ValueError('invalid origin')
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    if not weights.flags.contiguous:
        weights = weights.copy()
    output, return_value = _ni_support._get_output(output, input)
    mode = _ni_support._extend_mode_to_code(mode)
    _nd_image.correlate(input, weights, output, mode, cval, origins)
    return return_value


@docfiller
<<<<<<< HEAD
def correlate(input, weights, output = None, mode = 'reflect', cval = 0.0,
              origin = 0):
=======
def correlate(input, weights, output=None, mode='reflect', cval=0.0,
              origin=0):
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    """
    Multi-dimensional correlation.

    The array is correlated with the given kernel.

    Parameters
    ----------
    input : array-like
        input array to filter
    weights : ndarray
        array of weights, same number of dimensions as input
    output : array, optional
        The ``output`` parameter passes an array in which to store the
        filter output.
    mode : {'reflect','constant','nearest','mirror', 'wrap'}, optional
        The ``mode`` parameter determines how the array borders are
        handled, where ``cval`` is the value when mode is equal to
        'constant'. Default is 'reflect'
    cval : scalar, optional
        Value to fill past edges of input if ``mode`` is 'constant'. Default
        is 0.0
    origin : scalar, optional
        The ``origin`` parameter controls the placement of the filter.
        Default 0

    See Also
    --------
    convolve : Convolve an image with a kernel.

    """
    return _correlate_or_convolve(input, weights, output, mode, cval,
                                  origin, False)


@docfiller
<<<<<<< HEAD
def convolve(input, weights, output = None, mode = 'reflect', cval = 0.0,
             origin = 0):
    """
    Multi-dimensional convolution.
=======
def convolve(input, weights, output=None, mode='reflect', cval=0.0,
             origin=0):
    """
    Multidimensional convolution.
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    The array is convolved with the given kernel.

    Parameters
    ----------
<<<<<<< HEAD
    input : array-like
        input array to filter
    weights : ndarray
        array of weights, same number of dimensions as input
    output : array, optional
        The ``output`` parameter passes an array in which to store the
        filter output.
    mode : {'reflect','constant','nearest','mirror', 'wrap'}, optional
        The ``mode`` parameter determines how the array borders are
        handled, where ``cval`` is the value when mode is equal to
        'constant'. Default is 'reflect'
    cval : scalar, optional
        Value to fill past edges of input if ``mode`` is 'constant'. Default
        is 0.0
    origin : scalar, optional
        The ``origin`` parameter controls the placement of the filter.
        Default 0

    See Also
    --------

    correlate : Correlate an image with a kernel.

=======
    input : array_like
        Input array to filter.
    weights : array_like
        Array of weights, same number of dimensions as input
    output : ndarray, optional
        The `output` parameter passes an array in which to store the
        filter output.
    mode : {'reflect','constant','nearest','mirror', 'wrap'}, optional
        the `mode` parameter determines how the array borders are
        handled. For 'constant' mode, values beyond borders are set to be
        `cval`. Default is 'reflect'.
    cval : scalar, optional
        Value to fill past edges of input if `mode` is 'constant'. Default
        is 0.0
    origin : array_like, optional
        The `origin` parameter controls the placement of the filter.
        Default is 0.

    Returns
    -------
    result : ndarray
        The result of convolution of `input` with `weights`.

    See Also
    --------
    correlate : Correlate an image with a kernel.

    Notes
    -----
    Each value in result is :math:`C_i = \\sum_j{I_{i+j-k} W_j}`, where
    W is the `weights` kernel,
    j is the n-D spatial index over :math:`W`,
    I is the `input` and k is the coordinate of the center of
    W, specified by `origin` in the input parameters.

    Examples
    --------
    Perhaps the simplest case to understand is ``mode='constant', cval=0.0``,
    because in this case borders (i.e. where the `weights` kernel, centered
    on any one value, extends beyond an edge of `input`.

    >>> a = np.array([[1, 2, 0, 0],
    ...               [5, 3, 0, 4],
    ...               [0, 0, 0, 7],
    ...               [9, 3, 0, 0]])
    >>> k = np.array([[1,1,1],[1,1,0],[1,0,0]])
    >>> from scipy import ndimage
    >>> ndimage.convolve(a, k, mode='constant', cval=0.0)
    array([[11, 10,  7,  4],
           [10,  3, 11, 11],
           [15, 12, 14,  7],
           [12,  3,  7,  0]])

    Setting ``cval=1.0`` is equivalent to padding the outer edge of `input`
    with 1.0's (and then extracting only the original region of the result).

    >>> ndimage.convolve(a, k, mode='constant', cval=1.0)
    array([[13, 11,  8,  7],
           [11,  3, 11, 14],
           [16, 12, 14, 10],
           [15,  6, 10,  5]])

    With ``mode='reflect'`` (the default), outer values are reflected at the
    edge of `input` to fill in missing values.

    >>> b = np.array([[2, 0, 0],
    ...               [1, 0, 0],
    ...               [0, 0, 0]])
    >>> k = np.array([[0,1,0], [0,1,0], [0,1,0]])
    >>> ndimage.convolve(b, k, mode='reflect')
    array([[5, 0, 0],
           [3, 0, 0],
           [1, 0, 0]])

    This includes diagonally at the corners.

    >>> k = np.array([[1,0,0],[0,1,0],[0,0,1]])
    >>> ndimage.convolve(b, k)
    array([[4, 2, 0],
           [3, 2, 0],
           [1, 1, 0]])

    With ``mode='nearest'``, the single nearest value in to an edge in
    `input` is repeated as many times as needed to match the overlapping
    `weights`.

    >>> c = np.array([[2, 0, 1],
    ...               [1, 0, 0],
    ...               [0, 0, 0]])
    >>> k = np.array([[0, 1, 0],
    ...               [0, 1, 0],
    ...               [0, 1, 0],
    ...               [0, 1, 0],
    ...               [0, 1, 0]])
    >>> ndimage.convolve(c, k, mode='nearest')
    array([[7, 0, 3],
           [5, 0, 2],
           [3, 0, 1]])

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    """
    return _correlate_or_convolve(input, weights, output, mode, cval,
                                  origin, True)


@docfiller
<<<<<<< HEAD
def uniform_filter1d(input, size, axis = -1, output = None,
                     mode = "reflect", cval = 0.0, origin = 0):
=======
def uniform_filter1d(input, size, axis=-1, output=None,
                     mode="reflect", cval=0.0, origin=0):
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    """Calculate a one-dimensional uniform filter along the given axis.

    The lines of the array along the given axis are filtered with a
    uniform filter of given size.

    Parameters
    ----------
    %(input)s
<<<<<<< HEAD
    size : integer
=======
    size : int
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        length of uniform filter
    %(axis)s
    %(output)s
    %(mode)s
    %(cval)s
    %(origin)s
    """
    input = numpy.asarray(input)
    if numpy.iscomplexobj(input):
<<<<<<< HEAD
        raise TypeError, 'Complex type not supported'
    axis = _ni_support._check_axis(axis, input.ndim)
    if size < 1:
        raise RuntimeError, 'incorrect filter size'
    output, return_value = _ni_support._get_output(output, input)
    if (size // 2 + origin < 0) or (size // 2 + origin > size):
        raise ValueError, 'invalid origin'
=======
        raise TypeError('Complex type not supported')
    axis = _ni_support._check_axis(axis, input.ndim)
    if size < 1:
        raise RuntimeError('incorrect filter size')
    output, return_value = _ni_support._get_output(output, input)
    if (size // 2 + origin < 0) or (size // 2 + origin >= size):
        raise ValueError('invalid origin')
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    mode = _ni_support._extend_mode_to_code(mode)
    _nd_image.uniform_filter1d(input, size, axis, output, mode, cval,
                               origin)
    return return_value


@docfiller
<<<<<<< HEAD
def uniform_filter(input, size = 3, output = None, mode = "reflect",
                   cval = 0.0, origin = 0):
=======
def uniform_filter(input, size=3, output=None, mode="reflect",
                   cval=0.0, origin=0):
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    """Multi-dimensional uniform filter.

    Parameters
    ----------
    %(input)s
<<<<<<< HEAD
    size : int or sequence of ints
=======
    size : int or sequence of ints, optional
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        The sizes of the uniform filter are given for each axis as a
        sequence, or as a single number, in which case the size is
        equal for all axes.
    %(output)s
    %(mode)s
    %(cval)s
    %(origin)s

    Notes
    -----
    The multi-dimensional filter is implemented as a sequence of
    one-dimensional uniform filters. The intermediate arrays are stored
    in the same data type as the output. Therefore, for output types
    with a limited precision, the results may be imprecise because
    intermediate results may be stored with insufficient precision.
    """
    input = numpy.asarray(input)
    output, return_value = _ni_support._get_output(output, input)
    sizes = _ni_support._normalize_sequence(size, input.ndim)
    origins = _ni_support._normalize_sequence(origin, input.ndim)
<<<<<<< HEAD
    axes = range(input.ndim)
=======
    axes = list(range(input.ndim))
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    axes = [(axes[ii], sizes[ii], origins[ii])
                           for ii in range(len(axes)) if sizes[ii] > 1]
    if len(axes) > 0:
        for axis, size, origin in axes:
            uniform_filter1d(input, int(size), axis, output, mode,
                             cval, origin)
            input = output
    else:
        output[...] = input[...]
    return return_value


@docfiller
<<<<<<< HEAD
def minimum_filter1d(input, size, axis = -1, output = None,
                     mode = "reflect", cval = 0.0, origin = 0):
=======
def minimum_filter1d(input, size, axis=-1, output=None,
                     mode="reflect", cval=0.0, origin=0):
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    """Calculate a one-dimensional minimum filter along the given axis.

    The lines of the array along the given axis are filtered with a
    minimum filter of given size.

    Parameters
    ----------
    %(input)s
    size : int
        length along which to calculate 1D minimum
    %(axis)s
    %(output)s
    %(mode)s
    %(cval)s
    %(origin)s
<<<<<<< HEAD
    """
    input = numpy.asarray(input)
    if numpy.iscomplexobj(input):
        raise TypeError, 'Complex type not supported'
    axis = _ni_support._check_axis(axis, input.ndim)
    if size < 1:
        raise RuntimeError, 'incorrect filter size'
    output, return_value = _ni_support._get_output(output, input)
    if (size // 2 + origin < 0) or (size // 2 + origin > size):
        raise ValueError, 'invalid origin'
=======

    Notes
    -----
    This function implements the MINLIST algorithm [1]_, as described by
    Richard Harter [2]_, and has a guaranteed O(n) performance, `n` being
    the `input` length, regardless of filter size.

    References
    ----------
    .. [1] http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.42.2777
    .. [2] http://www.richardhartersworld.com/cri/2001/slidingmin.html
    """
    input = numpy.asarray(input)
    if numpy.iscomplexobj(input):
        raise TypeError('Complex type not supported')
    axis = _ni_support._check_axis(axis, input.ndim)
    if size < 1:
        raise RuntimeError('incorrect filter size')
    output, return_value = _ni_support._get_output(output, input)
    if (size // 2 + origin < 0) or (size // 2 + origin >= size):
        raise ValueError('invalid origin')
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    mode = _ni_support._extend_mode_to_code(mode)
    _nd_image.min_or_max_filter1d(input, size, axis, output, mode, cval,
                                  origin, 1)
    return return_value


@docfiller
<<<<<<< HEAD
def maximum_filter1d(input, size, axis = -1, output = None,
                     mode = "reflect", cval = 0.0, origin = 0):
=======
def maximum_filter1d(input, size, axis=-1, output=None,
                     mode="reflect", cval=0.0, origin=0):
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    """Calculate a one-dimensional maximum filter along the given axis.

    The lines of the array along the given axis are filtered with a
    maximum filter of given size.

<<<<<<< HEAD
        Parameters
    ----------
    %(input)s
    size : int
        length along which to calculate 1D maximum
=======
    Parameters
    ----------
    %(input)s
    size : int
        Length along which to calculate the 1-D maximum.
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    %(axis)s
    %(output)s
    %(mode)s
    %(cval)s
    %(origin)s
<<<<<<< HEAD
    """
    input = numpy.asarray(input)
    if numpy.iscomplexobj(input):
        raise TypeError, 'Complex type not supported'
    axis = _ni_support._check_axis(axis, input.ndim)
    if size < 1:
        raise RuntimeError, 'incorrect filter size'
    output, return_value = _ni_support._get_output(output, input)
    if (size // 2 + origin < 0) or (size // 2 + origin > size):
        raise ValueError, 'invalid origin'
=======

    Returns
    -------
    maximum1d : ndarray, None
        Maximum-filtered array with same shape as input.
        None if `output` is not None

    Notes
    -----
    This function implements the MAXLIST algorithm [1]_, as described by
    Richard Harter [2]_, and has a guaranteed O(n) performance, `n` being
    the `input` length, regardless of filter size.

    References
    ----------
    .. [1] http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.42.2777
    .. [2] http://www.richardhartersworld.com/cri/2001/slidingmin.html

    """
    input = numpy.asarray(input)
    if numpy.iscomplexobj(input):
        raise TypeError('Complex type not supported')
    axis = _ni_support._check_axis(axis, input.ndim)
    if size < 1:
        raise RuntimeError('incorrect filter size')
    output, return_value = _ni_support._get_output(output, input)
    if (size // 2 + origin < 0) or (size // 2 + origin >= size):
        raise ValueError('invalid origin')
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    mode = _ni_support._extend_mode_to_code(mode)
    _nd_image.min_or_max_filter1d(input, size, axis, output, mode, cval,
                                  origin, 0)
    return return_value


def _min_or_max_filter(input, size, footprint, structure, output, mode,
                       cval, origin, minimum):
    if structure is None:
        if footprint is None:
            if size is None:
<<<<<<< HEAD
                raise RuntimeError, "no footprint provided"
            separable= True
        else:
            footprint = numpy.asarray(footprint)
            footprint = footprint.astype(bool)
            if numpy.alltrue(numpy.ravel(footprint),axis=0):
=======
                raise RuntimeError("no footprint provided")
            separable = True
        else:
            footprint = numpy.asarray(footprint)
            footprint = footprint.astype(bool)
            if numpy.alltrue(numpy.ravel(footprint), axis=0):
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
                size = footprint.shape
                footprint = None
                separable = True
            else:
                separable = False
    else:
        structure = numpy.asarray(structure, dtype=numpy.float64)
        separable = False
        if footprint is None:
            footprint = numpy.ones(structure.shape, bool)
        else:
            footprint = numpy.asarray(footprint)
            footprint = footprint.astype(bool)
    input = numpy.asarray(input)
    if numpy.iscomplexobj(input):
<<<<<<< HEAD
        raise TypeError, 'Complex type not supported'
=======
        raise TypeError('Complex type not supported')
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    output, return_value = _ni_support._get_output(output, input)
    origins = _ni_support._normalize_sequence(origin, input.ndim)
    if separable:
        sizes = _ni_support._normalize_sequence(size, input.ndim)
<<<<<<< HEAD
        axes = range(input.ndim)
        axes = [(axes[ii], sizes[ii], origins[ii])
                               for ii in range(len(axes)) if sizes[ii] > 1]
        if minimum:
            filter = minimum_filter1d
        else:
            filter = maximum_filter1d
        if len(axes) > 0:
            for axis, size, origin in axes:
                filter(input, int(size), axis, output, mode, cval, origin)
=======
        axes = list(range(input.ndim))
        axes = [(axes[ii], sizes[ii], origins[ii])
                               for ii in range(len(axes)) if sizes[ii] > 1]
        if minimum:
            filter_ = minimum_filter1d
        else:
            filter_ = maximum_filter1d
        if len(axes) > 0:
            for axis, size, origin in axes:
                filter_(input, int(size), axis, output, mode, cval, origin)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
                input = output
        else:
            output[...] = input[...]
    else:
        fshape = [ii for ii in footprint.shape if ii > 0]
        if len(fshape) != input.ndim:
<<<<<<< HEAD
            raise RuntimeError, 'footprint array has incorrect shape.'
        for origin, lenf in zip(origins, fshape):
            if (lenf // 2 + origin < 0) or (lenf // 2 + origin > lenf):
                raise ValueError, 'invalid origin'
=======
            raise RuntimeError('footprint array has incorrect shape.')
        for origin, lenf in zip(origins, fshape):
            if (lenf // 2 + origin < 0) or (lenf // 2 + origin >= lenf):
                raise ValueError('invalid origin')
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        if not footprint.flags.contiguous:
            footprint = footprint.copy()
        if structure is not None:
            if len(structure.shape) != input.ndim:
<<<<<<< HEAD
                raise RuntimeError, 'structure array has incorrect shape'
=======
                raise RuntimeError('structure array has incorrect shape')
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
            if not structure.flags.contiguous:
                structure = structure.copy()
        mode = _ni_support._extend_mode_to_code(mode)
        _nd_image.min_or_max_filter(input, footprint, structure, output,
                                    mode, cval, origins, minimum)
    return return_value


@docfiller
<<<<<<< HEAD
def minimum_filter(input, size = None, footprint = None, output = None,
      mode = "reflect", cval = 0.0, origin = 0):
=======
def minimum_filter(input, size=None, footprint=None, output=None,
      mode="reflect", cval=0.0, origin=0):
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    """Calculates a multi-dimensional minimum filter.

    Parameters
    ----------
    %(input)s
    %(size_foot)s
    %(output)s
    %(mode)s
    %(cval)s
    %(origin)s
    """
    return _min_or_max_filter(input, size, footprint, None, output, mode,
                              cval, origin, 1)


@docfiller
<<<<<<< HEAD
def maximum_filter(input, size = None, footprint = None, output = None,
      mode = "reflect", cval = 0.0, origin = 0):
=======
def maximum_filter(input, size=None, footprint=None, output=None,
      mode="reflect", cval=0.0, origin=0):
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    """Calculates a multi-dimensional maximum filter.

    Parameters
    ----------
    %(input)s
    %(size_foot)s
    %(output)s
    %(mode)s
    %(cval)s
    %(origin)s
    """
    return _min_or_max_filter(input, size, footprint, None, output, mode,
                              cval, origin, 0)


@docfiller
<<<<<<< HEAD
def _rank_filter(input, rank, size = None, footprint = None, output = None,
     mode = "reflect", cval = 0.0, origin = 0, operation = 'rank'):
    input = numpy.asarray(input)
    if numpy.iscomplexobj(input):
        raise TypeError, 'Complex type not supported'
    origins = _ni_support._normalize_sequence(origin, input.ndim)
    if footprint is None:
        if size is None:
            raise RuntimeError, "no footprint or filter size provided"
=======
def _rank_filter(input, rank, size=None, footprint=None, output=None,
     mode="reflect", cval=0.0, origin=0, operation='rank'):
    input = numpy.asarray(input)
    if numpy.iscomplexobj(input):
        raise TypeError('Complex type not supported')
    origins = _ni_support._normalize_sequence(origin, input.ndim)
    if footprint is None:
        if size is None:
            raise RuntimeError("no footprint or filter size provided")
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        sizes = _ni_support._normalize_sequence(size, input.ndim)
        footprint = numpy.ones(sizes, dtype=bool)
    else:
        footprint = numpy.asarray(footprint, dtype=bool)
    fshape = [ii for ii in footprint.shape if ii > 0]
    if len(fshape) != input.ndim:
<<<<<<< HEAD
        raise RuntimeError, 'filter footprint array has incorrect shape.'
    for origin, lenf in zip(origins, fshape):
        if (lenf // 2 + origin < 0) or (lenf // 2 + origin > lenf):
            raise ValueError, 'invalid origin'
=======
        raise RuntimeError('filter footprint array has incorrect shape.')
    for origin, lenf in zip(origins, fshape):
        if (lenf // 2 + origin < 0) or (lenf // 2 + origin >= lenf):
            raise ValueError('invalid origin')
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    if not footprint.flags.contiguous:
        footprint = footprint.copy()
    filter_size = numpy.where(footprint, 1, 0).sum()
    if operation == 'median':
        rank = filter_size // 2
    elif operation == 'percentile':
        percentile = rank
        if percentile < 0.0:
            percentile += 100.0
        if percentile < 0 or percentile > 100:
<<<<<<< HEAD
            raise RuntimeError, 'invalid percentile'
=======
            raise RuntimeError('invalid percentile')
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        if percentile == 100.0:
            rank = filter_size - 1
        else:
            rank = int(float(filter_size) * percentile / 100.0)
    if rank < 0:
        rank += filter_size
<<<<<<< HEAD
    if rank < 0  or rank >= filter_size:
        raise RuntimeError, 'rank not within filter footprint size'
    if rank == 0:
        return minimum_filter(input, None, footprint, output, mode, cval,
                              origin)
    elif rank == filter_size - 1:
        return maximum_filter(input, None, footprint, output, mode, cval,
                              origin)
=======
    if rank < 0 or rank >= filter_size:
        raise RuntimeError('rank not within filter footprint size')
    if rank == 0:
        return minimum_filter(input, None, footprint, output, mode, cval,
                              origins)
    elif rank == filter_size - 1:
        return maximum_filter(input, None, footprint, output, mode, cval,
                              origins)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    else:
        output, return_value = _ni_support._get_output(output, input)
        mode = _ni_support._extend_mode_to_code(mode)
        _nd_image.rank_filter(input, rank, footprint, output, mode, cval,
                              origins)
        return return_value


@docfiller
<<<<<<< HEAD
def rank_filter(input, rank, size = None, footprint = None, output = None,
      mode = "reflect", cval = 0.0, origin = 0):
=======
def rank_filter(input, rank, size=None, footprint=None, output=None,
      mode="reflect", cval=0.0, origin=0):
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    """Calculates a multi-dimensional rank filter.

    Parameters
    ----------
    %(input)s
<<<<<<< HEAD
    rank : integer
=======
    rank : int
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        The rank parameter may be less then zero, i.e., rank = -1
        indicates the largest element.
    %(size_foot)s
    %(output)s
    %(mode)s
    %(cval)s
    %(origin)s
    """
    return _rank_filter(input, rank, size, footprint, output, mode, cval,
                        origin, 'rank')


@docfiller
<<<<<<< HEAD
def median_filter(input, size = None, footprint = None, output = None,
      mode = "reflect", cval = 0.0, origin = 0):
    """
    Calculates a multi-dimensional median filter.

    Parameters
    ----------
    input : array-like
        input array to filter
    size : scalar or tuple, optional
        See footprint, below
    footprint : array, optional
        Either ``size`` or ``footprint`` must be defined.  ``size`` gives
        the shape that is taken from the input array, at every element
        position, to define the input to the filter function.
        ``footprint`` is a boolean array that specifies (implicitly) a
        shape, but also which of the elements within this shape will get
        passed to the filter function.  Thus ``size=(n,m)`` is equivalent
        to ``footprint=np.ones((n,m))``.  We adjust ``size`` to the number
        of dimensions of the input array, so that, if the input array is
        shape (10,10,10), and ``size`` is 2, then the actual size used is
        (2,2,2).
    output : array, optional
        The ``output`` parameter passes an array in which to store the
        filter output.
    mode : {'reflect','constant','nearest','mirror', 'wrap'}, optional
        The ``mode`` parameter determines how the array borders are
        handled, where ``cval`` is the value when mode is equal to
        'constant'. Default is 'reflect'
    cval : scalar, optional
        Value to fill past edges of input if ``mode`` is 'constant'. Default
        is 0.0
    origin : scalar, optional
        The ``origin`` parameter controls the placement of the filter.
        Default 0
=======
def median_filter(input, size=None, footprint=None, output=None,
                  mode="reflect", cval=0.0, origin=0):
    """
    Calculates a multidimensional median filter.

    Parameters
    ----------
    %(input)s
    %(size_foot)s
    %(output)s
    %(mode)s
    %(cval)s
    %(origin)s

    Returns
    -------
    median_filter : ndarray
        Return of same shape as `input`.
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    """
    return _rank_filter(input, 0, size, footprint, output, mode, cval,
                        origin, 'median')


@docfiller
<<<<<<< HEAD
def percentile_filter(input, percentile, size = None, footprint = None,
                 output = None, mode = "reflect", cval = 0.0, origin = 0):
=======
def percentile_filter(input, percentile, size=None, footprint=None,
                 output=None, mode="reflect", cval=0.0, origin=0):
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    """Calculates a multi-dimensional percentile filter.

    Parameters
    ----------
    %(input)s
    percentile : scalar
        The percentile parameter may be less then zero, i.e.,
        percentile = -20 equals percentile = 80
    %(size_foot)s
    %(output)s
    %(mode)s
    %(cval)s
    %(origin)s
    """
    return _rank_filter(input, percentile, size, footprint, output, mode,
                                   cval, origin, 'percentile')


@docfiller
<<<<<<< HEAD
def generic_filter1d(input, function, filter_size, axis = -1,
                 output = None, mode = "reflect", cval = 0.0, origin = 0,
                 extra_arguments = (), extra_keywords = None):
    """Calculate a one-dimensional filter along the given axis.

    generic_filter1d iterates over the lines of the array, calling the
=======
def generic_filter1d(input, function, filter_size, axis=-1,
                 output=None, mode="reflect", cval=0.0, origin=0,
                 extra_arguments=(), extra_keywords = None):
    """Calculate a one-dimensional filter along the given axis.

    `generic_filter1d` iterates over the lines of the array, calling the
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    given function at each line. The arguments of the line are the
    input line, and the output line. The input and output lines are 1D
    double arrays.  The input line is extended appropriately according
    to the filter size and origin. The output line must be modified
    in-place with the result.

    Parameters
    ----------
    %(input)s
    function : callable
<<<<<<< HEAD
        function to apply along given axis
    filter_size : scalar
        length of the filter
=======
        Function to apply along given axis.
    filter_size : scalar
        Length of the filter.
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    %(axis)s
    %(output)s
    %(mode)s
    %(cval)s
    %(origin)s
    %(extra_arguments)s
    %(extra_keywords)s
    """
    if extra_keywords is None:
        extra_keywords = {}
    input = numpy.asarray(input)
    if numpy.iscomplexobj(input):
<<<<<<< HEAD
        raise TypeError, 'Complex type not supported'
    output, return_value = _ni_support._get_output(output, input)
    if filter_size < 1:
        raise RuntimeError, 'invalid filter size'
    axis = _ni_support._check_axis(axis, input.ndim)
    if ((filter_size // 2 + origin < 0) or
        (filter_size // 2 + origin > filter_size)):
        raise ValueError, 'invalid origin'
=======
        raise TypeError('Complex type not supported')
    output, return_value = _ni_support._get_output(output, input)
    if filter_size < 1:
        raise RuntimeError('invalid filter size')
    axis = _ni_support._check_axis(axis, input.ndim)
    if (filter_size // 2 + origin < 0) or (filter_size // 2 + origin >=
                                           filter_size):
        raise ValueError('invalid origin')
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    mode = _ni_support._extend_mode_to_code(mode)
    _nd_image.generic_filter1d(input, function, filter_size, axis, output,
                      mode, cval, origin, extra_arguments, extra_keywords)
    return return_value


@docfiller
<<<<<<< HEAD
def generic_filter(input, function, size = None, footprint = None,
                   output = None, mode = "reflect", cval = 0.0, origin = 0,
                   extra_arguments = (), extra_keywords = None):
=======
def generic_filter(input, function, size=None, footprint=None,
                   output=None, mode="reflect", cval=0.0, origin=0,
                   extra_arguments=(), extra_keywords = None):
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    """Calculates a multi-dimensional filter using the given function.

    At each element the provided function is called. The input values
    within the filter footprint at that element are passed to the function
    as a 1D array of double values.

    Parameters
    ----------
    %(input)s
    function : callable
<<<<<<< HEAD
        function to apply at each element
=======
        Function to apply at each element.
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    %(size_foot)s
    %(output)s
    %(mode)s
    %(cval)s
    %(origin)s
    %(extra_arguments)s
    %(extra_keywords)s
    """
    if extra_keywords is None:
        extra_keywords = {}
    input = numpy.asarray(input)
    if numpy.iscomplexobj(input):
<<<<<<< HEAD
        raise TypeError, 'Complex type not supported'
    origins = _ni_support._normalize_sequence(origin, input.ndim)
    if footprint is None:
        if size is None:
            raise RuntimeError, "no footprint or filter size provided"
=======
        raise TypeError('Complex type not supported')
    origins = _ni_support._normalize_sequence(origin, input.ndim)
    if footprint is None:
        if size is None:
            raise RuntimeError("no footprint or filter size provided")
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        sizes = _ni_support._normalize_sequence(size, input.ndim)
        footprint = numpy.ones(sizes, dtype=bool)
    else:
        footprint = numpy.asarray(footprint)
        footprint = footprint.astype(bool)
    fshape = [ii for ii in footprint.shape if ii > 0]
    if len(fshape) != input.ndim:
<<<<<<< HEAD
        raise RuntimeError, 'filter footprint array has incorrect shape.'
    for origin, lenf in zip(origins, fshape):
        if (lenf // 2 + origin < 0) or (lenf // 2 + origin > lenf):
            raise ValueError, 'invalid origin'
=======
        raise RuntimeError('filter footprint array has incorrect shape.')
    for origin, lenf in zip(origins, fshape):
        if (lenf // 2 + origin < 0) or (lenf // 2 + origin >= lenf):
            raise ValueError('invalid origin')
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    if not footprint.flags.contiguous:
        footprint = footprint.copy()
    output, return_value = _ni_support._get_output(output, input)
    mode = _ni_support._extend_mode_to_code(mode)
    _nd_image.generic_filter(input, function, footprint, output, mode,
                         cval, origins, extra_arguments, extra_keywords)
    return return_value
