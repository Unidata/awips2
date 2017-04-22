##
# This software was developed and / or modified by Raytheon Company,
# pursuant to Contract DG133W-05-CQ-1067 with the US Government.
# 
# U.S. EXPORT CONTROLLED TECHNICAL DATA
# This software product contains export-restricted data whose
# export/transfer/disclosure is restricted by U.S. law. Dissemination
# to non-U.S. persons whether in the United States or abroad requires
# an export license or other authorization.
# 
# Contractor Name:        Raytheon Company
# Contractor Address:     6825 Pine Street, Suite 340
#                         Mail Stop B8
#                         Omaha, NE 68106
#                         402.291.0100
# 
# See the AWIPS II Master Rights File ("Master Rights File.pdf") for
# further licensing information.
##
########################################################################
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
#    BaseTool -- library of methods of useful functions, originally ripped out
#        of SmartScript since they're not specific to GFE and smart tools
#
# Author: njensen
# ----------------------------------------------------------------------------
########################################################################

import math

import UnitConvertor

class BaseTool(UnitConvertor.UnitConvertor):
    
    def __init__(self):    
        UnitConvertor.UnitConvertor.__init__(self)        
        
        
    def interpolateValues(self, height, (h1, v1), (h2, v2)):
        # Interpolate between the height and values

        # Determine height ratio
        heightRatio = (height - h1)/ (h2 - h1)

        # Check for vector or scalar values
        if type(v1) == types.TupleType and type(v2) == types.TupleType:
            heightRatio2 = (h2 - height) / (h2 - h1)
            s1, d1 = v1
            s2, d2 = v2
            uv1 = self.MagDirToUV(s1, d1)
            uv2 = self.MagDirToUV(s2, d2)
            u = heightRatio * uv1[0] + heightRatio2 * uv2[0]
            v = heightRatio * uv1[1] + heightRatio2 * uv2[1]
            result = self.UVToMagDir(u,v)
            return result

        else:
            diffV = v2 - v1
            return v1 + heightRatio * diffV

    def linear(self, xmin, xmax, ymin, ymax, we):
        m = (ymax - ymin) / (xmax - xmin + .0000001)
        b = ymin - m * xmin
        return m * we + b

    def extrapolate(self, height, (h1, v1),(h2, v2)):
        # Extrapolate from the height and values
        if type(v1) == types.TupleType:
            # Vector -- Work with mag only
            mag1,dir1 = v1
            mag2,dir2 = v2
            slope = (mag2-mag1)/(h2-h1)
            mag = mag1 + slope * (h1-height)
            return (mag,dir1)
        else:
            slope = (v2-v1)/(h2-h1)
            return v1 + slope * (h1-height)

    def interpolateScalarValues(self, height, (h1, v1), (h2, v2)):
        # Interpolate between the height and values

        # Determine height ratio
        heightRatio = (height - h1)/ (h2 - h1)
        diffV = v2 - v1
        return v1 + heightRatio * diffV

    def interpolateVectorValues(self, height, (h1, v1), (h2, v2)):
        # Interpolate between the height and values

        # Determine height ratio
        heightRatio = (height - h1)/ (h2 - h1)

        heightRatio2 = (h2 - height) / (h2 - h1)
        s1, d1 = v1
        s2, d2 = v2
        uv1 = self.MagDirToUV(s1, d1)
        uv2 = self.MagDirToUV(s2, d2)
        u = heightRatio * uv1[0] + heightRatio2 * uv2[0]
        v = heightRatio * uv1[1] + heightRatio2 * uv2[1]
        result = self.UVToMagDir(u,v)
        return result
    
    def getLevels(self, level1, level2, noDataError=1):
        # Return a list of levels between and including level1 and level2
        # Will do ascending or descending depending on order of arguments
        #    levels = self.getLevels("MB900", "MB500") # descending
        #    levels = self.getLevels("MB600", "MB1000") # ascending
        levels = []
        kinds = [("MB",50),("K", 5)]
        levelKind = None
        for kind, increment in kinds:
            if kind in level1:
                levelKind = kind
                levelInc = increment
        if levelKind is None or levelKind not in level2:
            return errorReturn(
                noDataError,
                "SmartScript.getLevels:: Illegal kind of level."+\
                "Must be MB or K.  Level1, Level2: "+level1+", "+level2)
        l1 = level1.replace(levelKind, "")
        l2 = level2.replace(levelKind, "")
        try:
            l1 = int(l1)
            l2 = int(l2)
        except:
            return errorReturn(
                noDataError,
                "SmartScript.getLevels:: Illegal level."+\
                "Level1, Level2: "+level1+", "+level2)
        if l1 > l2:
            levelInc = -levelInc
        for i in xrange(l1, l2, levelInc):
            levels.append(levelKind + str(i))
        return levels
    
    def round(self, val, mode, increment):
        if not (mode == "RoundUp" or mode == "RoundDown" or mode == "Nearest"):
            raise TypeError("mode is invalid: " + `mode`)
        # convert to float
        value = float(val)
        # check for the case where no work is needed.
        if value % increment == 0:
            return value

        sign = abs(value) / value
        delta = 0
        if mode == "RoundUp" and sign > 0:
            delta = sign * increment
        elif mode == "RoundDown" and sign < 0:
            delta = sign * increment

        if mode == "RoundUp":
            value = (int(value / increment) * increment) + delta
        elif mode == "RoundDown":
            value = (int(value / increment) * increment) + delta
        elif mode == "Nearest":
            value = int((value + (sign * increment / 2.0)) / increment) * increment
        return float(value)    
    
    def fformat(self, value, roundVal):
        # Return a string for the floating point value
        # truncated to the resolution given by roundVal
        if roundVal > 1.0:
            return str(int(value))
        else:
            exp = abs(int(math.floor(math.log10(roundVal))))
            formatString = "{:." + str(exp) + "f}"
            return formatString.format(value)


##    Taken from http://starship.python.net/crew/jhauser/NumAdd.py
##    by Janko Hauser
##    """
##    Module with some additional routines for NumPy. Names partly taken
##    from similar Matrix languages. Functions which nameclashes to
##    standard Python builtin names have an `a' prepended (amin, amax).
##
##    Some of the functions can take any multidimensional array. Currently
##    they are not save for given lists or other sequence types than arrays.
##    """
##    __version__ = '0.1.1'
##    __email__ = 'jhauser@ifm.uni-kiel.de'
##    __author__ = 'Janko Hauser'

    def isinf(m):
        """
        Returns a condition array, which is true where m has an Inf value.
        """
        n = isnan(m)
        return isnan(numpy.where(n,0,m)*0.)

    def isnan(m):
        """
        Returns a condition array, which is true where m has a NaN value.
        """
        return numpy.not_equal(m,m)

    def aindex(condition):
        """
        Show multidimensional indices where condition is true.
        Indix-convention is c-style (fastest last).
        """
        lin_index = numpy.nonzero(numpy.ravel(condition))
        sh = list(numpy.shape(condition))
        sh.reverse()
        new_index = numpy.zeros((len(lin_index), len(sh)))
        mod = numpy.zeros(len(lin_index))
        for j in numpy.arange(len(lin_index)):
            count=len(sh)
            for i in sh:
                lin_index[j], mod[j] = divmod(lin_index[j], i)
                count = count - 1
                new_index[j, count] = mod[j]
        return new_index

    def DelAxis(m):
        """
        Removes all axis with length one
        """
        sh = m.shape
        new_shape=[]
        for axis_length in sh:
            if axis_length > 1:
                new_shape.append(axis_length)
        return numpy.reshape(m,new_shape)

    def around(m, signif=0):
        """
        Should round in the way Python builtin round does it. Presume
        that this is the right way to do it.
        """
        m = numpy.asarray(m)
        s = sign(m)
        if signif:
            m = numpy.absolute(m*10.**signif)
        else:
            m = numpy.absolute(m)
        rem = m-m.astype(numpy.Int)
        m = numpy.where(numpy.less(rem,0.5), numpy.floor(m), numpy.ceil(m))
        # convert back
        if signif:
            m = m*s/(10.**signif)
        else:
            m = m*s
        return m

    def sign(m):
        """
        Gives an array with shape of m. Where array less than 0 a=-1,
        where m greater null a=1, elsewhere a=0.
        """
        m = numpy.asarray(m)

        if ((type(m) == type(1.4)) or (type(m) == type(1))):
            return m-m-numpy.less(m,0)+numpy.greater(m,0)
        else:
            return numpy.zeros(numpy.shape(m))-numpy.less(m,0)+numpy.greater(m,0)

    def diag(m, k=0):
        """
        Returns the diagonal of m with offset k.
        """
        v = numpy.asarray(m)
        s = numpy.shape(v)
        if len(s)==1:
            n = s[0]+numpy.absolute(k)
            if k > 0:
                v = numpy.concatenate(v,numpy.zeros(k, v.typecode()))
            elif k < 0:
                v = numpy.concatenate(numpy.zeros(-k, v.typecode()),v)
            return numpy.multiply(eye(n, k=k), v)
        elif len(s)==2:
            v = numpy.add.reduce(eye(s[0], s[1], k=k)*v)
            if k > 0: return v[:-k]
            elif k < 0: return v[-k:]
            else: return v


    def corrcoef(x, y=None):
        """
        The correlation coefficients of the two vectors x and y or for every
        column of x.
        """
        # Handle the shape tests in cov()
        c = cov(x, y)
        d = diag(c)
        return c/numpy.sqrt(numpy.multiply.outer(d,d))

    def cov(x,y=None):
        """
        Covariance matrix of colums of x or the two vectors x,y, where
        each vector represents one column.
        """
        if y:
            x = numpy.transpose(numpy.array([x,y], x.typecode()))
        mu = numpy.mean(x)
        sum_cov = 0.0
        for v in x:
            sum_cov = sum_cov+numpy.multiply.outer(v,v)
        return (sum_cov-len(x)*numpy.multiply.outer(mu,mu))/(len(x)-1)

    def amax(m,axis=0):
        """
        Returns the maximum values of m along the axis axis.
        If axis=None return the absolute maximum of m.
        """
        if axis == None:
            return numpy.maximum.reduce(numpy.ravel(m))
        else:
            new_shape=list(m.shape)
            del(new_shape[axis])
            return numpy.reshape(numpy.maximum.reduce(m,axis),new_shape)

    def amin(m,axis=0):
        """
        Returns the minimum values of m along the axis axis.
        If axis=None return the absolute minimum of m.
        """
        if axis == None:
            return numpy.minimum.reduce(numpy.ravel(m))
        else:
            new_shape=list(m.shape)
            del(new_shape[axis])
            return numpy.reshape(numpy.minimum.reduce(m,axis),new_shape)

    def mean(m,axis=0):
        """
        Returns the mean of m along axis axis.
        If axis=None return the overall mean of m.
        """
        if axis == None:
            return numpy.add.reduce(numpy.ravel(m))/(numpy.multiply.reduce(m.shape)*1.)
        else:
            new_shape=list(m.shape)
            del(new_shape[axis])
            return numpy.reshape(numpy.add.reduce(m,axis)/(m.shape[axis]*1.),new_shape)

    def var(m,axis=0):
        """
        Variance of m along axis axis.
        If axis=None return the overall variance.
        """
        mu = mean(m,axis)
        if axis == None:
            return (numpy.add.reduce(numpy.power(numpy.ravel(m)-mu,2))) / (numpy.multiply.reduce(
                m.shape)-1.)
        else:
            new_shape=list(m.shape)
            del(new_shape[axis])
            return numpy.reshape((numpy.add.reduce(numpy.power(m-mu,2),axis)) / (
                m.shape[axis]-1.),new_shape)

    def std(m,axis=0):
        """
        Standard deviation of m along axis axis.
        If axis=None return the overall standard deviation.
        """
        return numpy.sqrt(var(m,axis))

    def diff(m,axis=0):
        """
        Foward difference of m along axis axis.
        """
        if m.shape[axis] < 2:
            raise 'Error, axis needs at least be of length 2'

        l_sl=[slice(None,None,None)]*len(m.shape)
        u_sl=l_sl[:]
        l_sl[axis]=slice(1,None,1)
        u_sl[axis]=slice(None,-1,1)

        return m[l_sl]-m[u_sl]

    def ndiff(m,n=1,axis=0):
        """
        N-th forward difference along axis axis.
        """
        if m.shape[axis] < 2:
            raise 'Error, axis needs at least be of length 2'

        l_sl=[slice(None,None,None)]*len(m.shape)
        u_sl=l_sl[:]
        l_sl[axis]=slice(1,None,1)
        u_sl[axis]=slice(None,-1,1)

        if n >= 1:
            return ndiff(m[l_sl]-m[u_sl],n-1,axis)
        else:
            return m[l_sl]-m[u_sl]

    def zonec(m,axis=0):
        """
        Reduce the field from the corners to the middle of the vertices
        """
        if m.shape[axis] < 2:
            raise 'Error, axis needs at least be of length 2'
        l_sl=[slice(None,None,None)]*len(m.shape)
        u_sl=l_sl[:]
        u_sl[axis]=slice(1,None,1)
        l_sl[axis]=slice(None,-1,1)

        return (m[l_sl]+m[u_sl])*0.5

    def gradient(var, x, axis=0):
        """
        Calculate the partial derivative of var in x. var and x are
        of the same shape.
        """
        return diff(var, axis=axis) / diff(x, axis=axis)

    def maverage(x, width):
        """maverage(x, width)

        Creates running mean of width over vector x. x needs to be
        of 1d.

        e.g width = 5

        x =    1 2 3 4 5 6 7 8 9 10 11 12 13 14 15

        1       5 6 7 8 9    11 12 13 14 15
        1 2 3     6 7 8 9 10       13 14 15
        1 2 3 4 5   7 8 9 10 11          15
        2 3 4 5 5   8 9 10 11 12
        3 4 5 6 7   9 10 11 12 13
        4 5 6 7 8   10 11 12 13 14
        """
        # test for right shape
        x = numpy.asarray(x)
        if len(x.shape) > 1:
            raise 'Error in rmean, input array x needs to be 1d'

        # construct a matrix where the elements of x are repeated with
        # a shift of one. Can this be used in general?
        xbar = numpy.zeros(x.shape,'d')
        w = int(numpy.floor(width/2.))
        lx = len(x)
        l = lx - width+1
        A = numpy.indices((l,width))[1]+1
        Y = numpy.indices((l,width))[0]
        B = A+Y
        U = numpy.reshape(numpy.take(x,numpy.ravel(numpy.transpose(B))-1), (width,l))
        xbar[w:lx-w] = mean(U)

        # do the start and end points (width/2) by hand
        for i in numpy.arange(w)+1:
            xbar[i-1] = mean(x[:i*2-1])[0]

        for i in numpy.arange(w)+1:
            xbar[lx-i] = mean(x[lx - (i*2) + 1:])[0]

        return xbar,U

    def  raverage(x, width, offset=0):
        """raverage(x, width, offset=0)

           average creates a centered mean over width data points

           e.g. width = 5

           x     = 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
                   --------- ---------- --------------
           xbar  =     3         8            13

           e.g. width = 4

           x     = 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
                   ------- ------- ----------
           xbar  =   2.5      6.5     10.5

           e.g. width = 4, offset = 1

           x     = 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
                     ------- ------- -----------
           xbar  =     3.5      7.5     11.5

        """
        if offset:
            x = take(x,(numpy.arange(offset, len(x))))

        width = width*1.
        l  = len(x)-width
        ll = numpy.floor(len(x)/width);
        i  = numpy.arange(width)
        A  = numpy.ones((ll,width))*i[numpy.NewAxis,:]
        j  = numpy.arange(0,width,l)
        Y  = numpy.transpose(numpy.ones((width,1))*j[numpy.NewAxis,:])
        B  = A + Y
        U  = numpy.reshape(numpy.take(x,numpy.ravel(numpy.transpose(B)).astype(numpy.Int)),(width,ll))
        xbar = mean(U)

        return xbar

    
        