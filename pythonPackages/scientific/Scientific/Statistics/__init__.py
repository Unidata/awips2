# Basic statistics functions.
#
# Written by Konrad Hinsen <hinsen@cnrs-orleans.fr>
# With contributions from Moshe Zadka <mzadka@geocities.com>
# last revision: 2006-5-28
#

"""
Basic monovariate statistics
"""

from Scientific import N

#
# Univariate statistics functions
#

def moment(data, order, about=None, theoretical=1):
    """
    Calculate the moments of the distribution corresponding to a set of data
    
    @param data: a sequence of data points
    @type data: C{Numeric.array} of numbers
    @param order: the order of the moment to be calculated
    @type order: C{int}
    @param about: the center of the distribution. If C{None}, use the
                  mean value as the center.
    @type about: number
    @param theoretical: for internal use
    @returns: the moment of the given order about the specified center
    @rtype: number
    """
    data = 1.*N.array(data)
    if about is None:
        about = mean(data)
        theoretical = 0
    ln = len(data)-(1-theoretical)
    return 1.*N.add.reduce((data-about)**order)/ln

def mean(data):
    """
    Mean (average value, first moment)

    @param data: a sequence of numbers
    @type data: C{list} or C{Numeric.array}
    @returns: the mean of the number sequence
    @rtype: number
    """
    return moment(data, 1, 0)

average = mean

def weightedMean(data, sigma):
    """
    Weighted mean of a sequence of numbers with given standard deviations

    @param data: a sequence of measurements
    @type data: C{list} or C{Numeric.array}
    @param sigma: a sequence of corresponding standard deviations
    @type sigma: C{list} or C{Numeric.array}
    @returns: the weighted mean and corresponding standard deviation
    @rtype: C{tuple} of two elements
    """
    if len(data) != len(sigma):
        raise ValueError
    data = 1.*N.array(data)
    sigma = 1.*N.array(sigma)
    nom = N.sum(data/sigma**2)
    denom = N.sum(1./sigma**2)
    mean = nom / denom
    sig = N.sqrt(1./denom)
    return mean, sig

def variance(data):
    """
    Variance (second moment)

    @param data: a sequence of numbers
    @type data: C{list} or C{Numeric.array}
    @returns: the variance of the number sequence
    @rtype: number
    """
    return moment(data, 2)

def standardDeviation(data):
    """
    Standard deviation

    @param data: a sequence of numbers
    @type data: C{list} or C{Numeric.array}
    @returns: the standard deviation of the number sequence
    @rtype: number
    """
    return N.sqrt(variance(data))

def median(data):
    """
    Median

    @param data: a sequence of numbers
    @type data: C{list} or C{Numeric.array}
    @returns: the median of the number sequence
    @rtype: number
    """
    data = N.sort(N.array(data))
    l = (len(data)-1)/2.
    return (data[int(N.floor(l))]+data[int(N.ceil(l))])/2.

def mode(data):
    h = {}
    for n in data:
        try: h[n] = h[n]+1
        except KeyError: h[n] = 1
    a = map(lambda x: (x[1], x[0]), h.items())
    return max(a)[1]

def normalizedMoment(data, order):
    """
    Calculate the normalized moments of the distribution corresponding
    to a set of data. Normalization means division by the n-th power of
    the standard deviation.
    
    @param data: a sequence of data points
    @type data: C{Numeric.array} of numbers
    @param order: the order of the moment to be calculated
    @type order: C{int}
    @returns: the normalized moment of the given order
    @rtype: number
    """
    mn = mean(data)
    return moment(data, order, mn)/N.sqrt(moment(data, 2, mn)**order)

def skewness(data):
    """
    Skewness (third normalized moment)

    @param data: a sequence of numbers
    @type data: C{list} or C{Numeric.array}
    @returns: the skewness of the number sequence
    @rtype: number
    """
    return normalizedMoment(data, 3)

def kurtosis(data):
    """
    Kurtosis (fourth normalized moment)

    @param data: a sequence of numbers
    @type data: C{list} or C{Numeric.array}
    @returns: the kurtosis of the number sequence
    @rtype: number
    """
    return normalizedMoment(data, 4)

##  def chiSquare(data):
##      h = {}
##      for n in data:
##         try: h[n] = h[n]+1
##         except KeyError: h[n] = 1
##      h = N.array(h.values())
##      h = h/N.add.reduce(h)
##      return moment(h, 2, 1./len(h))

def correlation(data1, data2):
    """
    Calculates the correlation coefficient between two data sequences

    @param data1: first data sequence
    @type data1: C{Numeric.array} or C{list}
    @param data2: second data sequence of length identical to data1
    @type data2: C{Numeric.array} or C{list}
    @returns: the correlation coefficient between data1 and data2
    @rtype: number
    @raises ValueError: if data1 and data2 have different lengths
    """
    if len(data1) != len(data2):
        raise ValueError("data series must have equal length")
    data1 = N.array(data1)
    data2 = N.array(data2)
    data1 = data1 - N.add.reduce(data1)/len(data1)
    data2 = data2 - N.add.reduce(data2)/len(data2)
    return N.add.reduce(data1*data2) / \
           N.sqrt(N.add.reduce(data1*data1) \
                        * N.add.reduce(data2*data2))
