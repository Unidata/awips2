<<<<<<< HEAD
#
# spatial - Distances
#

from info import __doc__

__all__ = ['vq', 'hierarchy']

import vq, hierarchy
=======
"""
=========================================
Clustering package (:mod:`scipy.cluster`)
=========================================

.. currentmodule:: scipy.cluster

:mod:`scipy.cluster.vq`

Clustering algorithms are useful in information theory, target detection,
communications, compression, and other areas.  The `vq` module only
supports vector quantization and the k-means algorithms.

:mod:`scipy.cluster.hierarchy`

The `hierarchy` module provides functions for hierarchical and
agglomerative clustering.  Its features include generating hierarchical
clusters from distance matrices,
calculating statistics on clusters, cutting linkages
to generate flat clusters, and visualizing clusters with dendrograms.

"""
from __future__ import division, print_function, absolute_import

__all__ = ['vq', 'hierarchy']

from . import vq, hierarchy
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

from numpy.testing import Tester
test = Tester().test
