#! /usr/bin/env python
#
# Author: Damian Eads
# Date: April 17, 2008
#
# Copyright (C) 2008 Damian Eads
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

import os.path

import numpy as np
from numpy.testing import *

from scipy.cluster.hierarchy import linkage, from_mlab_linkage, to_mlab_linkage, num_obs_linkage, inconsistent, cophenet, from_mlab_linkage, fclusterdata, fcluster, is_isomorphic, single, complete, average, weighted, centroid, median, ward, leaders, correspond, is_monotonic, maxdists, maxinconsts, maxRstat, is_valid_linkage, is_valid_im, to_tree, leaves_list
from scipy.spatial.distance import squareform, pdist

_tdist = np.array([[0,    662,  877,  255,  412,  996],
                   [662,  0,    295,  468,  268,  400],
                   [877,  295,  0,    754,  564,  138],
                   [255,  468,  754,  0,    219,  869],
                   [412,  268,  564,  219,  0,    669],
                   [996,  400,  138,  869,  669,  0  ]], dtype='double')

_ytdist = squareform(_tdist)


eo = {}

_filenames = ["iris.txt",
              "Q-X.txt",
              "fclusterdata-maxclusts-2.txt",
              "fclusterdata-maxclusts-3.txt",
              "fclusterdata-maxclusts-4.txt",
              "linkage-single-tdist.txt",
              "linkage-complete-tdist.txt",
              "linkage-average-tdist.txt",
              "linkage-weighted-tdist.txt",
              "inconsistent-Q-single-1.txt",
              "inconsistent-Q-single-2.txt",
              "inconsistent-Q-single-3.txt",
              "inconsistent-Q-single-4.txt",
              "inconsistent-Q-single-5.txt",
              "inconsistent-Q-single-6.txt",
              "inconsistent-complete-tdist-depth-1.txt",
              "inconsistent-complete-tdist-depth-2.txt",
              "inconsistent-complete-tdist-depth-3.txt",
              "inconsistent-complete-tdist-depth-4.txt",
              "inconsistent-single-tdist-depth-0.txt",
              "inconsistent-single-tdist-depth-1.txt",
              "inconsistent-single-tdist-depth-2.txt",
              "inconsistent-single-tdist-depth-3.txt",
              "inconsistent-single-tdist-depth-4.txt",
              "inconsistent-single-tdist-depth-5.txt",
              "inconsistent-single-tdist.txt",
              "inconsistent-weighted-tdist-depth-1.txt",
              "inconsistent-weighted-tdist-depth-2.txt",
              "inconsistent-weighted-tdist-depth-3.txt",
              "inconsistent-weighted-tdist-depth-4.txt",
              "linkage-Q-average.txt",
              "linkage-Q-complete.txt",
              "linkage-Q-single.txt",
              "linkage-Q-weighted.txt",
              "linkage-Q-centroid.txt",
              "linkage-Q-median.txt",
              "linkage-Q-ward.txt"
              ]

def load_testing_files():
    for fn in _filenames:
        name = fn.replace(".txt", "").replace("-ml", "")
        fqfn = os.path.join(os.path.dirname(__file__), fn)
        eo[name] = np.loadtxt(open(fqfn))
        #print "%s: %s   %s" % (name, str(eo[name].shape), str(eo[name].dtype))
    #eo['pdist-boolean-inp'] = np.bool_(eo['pdist-boolean-inp'])

load_testing_files()

class TestLinkage(TestCase):

    def test_linkage_empty_distance_matrix(self):
        "Tests linkage(Y) where Y is a 0x4 linkage matrix. Exception expected."
        y = np.zeros((0,))
        self.failUnlessRaises(ValueError, linkage, y)

    ################### linkage
    def test_linkage_single_tdist(self):
        "Tests linkage(Y, 'single') on the tdist data set."
        Z = linkage(_ytdist, 'single')
        Zmlab = eo['linkage-single-tdist']
        eps = 1e-10
        expectedZ = from_mlab_linkage(Zmlab)
        self.failUnless(within_tol(Z, expectedZ, eps))

    def test_linkage_complete_tdist(self):
        "Tests linkage(Y, 'complete') on the tdist data set."
        Z = linkage(_ytdist, 'complete')
        Zmlab = eo['linkage-complete-tdist']
        eps = 1e-10
        expectedZ = from_mlab_linkage(Zmlab)
        self.failUnless(within_tol(Z, expectedZ, eps))

    def test_linkage_average_tdist(self):
        "Tests linkage(Y, 'average') on the tdist data set."
        Z = linkage(_ytdist, 'average')
        Zmlab = eo['linkage-average-tdist']
        eps = 1e-05
        expectedZ = from_mlab_linkage(Zmlab)
        #print Z, expectedZ, np.abs(Z - expectedZ).max()
        self.failUnless(within_tol(Z, expectedZ, eps))

    def test_linkage_weighted_tdist(self):
        "Tests linkage(Y, 'weighted') on the tdist data set."
        Z = linkage(_ytdist, 'weighted')
        Zmlab = eo['linkage-weighted-tdist']
        eps = 1e-10
        expectedZ = from_mlab_linkage(Zmlab)
        #print Z, expectedZ, np.abs(Z - expectedZ).max()
        self.failUnless(within_tol(Z, expectedZ, eps))

    ################### linkage on Q
    def test_linkage_single_q(self):
        "Tests linkage(Y, 'single') on the Q data set."
        X = eo['Q-X']
        Z = single(X)
        Zmlab = eo['linkage-Q-single']
        eps = 1e-06
        expectedZ = from_mlab_linkage(Zmlab)
        #print abs(Z-expectedZ).max()
        self.failUnless(within_tol(Z, expectedZ, eps))

    def test_linkage_complete_q(self):
        "Tests linkage(Y, 'complete') on the Q data set."
        X = eo['Q-X']
        Z = complete(X)
        Zmlab = eo['linkage-Q-complete']
        eps = 1e-07
        expectedZ = from_mlab_linkage(Zmlab)
        #print abs(Z-expectedZ).max()
        self.failUnless(within_tol(Z, expectedZ, eps))

    def test_linkage_centroid_q(self):
        "Tests linkage(Y, 'centroid') on the Q data set."
        X = eo['Q-X']
        Z = centroid(X)
        Zmlab = eo['linkage-Q-centroid']
        eps = 1e-07
        expectedZ = from_mlab_linkage(Zmlab)
        #print abs(Z-expectedZ).max()
        self.failUnless(within_tol(Z, expectedZ, eps))

    def test_linkage_weighted_q(self):
        "Tests linkage(Y, 'weighted') on the Q data set."
        X = eo['Q-X']
        Z = weighted(X)
        Zmlab = eo['linkage-Q-weighted']
        eps = 1e-07
        expectedZ = from_mlab_linkage(Zmlab)
        #print abs(Z-expectedZ).max()
        self.failUnless(within_tol(Z, expectedZ, eps))

class TestInconsistent(TestCase):

    def test_single_inconsistent_tdist_1(self):
        "Tests inconsistency matrix calculation (depth=1) on a single linkage."
        Y = squareform(_tdist)
        Z = linkage(Y, 'single')
        R = inconsistent(Z, 1)
        Rright = eo['inconsistent-single-tdist-depth-1']
        eps = 1e-15
        #print np.abs(R - Rright).max()
        self.failUnless(within_tol(R, Rright, eps))

    def test_single_inconsistent_tdist_2(self):
        "Tests inconsistency matrix calculation (depth=2) on a single linkage."
        Y = squareform(_tdist)
        Z = linkage(Y, 'single')
        R = inconsistent(Z, 2)
        Rright = eo['inconsistent-single-tdist-depth-2']
        eps = 1e-05
        #print np.abs(R - Rright).max()
        self.failUnless(within_tol(R, Rright, eps))

    def test_single_inconsistent_tdist_3(self):
        "Tests inconsistency matrix calculation (depth=3) on a single linkage."
        Y = squareform(_tdist)
        Z = linkage(Y, 'single')
        R = inconsistent(Z, 3)
        Rright = eo['inconsistent-single-tdist-depth-3']
        eps = 1e-05
        #print np.abs(R - Rright).max()
        self.failUnless(within_tol(R, Rright, eps))

    def test_single_inconsistent_tdist_4(self):
        "Tests inconsistency matrix calculation (depth=4) on a single linkage."
        Y = squareform(_tdist)
        Z = linkage(Y, 'single')
        R = inconsistent(Z, 4)
        Rright = eo['inconsistent-single-tdist-depth-4']
        eps = 1e-05
        #print np.abs(R - Rright).max()
        self.failUnless(within_tol(R, Rright, eps))

    # with complete linkage...

    def test_complete_inconsistent_tdist_1(self):
        "Tests inconsistency matrix calculation (depth=1) on a complete linkage."
        Y = squareform(_tdist)
        Z = linkage(Y, 'complete')
        R = inconsistent(Z, 1)
        Rright = eo['inconsistent-complete-tdist-depth-1']
        eps = 1e-15
        #print np.abs(R - Rright).max()
        self.failUnless(within_tol(R, Rright, eps))

    def test_complete_inconsistent_tdist_2(self):
        "Tests inconsistency matrix calculation (depth=2) on a complete linkage."
        Y = squareform(_tdist)
        Z = linkage(Y, 'complete')
        R = inconsistent(Z, 2)
        Rright = eo['inconsistent-complete-tdist-depth-2']
        eps = 1e-05
        #print np.abs(R - Rright).max()
        self.failUnless(within_tol(R, Rright, eps))

    def test_complete_inconsistent_tdist_3(self):
        "Tests inconsistency matrix calculation (depth=3) on a complete linkage."
        Y = squareform(_tdist)
        Z = linkage(Y, 'complete')
        R = inconsistent(Z, 3)
        Rright = eo['inconsistent-complete-tdist-depth-3']
        eps = 1e-05
        #print np.abs(R - Rright).max()
        self.failUnless(within_tol(R, Rright, eps))

    def test_complete_inconsistent_tdist_4(self):
        "Tests inconsistency matrix calculation (depth=4) on a complete linkage."
        Y = squareform(_tdist)
        Z = linkage(Y, 'complete')
        R = inconsistent(Z, 4)
        Rright = eo['inconsistent-complete-tdist-depth-4']
        eps = 1e-05
        #print np.abs(R - Rright).max()
        self.failUnless(within_tol(R, Rright, eps))

    # with single linkage and Q data set

    def test_single_inconsistent_Q_1(self):
        "Tests inconsistency matrix calculation (depth=1, dataset=Q) with single linkage."
        X = eo['Q-X']
        Z = linkage(X, 'single', 'euclidean')
        R = inconsistent(Z, 1)
        Rright = eo['inconsistent-Q-single-1']
        eps = 1e-06
        #print np.abs(R - Rright).max()
        self.failUnless(within_tol(R, Rright, eps))

    def test_single_inconsistent_Q_2(self):
        "Tests inconsistency matrix calculation (depth=2, dataset=Q) with single linkage."
        X = eo['Q-X']
        Z = linkage(X, 'single', 'euclidean')
        R = inconsistent(Z, 2)
        Rright = eo['inconsistent-Q-single-2']
        eps = 1e-06
        #print np.abs(R - Rright).max()
        self.failUnless(within_tol(R, Rright, eps))

    def test_single_inconsistent_Q_3(self):
        "Tests inconsistency matrix calculation (depth=3, dataset=Q) with single linkage."
        X = eo['Q-X']
        Z = linkage(X, 'single', 'euclidean')
        R = inconsistent(Z, 3)
        Rright = eo['inconsistent-Q-single-3']
        eps = 1e-05
        #print np.abs(R - Rright).max()
        self.failUnless(within_tol(R, Rright, eps))

    def test_single_inconsistent_Q_4(self):
        "Tests inconsistency matrix calculation (depth=4, dataset=Q) with single linkage."
        X = eo['Q-X']
        Z = linkage(X, 'single', 'euclidean')
        R = inconsistent(Z, 4)
        Rright = eo['inconsistent-Q-single-4']
        eps = 1e-05
        #print np.abs(R - Rright).max()
        self.failUnless(within_tol(R, Rright, eps))

class TestCopheneticDistance(TestCase):

    def test_linkage_cophenet_tdist_Z(self):
        "Tests cophenet(Z) on tdist data set."
        expectedM = np.array([268, 295, 255, 255, 295, 295, 268, 268, 295, 295, 295, 138, 219, 295, 295]);
        Z = linkage(_ytdist, 'single')
        M = cophenet(Z)
        eps = 1e-10
        self.failUnless(within_tol(M, expectedM, eps))

    def test_linkage_cophenet_tdist_Z_Y(self):
        "Tests cophenet(Z, Y) on tdist data set."
        Z = linkage(_ytdist, 'single')
        (c, M) = cophenet(Z, _ytdist)
        expectedM = np.array([268, 295, 255, 255, 295, 295, 268, 268, 295, 295, 295, 138, 219, 295, 295]);
        expectedc = 0.639931296433393415057366837573
        eps = 1e-10
        self.failUnless(np.abs(c - expectedc) <= eps)
        self.failUnless(within_tol(M, expectedM, eps))

class TestFromMLabLinkage(TestCase):

    def test_from_mlab_linkage_empty(self):
        "Tests from_mlab_linkage on empty linkage array."
        X = np.asarray([])
        R = from_mlab_linkage([])
        self.failUnless((R == X).all())

    def test_from_mlab_linkage_single_row(self):
        "Tests from_mlab_linkage on linkage array with single row."
        expectedZP = np.asarray([[ 0.,  1.,  3.,  2.]])
        Z = [[1,2,3]]
        ZP = from_mlab_linkage(Z)
        return self.failUnless((ZP == expectedZP).all())

    def test_from_mlab_linkage_multiple_rows(self):
        "Tests from_mlab_linkage on linkage array with multiple rows."
        Z = np.asarray([[3, 6, 138], [4, 5, 219],
                        [1, 8, 255], [2, 9, 268], [7, 10, 295]])
        expectedZS = np.array([[   2.,    5.,  138.,    2.],
                               [   3.,    4.,  219.,    2.],
                               [   0.,    7.,  255.,    3.],
                               [   1.,    8.,  268.,    4.],
                               [   6.,    9.,  295.,    6.]],
                              dtype=np.double)
        ZS = from_mlab_linkage(Z)
        #print expectedZS, ZS
        self.failUnless((expectedZS == ZS).all())


class TestToMLabLinkage(TestCase):

    def test_to_mlab_linkage_empty(self):
        "Tests to_mlab_linkage on empty linkage array."
        X = np.asarray([])
        R = to_mlab_linkage([])
        self.failUnless((R == X).all())

    def test_to_mlab_linkage_single_row(self):
        "Tests to_mlab_linkage on linkage array with single row."
        Z = np.asarray([[ 0.,  1.,  3.,  2.]])
        expectedZP = np.asarray([[1,2,3]])
        ZP = to_mlab_linkage(Z)
        return self.failUnless((ZP == expectedZP).all())

    def test_from_mlab_linkage_multiple_rows(self):
        "Tests to_mlab_linkage on linkage array with multiple rows."
        expectedZM = np.asarray([[3, 6, 138], [4, 5, 219],
                        [1, 8, 255], [2, 9, 268], [7, 10, 295]])
        Z = np.array([[   2.,    5.,  138.,    2.],
                      [   3.,    4.,  219.,    2.],
                      [   0.,    7.,  255.,    3.],
                      [   1.,    8.,  268.,    4.],
                      [   6.,    9.,  295.,    6.]],
                     dtype=np.double)
        ZM = to_mlab_linkage(Z)
        #print expectedZM, ZM
        self.failUnless((expectedZM == ZM).all())

class TestFcluster(TestCase):

    def test_fclusterdata_maxclusts_2(self):
        "Tests fclusterdata(X, criterion='maxclust', t=2) on a random 3-cluster data set."
        expectedT = np.int_(eo['fclusterdata-maxclusts-2'])
        X = eo['Q-X']
        T = fclusterdata(X, criterion='maxclust', t=2)
        self.failUnless(is_isomorphic(T, expectedT))

    def test_fclusterdata_maxclusts_3(self):
        "Tests fclusterdata(X, criterion='maxclust', t=3) on a random 3-cluster data set."
        expectedT = np.int_(eo['fclusterdata-maxclusts-3'])
        X = eo['Q-X']
        T = fclusterdata(X, criterion='maxclust', t=3)
        self.failUnless(is_isomorphic(T, expectedT))

    def test_fclusterdata_maxclusts_4(self):
        "Tests fclusterdata(X, criterion='maxclust', t=4) on a random 3-cluster data set."
        expectedT = np.int_(eo['fclusterdata-maxclusts-4'])
        X = eo['Q-X']
        T = fclusterdata(X, criterion='maxclust', t=4)
        self.failUnless(is_isomorphic(T, expectedT))

    def test_fcluster_maxclusts_2(self):
        "Tests fcluster(Z, criterion='maxclust', t=2) on a random 3-cluster data set."
        expectedT = np.int_(eo['fclusterdata-maxclusts-2'])
        X = eo['Q-X']
        Y = pdist(X)
        Z = linkage(Y)
        T = fcluster(Z, criterion='maxclust', t=2)
        self.failUnless(is_isomorphic(T, expectedT))

    def test_fcluster_maxclusts_3(self):
        "Tests fcluster(Z, criterion='maxclust', t=3) on a random 3-cluster data set."
        expectedT = np.int_(eo['fclusterdata-maxclusts-3'])
        X = eo['Q-X']
        Y = pdist(X)
        Z = linkage(Y)
        T = fcluster(Z, criterion='maxclust', t=3)
        self.failUnless(is_isomorphic(T, expectedT))

    def test_fcluster_maxclusts_4(self):
        "Tests fcluster(Z, criterion='maxclust', t=4) on a random 3-cluster data set."
        expectedT = np.int_(eo['fclusterdata-maxclusts-4'])
        X = eo['Q-X']
        Y = pdist(X)
        Z = linkage(Y)
        T = fcluster(Z, criterion='maxclust', t=4)
        self.failUnless(is_isomorphic(T, expectedT))

class TestLeaders(TestCase):

    def test_leaders_single(self):
        "Tests leaders using a flat clustering generated by single linkage."
        X = eo['Q-X']
=======
from __future__ import division, print_function, absolute_import

import numpy as np
from numpy.testing import (TestCase, run_module_suite, dec, assert_raises,
                           assert_allclose, assert_equal, assert_)

from scipy._lib.six import xrange, u

import scipy.cluster.hierarchy
from scipy.cluster.hierarchy import (
    linkage, from_mlab_linkage, to_mlab_linkage, num_obs_linkage, inconsistent,
    cophenet, fclusterdata, fcluster, is_isomorphic, single, leaders,
    correspond, is_monotonic, maxdists, maxinconsts, maxRstat,
    is_valid_linkage, is_valid_im, to_tree, leaves_list, dendrogram,
    set_link_color_palette)
from scipy.spatial.distance import pdist

import hierarchy_test_data


# Matplotlib is not a scipy dependency but is optionally used in dendrogram, so
# check if it's available
try:
    import matplotlib
    # and set the backend to be Agg (no gui)
    matplotlib.use('Agg')
    # before importing pyplot
    import matplotlib.pyplot as plt
    have_matplotlib = True
except:
    have_matplotlib = False


class TestLinkage(object):
    def test_linkage_empty_distance_matrix(self):
        # Tests linkage(Y) where Y is a 0x4 linkage matrix. Exception expected.
        y = np.zeros((0,))
        assert_raises(ValueError, linkage, y)

    def test_linkage_tdist(self):
        for method in ['single', 'complete', 'average', 'weighted', u('single')]:
            yield self.check_linkage_tdist, method

    def check_linkage_tdist(self, method):
        # Tests linkage(Y, method) on the tdist data set.
        Z = linkage(hierarchy_test_data.ytdist, method)
        expectedZ = getattr(hierarchy_test_data, 'linkage_ytdist_' + method)
        assert_allclose(Z, expectedZ, atol=1e-10)

    def test_linkage_X(self):
        for method in ['centroid', 'median', 'ward']:
            yield self.check_linkage_q, method

    def check_linkage_q(self, method):
        # Tests linkage(Y, method) on the Q data set.
        Z = linkage(hierarchy_test_data.X, method)
        expectedZ = getattr(hierarchy_test_data, 'linkage_X_' + method)
        assert_allclose(Z, expectedZ, atol=1e-06)


class TestInconsistent(object):
    def test_inconsistent_tdist(self):
        for depth in hierarchy_test_data.inconsistent_ytdist:
            yield self.check_inconsistent_tdist, depth

    def check_inconsistent_tdist(self, depth):
        Z = hierarchy_test_data.linkage_ytdist_single
        assert_allclose(inconsistent(Z, depth),
                        hierarchy_test_data.inconsistent_ytdist[depth])


class TestCopheneticDistance(object):
    def test_linkage_cophenet_tdist_Z(self):
        # Tests cophenet(Z) on tdist data set.
        expectedM = np.array([268, 295, 255, 255, 295, 295, 268, 268, 295, 295,
                              295, 138, 219, 295, 295])
        Z = hierarchy_test_data.linkage_ytdist_single
        M = cophenet(Z)
        assert_allclose(M, expectedM, atol=1e-10)

    def test_linkage_cophenet_tdist_Z_Y(self):
        # Tests cophenet(Z, Y) on tdist data set.
        Z = hierarchy_test_data.linkage_ytdist_single
        (c, M) = cophenet(Z, hierarchy_test_data.ytdist)
        expectedM = np.array([268, 295, 255, 255, 295, 295, 268, 268, 295, 295,
                              295, 138, 219, 295, 295])
        expectedc = 0.639931296433393415057366837573
        assert_allclose(c, expectedc, atol=1e-10)
        assert_allclose(M, expectedM, atol=1e-10)


class TestMLabLinkageConversion(object):
    def test_mlab_linkage_conversion_empty(self):
        # Tests from/to_mlab_linkage on empty linkage array.
        X = np.asarray([])
        assert_equal(from_mlab_linkage([]), X)
        assert_equal(to_mlab_linkage([]), X)

    def test_mlab_linkage_conversion_single_row(self):
        # Tests from/to_mlab_linkage on linkage array with single row.
        Z = np.asarray([[0., 1., 3., 2.]])
        Zm = [[1, 2, 3]]
        assert_equal(from_mlab_linkage(Zm), Z)
        assert_equal(to_mlab_linkage(Z), Zm)

    def test_mlab_linkage_conversion_multiple_rows(self):
        # Tests from/to_mlab_linkage on linkage array with multiple rows.
        Zm = np.asarray([[3, 6, 138], [4, 5, 219],
                         [1, 8, 255], [2, 9, 268], [7, 10, 295]])
        Z = np.array([[2., 5., 138., 2.],
                      [3., 4., 219., 2.],
                      [0., 7., 255., 3.],
                      [1., 8., 268., 4.],
                      [6., 9., 295., 6.]],
                      dtype=np.double)
        assert_equal(from_mlab_linkage(Zm), Z)
        assert_equal(to_mlab_linkage(Z), Zm)


class TestFcluster(object):
    def test_fclusterdata(self):
        for t in hierarchy_test_data.fcluster_inconsistent:
            yield self.check_fclusterdata, t, 'inconsistent'
        for t in hierarchy_test_data.fcluster_distance:
            yield self.check_fclusterdata, t, 'distance'
        for t in hierarchy_test_data.fcluster_maxclust:
            yield self.check_fclusterdata, t, 'maxclust'

    def check_fclusterdata(self, t, criterion):
        # Tests fclusterdata(X, criterion=criterion, t=t) on a random 3-cluster data set.
        expectedT = getattr(hierarchy_test_data, 'fcluster_' + criterion)[t]
        X = hierarchy_test_data.Q_X
        T = fclusterdata(X, criterion=criterion, t=t)
        assert_(is_isomorphic(T, expectedT))

    def test_fcluster(self):
        for t in hierarchy_test_data.fcluster_inconsistent:
            yield self.check_fcluster, t, 'inconsistent'
        for t in hierarchy_test_data.fcluster_distance:
            yield self.check_fcluster, t, 'distance'
        for t in hierarchy_test_data.fcluster_maxclust:
            yield self.check_fcluster, t, 'maxclust'

    def check_fcluster(self, t, criterion):
        # Tests fcluster(Z, criterion=criterion, t=t) on a random 3-cluster data set.
        expectedT = getattr(hierarchy_test_data, 'fcluster_' + criterion)[t]
        Z = single(hierarchy_test_data.Q_X)
        T = fcluster(Z, criterion=criterion, t=t)
        assert_(is_isomorphic(T, expectedT))

    def test_fcluster_monocrit(self):
        for t in hierarchy_test_data.fcluster_distance:
            yield self.check_fcluster_monocrit, t
        for t in hierarchy_test_data.fcluster_maxclust:
            yield self.check_fcluster_maxclust_monocrit, t

    def check_fcluster_monocrit(self, t):
        expectedT = hierarchy_test_data.fcluster_distance[t]
        Z = single(hierarchy_test_data.Q_X)
        T = fcluster(Z, t, criterion='monocrit', monocrit=maxdists(Z))
        assert_(is_isomorphic(T, expectedT))

    def check_fcluster_maxclust_monocrit(self, t):
        expectedT = hierarchy_test_data.fcluster_maxclust[t]
        Z = single(hierarchy_test_data.Q_X)
        T = fcluster(Z, t, criterion='maxclust_monocrit', monocrit=maxdists(Z))
        assert_(is_isomorphic(T, expectedT))


class TestLeaders(object):
    def test_leaders_single(self):
        # Tests leaders using a flat clustering generated by single linkage.
        X = hierarchy_test_data.Q_X
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        Y = pdist(X)
        Z = linkage(Y)
        T = fcluster(Z, criterion='maxclust', t=3)
        Lright = (np.array([53, 55, 56]), np.array([2, 3, 1]))
        L = leaders(Z, T)
<<<<<<< HEAD
        #print L, Lright, T
        self.failUnless((L[0] == Lright[0]).all() and (L[1] == Lright[1]).all())

class TestIsIsomorphic(TestCase):

    def test_is_isomorphic_1(self):
        "Tests is_isomorphic on test case #1 (one flat cluster, different labellings)"
        a = [1, 1, 1]
        b = [2, 2, 2]
        self.failUnless(is_isomorphic(a, b) == True)
        self.failUnless(is_isomorphic(b, a) == True)

    def test_is_isomorphic_2(self):
        "Tests is_isomorphic on test case #2 (two flat clusters, different labelings)"
        a = [1, 7, 1]
        b = [2, 3, 2]
        self.failUnless(is_isomorphic(a, b) == True)
        self.failUnless(is_isomorphic(b, a) == True)

    def test_is_isomorphic_3(self):
        "Tests is_isomorphic on test case #3 (no flat clusters)"
        a = []
        b = []
        self.failUnless(is_isomorphic(a, b) == True)

    def test_is_isomorphic_4A(self):
        "Tests is_isomorphic on test case #4A (3 flat clusters, different labelings, isomorphic)"
        a = [1, 2, 3]
        b = [1, 3, 2]
        self.failUnless(is_isomorphic(a, b) == True)
        self.failUnless(is_isomorphic(b, a) == True)

    def test_is_isomorphic_4B(self):
        "Tests is_isomorphic on test case #4B (3 flat clusters, different labelings, nonisomorphic)"
        a = [1, 2, 3, 3]
        b = [1, 3, 2, 3]
        self.failUnless(is_isomorphic(a, b) == False)
        self.failUnless(is_isomorphic(b, a) == False)

    def test_is_isomorphic_4C(self):
        "Tests is_isomorphic on test case #4C (3 flat clusters, different labelings, isomorphic)"
        a = [7, 2, 3]
        b = [6, 3, 2]
        self.failUnless(is_isomorphic(a, b) == True)
        self.failUnless(is_isomorphic(b, a) == True)

    def test_is_isomorphic_5A(self):
        "Tests is_isomorphic on test case #5A (1000 observations, 2 random clusters, random permutation of the labeling). Run 3 times."
        for k in xrange(0, 3):
            self.help_is_isomorphic_randperm(1000, 2)

    def test_is_isomorphic_5B(self):
        "Tests is_isomorphic on test case #5B (1000 observations, 3 random clusters, random permutation of the labeling). Run 3 times."
        for k in xrange(0, 3):
            self.help_is_isomorphic_randperm(1000, 3)

    def test_is_isomorphic_5C(self):
        "Tests is_isomorphic on test case #5C (1000 observations, 5 random clusters, random permutation of the labeling). Run 3 times."
        for k in xrange(0, 3):
            self.help_is_isomorphic_randperm(1000, 5)

    def test_is_isomorphic_6A(self):
        "Tests is_isomorphic on test case #5A (1000 observations, 2 random clusters, random permutation of the labeling, slightly nonisomorphic.) Run 3 times."
        for k in xrange(0, 3):
            self.help_is_isomorphic_randperm(1000, 2, True, 5)

    def test_is_isomorphic_6B(self):
        "Tests is_isomorphic on test case #5B (1000 observations, 3 random clusters, random permutation of the labeling, slightly nonisomorphic.) Run 3 times."
        for k in xrange(0, 3):
            self.help_is_isomorphic_randperm(1000, 3, True, 5)

    def test_is_isomorphic_6C(self):
        "Tests is_isomorphic on test case #5C (1000 observations, 5 random clusters, random permutation of the labeling, slightly non-isomorphic.) Run 3 times."
        for k in xrange(0, 3):
            self.help_is_isomorphic_randperm(1000, 5, True, 5)

    def help_is_isomorphic_randperm(self, nobs, nclusters, noniso=False, nerrors=0):
        a = np.int_(np.random.rand(nobs) * nclusters)
        b = np.zeros(a.size, dtype=np.int_)
        q = {}
        P = np.random.permutation(nclusters)
        for i in xrange(0, a.shape[0]):
            b[i] = P[a[i]]
        if noniso:
            Q = np.random.permutation(nobs)
            b[Q[0:nerrors]] += 1
            b[Q[0:nerrors]] %= nclusters
        self.failUnless(is_isomorphic(a, b) == (not noniso))
        self.failUnless(is_isomorphic(b, a) == (not noniso))

class TestIsValidLinkage(TestCase):

    def test_is_valid_linkage_int_type(self):
        "Tests is_valid_linkage(Z) with integer type."
        Z = np.asarray([[0,   1, 3.0, 2],
                        [3,   2, 4.0, 3]], dtype=np.int)
        self.failUnless(is_valid_linkage(Z) == False)
        self.failUnlessRaises(TypeError, is_valid_linkage, Z, throw=True)

    def test_is_valid_linkage_5_columns(self):
        "Tests is_valid_linkage(Z) with 5 columns."
        Z = np.asarray([[0,   1, 3.0, 2, 5],
                        [3,   2, 4.0, 3, 3]], dtype=np.double)
        self.failUnless(is_valid_linkage(Z) == False)
        self.failUnlessRaises(ValueError, is_valid_linkage, Z, throw=True)

    def test_is_valid_linkage_3_columns(self):
        "Tests is_valid_linkage(Z) with 3 columns."
        Z = np.asarray([[0,   1, 3.0],
                        [3,   2, 4.0]], dtype=np.double)
        self.failUnless(is_valid_linkage(Z) == False)
        self.failUnlessRaises(ValueError, is_valid_linkage, Z, throw=True)

    def test_is_valid_linkage_empty(self):
        "Tests is_valid_linkage(Z) with empty linkage."
        Z = np.zeros((0, 4), dtype=np.double)
        self.failUnless(is_valid_linkage(Z) == False)
        self.failUnlessRaises(ValueError, is_valid_linkage, Z, throw=True)

    def test_is_valid_linkage_1x4(self):
        "Tests is_valid_linkage(Z) on linkage over 2 observations."
        Z = np.asarray([[0,   1, 3.0, 2]], dtype=np.double)
        self.failUnless(is_valid_linkage(Z) == True)

    def test_is_valid_linkage_2x4(self):
        "Tests is_valid_linkage(Z) on linkage over 3 observations."
        Z = np.asarray([[0,   1, 3.0, 2],
                        [3,   2, 4.0, 3]], dtype=np.double)
        self.failUnless(is_valid_linkage(Z) == True)

    def test_is_valid_linkage_4_and_up(self):
        "Tests is_valid_linkage(Z) on linkage on observation sets between sizes 4 and 15 (step size 3)."
        for i in xrange(4, 15, 3):
            y = np.random.rand(i*(i-1)/2)
            Z = linkage(y)
            self.failUnless(is_valid_linkage(Z) == True)

    def test_is_valid_linkage_4_and_up_neg_index_left(self):
        "Tests is_valid_linkage(Z) on linkage on observation sets between sizes 4 and 15 (step size 3) with negative indices (left)."
        for i in xrange(4, 15, 3):
            y = np.random.rand(i*(i-1)/2)
            Z = linkage(y)
            Z[int(i/2),0] = -2
            self.failUnless(is_valid_linkage(Z) == False)
            self.failUnlessRaises(ValueError, is_valid_linkage, Z, throw=True)

    def test_is_valid_linkage_4_and_up_neg_index_right(self):
        "Tests is_valid_linkage(Z) on linkage on observation sets between sizes 4 and 15 (step size 3) with negative indices (right)."
        for i in xrange(4, 15, 3):
            y = np.random.rand(i*(i-1)/2)
            Z = linkage(y)
            Z[int(i/2),1] = -2
            self.failUnless(is_valid_linkage(Z) == False)
            self.failUnlessRaises(ValueError, is_valid_linkage, Z, throw=True)

    def test_is_valid_linkage_4_and_up_neg_dist(self):
        "Tests is_valid_linkage(Z) on linkage on observation sets between sizes 4 and 15 (step size 3) with negative distances."
        for i in xrange(4, 15, 3):
            y = np.random.rand(i*(i-1)/2)
            Z = linkage(y)
            Z[int(i/2),2] = -0.5
            self.failUnless(is_valid_linkage(Z) == False)
            self.failUnlessRaises(ValueError, is_valid_linkage, Z, throw=True)

    def test_is_valid_linkage_4_and_up_neg_counts(self):
        "Tests is_valid_linkage(Z) on linkage on observation sets between sizes 4 and 15 (step size 3) with negative counts."
        for i in xrange(4, 15, 3):
            y = np.random.rand(i*(i-1)/2)
            Z = linkage(y)
            Z[int(i/2),3] = -2
            self.failUnless(is_valid_linkage(Z) == False)
            self.failUnlessRaises(ValueError, is_valid_linkage, Z, throw=True)

class TestIsValidInconsistent(TestCase):

    def test_is_valid_im_int_type(self):
        "Tests is_valid_im(R) with integer type."
        R = np.asarray([[0,   1, 3.0, 2],
                        [3,   2, 4.0, 3]], dtype=np.int)
        self.failUnless(is_valid_im(R) == False)
        self.failUnlessRaises(TypeError, is_valid_im, R, throw=True)

    def test_is_valid_im_5_columns(self):
        "Tests is_valid_im(R) with 5 columns."
        R = np.asarray([[0,   1, 3.0, 2, 5],
                        [3,   2, 4.0, 3, 3]], dtype=np.double)
        self.failUnless(is_valid_im(R) == False)
        self.failUnlessRaises(ValueError, is_valid_im, R, throw=True)

    def test_is_valid_im_3_columns(self):
        "Tests is_valid_im(R) with 3 columns."
        R = np.asarray([[0,   1, 3.0],
                        [3,   2, 4.0]], dtype=np.double)
        self.failUnless(is_valid_im(R) == False)
        self.failUnlessRaises(ValueError, is_valid_im, R, throw=True)

    def test_is_valid_im_empty(self):
        "Tests is_valid_im(R) with empty inconsistency matrix."
        R = np.zeros((0, 4), dtype=np.double)
        self.failUnless(is_valid_im(R) == False)
        self.failUnlessRaises(ValueError, is_valid_im, R, throw=True)

    def test_is_valid_im_1x4(self):
        "Tests is_valid_im(R) on im over 2 observations."
        R = np.asarray([[0,   1, 3.0, 2]], dtype=np.double)
        self.failUnless(is_valid_im(R) == True)

    def test_is_valid_im_2x4(self):
        "Tests is_valid_im(R) on im over 3 observations."
        R = np.asarray([[0,   1, 3.0, 2],
                        [3,   2, 4.0, 3]], dtype=np.double)
        self.failUnless(is_valid_im(R) == True)

    def test_is_valid_im_4_and_up(self):
        "Tests is_valid_im(R) on im on observation sets between sizes 4 and 15 (step size 3)."
        for i in xrange(4, 15, 3):
            y = np.random.rand(i*(i-1)/2)
            Z = linkage(y)
            R = inconsistent(Z)
            self.failUnless(is_valid_im(R) == True)

    def test_is_valid_im_4_and_up_neg_index_left(self):
        "Tests is_valid_im(R) on im on observation sets between sizes 4 and 15 (step size 3) with negative link height means."
        for i in xrange(4, 15, 3):
            y = np.random.rand(i*(i-1)/2)
            Z = linkage(y)
            R = inconsistent(Z)
            R[int(i/2),0] = -2.0
            self.failUnless(is_valid_im(R) == False)
            self.failUnlessRaises(ValueError, is_valid_im, R, throw=True)

    def test_is_valid_im_4_and_up_neg_index_right(self):
        "Tests is_valid_im(R) on im on observation sets between sizes 4 and 15 (step size 3) with negative link height standard deviations."
        for i in xrange(4, 15, 3):
            y = np.random.rand(i*(i-1)/2)
            Z = linkage(y)
            R = inconsistent(Z)
            R[int(i/2),1] = -2.0
            self.failUnless(is_valid_im(R) == False)
            self.failUnlessRaises(ValueError, is_valid_im, R, throw=True)

    def test_is_valid_im_4_and_up_neg_dist(self):
        "Tests is_valid_im(R) on im on observation sets between sizes 4 and 15 (step size 3) with negative link counts."
        for i in xrange(4, 15, 3):
            y = np.random.rand(i*(i-1)/2)
            Z = linkage(y)
            R = inconsistent(Z)
            R[int(i/2),2] = -0.5
            self.failUnless(is_valid_im(R) == False)
            self.failUnlessRaises(ValueError, is_valid_im, R, throw=True)

class TestNumObsLinkage(TestCase):

    def test_num_obs_linkage_empty(self):
        "Tests num_obs_linkage(Z) with empty linkage."
        Z = np.zeros((0, 4), dtype=np.double)
        self.failUnlessRaises(ValueError, num_obs_linkage, Z)

    def test_num_obs_linkage_1x4(self):
        "Tests num_obs_linkage(Z) on linkage over 2 observations."
        Z = np.asarray([[0,   1, 3.0, 2]], dtype=np.double)
        self.failUnless(num_obs_linkage(Z) == 2)

    def test_num_obs_linkage_2x4(self):
        "Tests num_obs_linkage(Z) on linkage over 3 observations."
        Z = np.asarray([[0,   1, 3.0, 2],
                        [3,   2, 4.0, 3]], dtype=np.double)
        self.failUnless(num_obs_linkage(Z) == 3)

    def test_num_obs_linkage_4_and_up(self):
        "Tests num_obs_linkage(Z) on linkage on observation sets between sizes 4 and 15 (step size 3)."
        for i in xrange(4, 15, 3):
            y = np.random.rand(i*(i-1)/2)
            Z = linkage(y)
            self.failUnless(num_obs_linkage(Z) == i)

class TestLeavesList(TestCase):

    def test_leaves_list_1x4(self):
        "Tests leaves_list(Z) on a 1x4 linkage."
        Z = np.asarray([[0,   1, 3.0, 2]], dtype=np.double)
        node = to_tree(Z)
        self.failUnless((leaves_list(Z) == [0, 1]).all())

    def test_leaves_list_2x4(self):
        "Tests leaves_list(Z) on a 2x4 linkage."
        Z = np.asarray([[0,   1, 3.0, 2],
                        [3,   2, 4.0, 3]], dtype=np.double)
        node = to_tree(Z)
        self.failUnless((leaves_list(Z) == [0, 1, 2]).all())

    def test_leaves_list_iris_single(self):
        "Tests leaves_list(Z) on the Iris data set using single linkage."
        X = eo['iris']
        Y = pdist(X)
        Z = linkage(X, 'single')
        node = to_tree(Z)
        self.failUnless((node.pre_order() == leaves_list(Z)).all())

    def test_leaves_list_iris_complete(self):
        "Tests leaves_list(Z) on the Iris data set using complete linkage."
        X = eo['iris']
        Y = pdist(X)
        Z = linkage(X, 'complete')
        node = to_tree(Z)
        self.failUnless((node.pre_order() == leaves_list(Z)).all())

    def test_leaves_list_iris_centroid(self):
        "Tests leaves_list(Z) on the Iris data set using centroid linkage."
        X = eo['iris']
        Y = pdist(X)
        Z = linkage(X, 'centroid')
        node = to_tree(Z)
        self.failUnless((node.pre_order() == leaves_list(Z)).all())

    def test_leaves_list_iris_median(self):
        "Tests leaves_list(Z) on the Iris data set using median linkage."
        X = eo['iris']
        Y = pdist(X)
        Z = linkage(X, 'median')
        node = to_tree(Z)
        self.failUnless((node.pre_order() == leaves_list(Z)).all())

    def test_leaves_list_iris_ward(self):
        "Tests leaves_list(Z) on the Iris data set using ward linkage."
        X = eo['iris']
        Y = pdist(X)
        Z = linkage(X, 'ward')
        node = to_tree(Z)
        self.failUnless((node.pre_order() == leaves_list(Z)).all())

    def test_leaves_list_iris_average(self):
        "Tests leaves_list(Z) on the Iris data set using average linkage."
        X = eo['iris']
        Y = pdist(X)
        Z = linkage(X, 'average')
        node = to_tree(Z)
        self.failUnless((node.pre_order() == leaves_list(Z)).all())

class TestCorrespond(TestCase):

    def test_correspond_empty(self):
        "Tests correspond(Z, y) with empty linkage and condensed distance matrix."
        y = np.zeros((0,))
        Z = np.zeros((0,4))
        self.failUnlessRaises(ValueError, correspond, Z, y)

    def test_correspond_2_and_up(self):
        "Tests correspond(Z, y) on linkage and CDMs over observation sets of different sizes."
        for i in xrange(2, 4):
            y = np.random.rand(i*(i-1)/2)
            Z = linkage(y)
            self.failUnless(correspond(Z, y))
        for i in xrange(4, 15, 3):
            y = np.random.rand(i*(i-1)/2)
            Z = linkage(y)
            self.failUnless(correspond(Z, y))

    def test_correspond_4_and_up(self):
        "Tests correspond(Z, y) on linkage and CDMs over observation sets of different sizes. Correspondance should be false."
        for (i, j) in zip(range(2, 4), range(3, 5)) + zip(range(3, 5), range(2, 4)):
            y = np.random.rand(i*(i-1)/2)
            y2 = np.random.rand(j*(j-1)/2)
            Z = linkage(y)
            Z2 = linkage(y2)
            self.failUnless(correspond(Z, y2) == False)
            self.failUnless(correspond(Z2, y) == False)

    def test_correspond_4_and_up_2(self):
        "Tests correspond(Z, y) on linkage and CDMs over observation sets of different sizes. Correspondance should be false."
        for (i, j) in zip(range(2, 7), range(16, 21)) + zip(range(2, 7), range(16, 21)):
            y = np.random.rand(i*(i-1)/2)
            y2 = np.random.rand(j*(j-1)/2)
            Z = linkage(y)
            Z2 = linkage(y2)
            self.failUnless(correspond(Z, y2) == False)
            self.failUnless(correspond(Z2, y) == False)

    def test_num_obs_linkage_multi_matrix(self):
        "Tests num_obs_linkage with observation matrices of multiple sizes."
=======
        assert_equal(L, Lright)


class TestIsIsomorphic(object):
    def test_is_isomorphic_1(self):
        # Tests is_isomorphic on test case #1 (one flat cluster, different labellings)
        a = [1, 1, 1]
        b = [2, 2, 2]
        assert_(is_isomorphic(a, b))
        assert_(is_isomorphic(b, a))

    def test_is_isomorphic_2(self):
        # Tests is_isomorphic on test case #2 (two flat clusters, different labelings)
        a = [1, 7, 1]
        b = [2, 3, 2]
        assert_(is_isomorphic(a, b))
        assert_(is_isomorphic(b, a))

    def test_is_isomorphic_3(self):
        # Tests is_isomorphic on test case #3 (no flat clusters)
        a = []
        b = []
        assert_(is_isomorphic(a, b))

    def test_is_isomorphic_4A(self):
        # Tests is_isomorphic on test case #4A (3 flat clusters, different labelings, isomorphic)
        a = [1, 2, 3]
        b = [1, 3, 2]
        assert_(is_isomorphic(a, b))
        assert_(is_isomorphic(b, a))

    def test_is_isomorphic_4B(self):
        # Tests is_isomorphic on test case #4B (3 flat clusters, different labelings, nonisomorphic)
        a = [1, 2, 3, 3]
        b = [1, 3, 2, 3]
        assert_(is_isomorphic(a, b) == False)
        assert_(is_isomorphic(b, a) == False)

    def test_is_isomorphic_4C(self):
        # Tests is_isomorphic on test case #4C (3 flat clusters, different labelings, isomorphic)
        a = [7, 2, 3]
        b = [6, 3, 2]
        assert_(is_isomorphic(a, b))
        assert_(is_isomorphic(b, a))

    def test_is_isomorphic_5(self):
        # Tests is_isomorphic on test case #5 (1000 observations, 2/3/5 random
        # clusters, random permutation of the labeling).
        for nc in [2, 3, 5]:
            yield self.help_is_isomorphic_randperm, 1000, nc

    def test_is_isomorphic_6(self):
        # Tests is_isomorphic on test case #5A (1000 observations, 2/3/5 random
        # clusters, random permutation of the labeling, slightly
        # nonisomorphic.)
        for nc in [2, 3, 5]:
            yield self.help_is_isomorphic_randperm, 1000, nc, True, 5

    def help_is_isomorphic_randperm(self, nobs, nclusters, noniso=False, nerrors=0):
        for k in range(3):
            a = np.int_(np.random.rand(nobs) * nclusters)
            b = np.zeros(a.size, dtype=np.int_)
            P = np.random.permutation(nclusters)
            for i in xrange(0, a.shape[0]):
                b[i] = P[a[i]]
            if noniso:
                Q = np.random.permutation(nobs)
                b[Q[0:nerrors]] += 1
                b[Q[0:nerrors]] %= nclusters
            assert_(is_isomorphic(a, b) == (not noniso))
            assert_(is_isomorphic(b, a) == (not noniso))


class TestIsValidLinkage(object):
    def test_is_valid_linkage_various_size(self):
        for nrow, ncol, valid in [(2, 5, False), (2, 3, False),
                                  (1, 4, True), (2, 4, True)]:
            yield self.check_is_valid_linkage_various_size, nrow, ncol, valid

    def check_is_valid_linkage_various_size(self, nrow, ncol, valid):
        # Tests is_valid_linkage(Z) with linkage matrics of various sizes
        Z = np.asarray([[0, 1, 3.0, 2, 5],
                        [3, 2, 4.0, 3, 3]], dtype=np.double)
        Z = Z[:nrow, :ncol]
        assert_(is_valid_linkage(Z) == valid)
        if not valid:
            assert_raises(ValueError, is_valid_linkage, Z, throw=True)

    def test_is_valid_linkage_int_type(self):
        # Tests is_valid_linkage(Z) with integer type.
        Z = np.asarray([[0, 1, 3.0, 2],
                        [3, 2, 4.0, 3]], dtype=int)
        assert_(is_valid_linkage(Z) == False)
        assert_raises(TypeError, is_valid_linkage, Z, throw=True)

    def test_is_valid_linkage_empty(self):
        # Tests is_valid_linkage(Z) with empty linkage.
        Z = np.zeros((0, 4), dtype=np.double)
        assert_(is_valid_linkage(Z) == False)
        assert_raises(ValueError, is_valid_linkage, Z, throw=True)

    def test_is_valid_linkage_4_and_up(self):
        # Tests is_valid_linkage(Z) on linkage on observation sets between
        # sizes 4 and 15 (step size 3).
        for i in xrange(4, 15, 3):
            y = np.random.rand(i*(i-1)//2)
            Z = linkage(y)
            assert_(is_valid_linkage(Z) == True)

    def test_is_valid_linkage_4_and_up_neg_index_left(self):
        # Tests is_valid_linkage(Z) on linkage on observation sets between
        # sizes 4 and 15 (step size 3) with negative indices (left).
        for i in xrange(4, 15, 3):
            y = np.random.rand(i*(i-1)//2)
            Z = linkage(y)
            Z[i//2,0] = -2
            assert_(is_valid_linkage(Z) == False)
            assert_raises(ValueError, is_valid_linkage, Z, throw=True)

    def test_is_valid_linkage_4_and_up_neg_index_right(self):
        # Tests is_valid_linkage(Z) on linkage on observation sets between
        # sizes 4 and 15 (step size 3) with negative indices (right).
        for i in xrange(4, 15, 3):
            y = np.random.rand(i*(i-1)//2)
            Z = linkage(y)
            Z[i//2,1] = -2
            assert_(is_valid_linkage(Z) == False)
            assert_raises(ValueError, is_valid_linkage, Z, throw=True)

    def test_is_valid_linkage_4_and_up_neg_dist(self):
        # Tests is_valid_linkage(Z) on linkage on observation sets between
        # sizes 4 and 15 (step size 3) with negative distances.
        for i in xrange(4, 15, 3):
            y = np.random.rand(i*(i-1)//2)
            Z = linkage(y)
            Z[i//2,2] = -0.5
            assert_(is_valid_linkage(Z) == False)
            assert_raises(ValueError, is_valid_linkage, Z, throw=True)

    def test_is_valid_linkage_4_and_up_neg_counts(self):
        # Tests is_valid_linkage(Z) on linkage on observation sets between
        # sizes 4 and 15 (step size 3) with negative counts.
        for i in xrange(4, 15, 3):
            y = np.random.rand(i*(i-1)//2)
            Z = linkage(y)
            Z[i//2,3] = -2
            assert_(is_valid_linkage(Z) == False)
            assert_raises(ValueError, is_valid_linkage, Z, throw=True)


class TestIsValidInconsistent(object):
    def test_is_valid_im_int_type(self):
        # Tests is_valid_im(R) with integer type.
        R = np.asarray([[0, 1, 3.0, 2],
                        [3, 2, 4.0, 3]], dtype=int)
        assert_(is_valid_im(R) == False)
        assert_raises(TypeError, is_valid_im, R, throw=True)

    def test_is_valid_im_various_size(self):
        for nrow, ncol, valid in [(2, 5, False), (2, 3, False),
                                  (1, 4, True), (2, 4, True)]:
            yield self.check_is_valid_im_various_size, nrow, ncol, valid

    def check_is_valid_im_various_size(self, nrow, ncol, valid):
        # Tests is_valid_im(R) with linkage matrics of various sizes
        R = np.asarray([[0, 1, 3.0, 2, 5],
                        [3, 2, 4.0, 3, 3]], dtype=np.double)
        R = R[:nrow, :ncol]
        assert_(is_valid_im(R) == valid)
        if not valid:
            assert_raises(ValueError, is_valid_im, R, throw=True)

    def test_is_valid_im_empty(self):
        # Tests is_valid_im(R) with empty inconsistency matrix.
        R = np.zeros((0, 4), dtype=np.double)
        assert_(is_valid_im(R) == False)
        assert_raises(ValueError, is_valid_im, R, throw=True)

    def test_is_valid_im_4_and_up(self):
        # Tests is_valid_im(R) on im on observation sets between sizes 4 and 15
        # (step size 3).
        for i in xrange(4, 15, 3):
            y = np.random.rand(i*(i-1)//2)
            Z = linkage(y)
            R = inconsistent(Z)
            assert_(is_valid_im(R) == True)

    def test_is_valid_im_4_and_up_neg_index_left(self):
        # Tests is_valid_im(R) on im on observation sets between sizes 4 and 15
        # (step size 3) with negative link height means.
        for i in xrange(4, 15, 3):
            y = np.random.rand(i*(i-1)//2)
            Z = linkage(y)
            R = inconsistent(Z)
            R[i//2,0] = -2.0
            assert_(is_valid_im(R) == False)
            assert_raises(ValueError, is_valid_im, R, throw=True)

    def test_is_valid_im_4_and_up_neg_index_right(self):
        # Tests is_valid_im(R) on im on observation sets between sizes 4 and 15
        # (step size 3) with negative link height standard deviations.
        for i in xrange(4, 15, 3):
            y = np.random.rand(i*(i-1)//2)
            Z = linkage(y)
            R = inconsistent(Z)
            R[i//2,1] = -2.0
            assert_(is_valid_im(R) == False)
            assert_raises(ValueError, is_valid_im, R, throw=True)

    def test_is_valid_im_4_and_up_neg_dist(self):
        # Tests is_valid_im(R) on im on observation sets between sizes 4 and 15
        # (step size 3) with negative link counts.
        for i in xrange(4, 15, 3):
            y = np.random.rand(i*(i-1)//2)
            Z = linkage(y)
            R = inconsistent(Z)
            R[i//2,2] = -0.5
            assert_(is_valid_im(R) == False)
            assert_raises(ValueError, is_valid_im, R, throw=True)


class TestNumObsLinkage(TestCase):
    def test_num_obs_linkage_empty(self):
        # Tests num_obs_linkage(Z) with empty linkage.
        Z = np.zeros((0, 4), dtype=np.double)
        assert_raises(ValueError, num_obs_linkage, Z)

    def test_num_obs_linkage_1x4(self):
        # Tests num_obs_linkage(Z) on linkage over 2 observations.
        Z = np.asarray([[0, 1, 3.0, 2]], dtype=np.double)
        assert_equal(num_obs_linkage(Z), 2)

    def test_num_obs_linkage_2x4(self):
        # Tests num_obs_linkage(Z) on linkage over 3 observations.
        Z = np.asarray([[0, 1, 3.0, 2],
                        [3, 2, 4.0, 3]], dtype=np.double)
        assert_equal(num_obs_linkage(Z), 3)

    def test_num_obs_linkage_4_and_up(self):
        # Tests num_obs_linkage(Z) on linkage on observation sets between sizes
        # 4 and 15 (step size 3).
        for i in xrange(4, 15, 3):
            y = np.random.rand(i*(i-1)//2)
            Z = linkage(y)
            assert_equal(num_obs_linkage(Z), i)


class TestLeavesList(object):
    def test_leaves_list_1x4(self):
        # Tests leaves_list(Z) on a 1x4 linkage.
        Z = np.asarray([[0, 1, 3.0, 2]], dtype=np.double)
        to_tree(Z)
        assert_equal(leaves_list(Z), [0, 1])

    def test_leaves_list_2x4(self):
        # Tests leaves_list(Z) on a 2x4 linkage.
        Z = np.asarray([[0, 1, 3.0, 2],
                        [3, 2, 4.0, 3]], dtype=np.double)
        to_tree(Z)
        assert_equal(leaves_list(Z), [0, 1, 2])

    def test_leaves_list_Q(self):
        for method in ['single', 'complete', 'average', 'weighted', 'centroid',
                       'median', 'ward']:
            yield self.check_leaves_list_Q, method

    def check_leaves_list_Q(self, method):
        # Tests leaves_list(Z) on the Q data set
        X = hierarchy_test_data.Q_X
        Z = linkage(X, method)
        node = to_tree(Z)
        assert_equal(node.pre_order(), leaves_list(Z))

    def test_Q_subtree_pre_order(self):
        # Tests that pre_order() works when called on sub-trees.
        X = hierarchy_test_data.Q_X
        Z = linkage(X, 'single')
        node = to_tree(Z)
        assert_equal(node.pre_order(), (node.get_left().pre_order()
                                        + node.get_right().pre_order()))


class TestCorrespond(TestCase):
    def test_correspond_empty(self):
        # Tests correspond(Z, y) with empty linkage and condensed distance matrix.
        y = np.zeros((0,))
        Z = np.zeros((0,4))
        assert_raises(ValueError, correspond, Z, y)

    def test_correspond_2_and_up(self):
        # Tests correspond(Z, y) on linkage and CDMs over observation sets of
        # different sizes.
        for i in xrange(2, 4):
            y = np.random.rand(i*(i-1)//2)
            Z = linkage(y)
            assert_(correspond(Z, y))
        for i in xrange(4, 15, 3):
            y = np.random.rand(i*(i-1)//2)
            Z = linkage(y)
            assert_(correspond(Z, y))

    def test_correspond_4_and_up(self):
        # Tests correspond(Z, y) on linkage and CDMs over observation sets of
        # different sizes. Correspondance should be false.
        for (i, j) in (list(zip(list(range(2, 4)), list(range(3, 5)))) +
                       list(zip(list(range(3, 5)), list(range(2, 4))))):
            y = np.random.rand(i*(i-1)//2)
            y2 = np.random.rand(j*(j-1)//2)
            Z = linkage(y)
            Z2 = linkage(y2)
            assert_equal(correspond(Z, y2), False)
            assert_equal(correspond(Z2, y), False)

    def test_correspond_4_and_up_2(self):
        # Tests correspond(Z, y) on linkage and CDMs over observation sets of
        # different sizes. Correspondance should be false.
        for (i, j) in (list(zip(list(range(2, 7)), list(range(16, 21)))) +
                       list(zip(list(range(2, 7)), list(range(16, 21))))):
            y = np.random.rand(i*(i-1)//2)
            y2 = np.random.rand(j*(j-1)//2)
            Z = linkage(y)
            Z2 = linkage(y2)
            assert_equal(correspond(Z, y2), False)
            assert_equal(correspond(Z2, y), False)

    def test_num_obs_linkage_multi_matrix(self):
        # Tests num_obs_linkage with observation matrices of multiple sizes.
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        for n in xrange(2, 10):
            X = np.random.rand(n, 4)
            Y = pdist(X)
            Z = linkage(Y)
<<<<<<< HEAD
            #print Z
            #print A.shape, Y.shape, Yr.shape
            self.failUnless(num_obs_linkage(Z) == n)

class TestIsMonotonic(TestCase):

    def test_is_monotonic_empty(self):
        "Tests is_monotonic(Z) on an empty linkage."
        Z = np.zeros((0, 4))
        self.failUnlessRaises(ValueError, is_monotonic, Z)

    def test_is_monotonic_1x4(self):
        "Tests is_monotonic(Z) on 1x4 linkage. Expecting True."
        Z = np.asarray([[0, 1, 0.3, 2]], dtype=np.double);
        self.failUnless(is_monotonic(Z) == True)

    def test_is_monotonic_2x4_T(self):
        "Tests is_monotonic(Z) on 2x4 linkage. Expecting True."
        Z = np.asarray([[0, 1, 0.3, 2],
                        [2, 3, 0.4, 3]], dtype=np.double)
        self.failUnless(is_monotonic(Z) == True)

    def test_is_monotonic_2x4_F(self):
        "Tests is_monotonic(Z) on 2x4 linkage. Expecting False."
        Z = np.asarray([[0, 1, 0.4, 2],
                        [2, 3, 0.3, 3]], dtype=np.double)
        self.failUnless(is_monotonic(Z) == False)

    def test_is_monotonic_3x4_T(self):
        "Tests is_monotonic(Z) on 3x4 linkage. Expecting True."
        Z = np.asarray([[0, 1, 0.3, 2],
                        [2, 3, 0.4, 2],
                        [4, 5, 0.6, 4]], dtype=np.double)
        self.failUnless(is_monotonic(Z) == True)

    def test_is_monotonic_3x4_F1(self):
        "Tests is_monotonic(Z) on 3x4 linkage (case 1). Expecting False."
        Z = np.asarray([[0, 1, 0.3, 2],
                        [2, 3, 0.2, 2],
                        [4, 5, 0.6, 4]], dtype=np.double)
        self.failUnless(is_monotonic(Z) == False)

    def test_is_monotonic_3x4_F2(self):
        "Tests is_monotonic(Z) on 3x4 linkage (case 2). Expecting False."
        Z = np.asarray([[0, 1, 0.8, 2],
                        [2, 3, 0.4, 2],
                        [4, 5, 0.6, 4]], dtype=np.double)
        self.failUnless(is_monotonic(Z) == False)

    def test_is_monotonic_3x4_F3(self):
        "Tests is_monotonic(Z) on 3x4 linkage (case 3). Expecting False"
        Z = np.asarray([[0, 1, 0.3, 2],
                        [2, 3, 0.4, 2],
                        [4, 5, 0.2, 4]], dtype=np.double)
        self.failUnless(is_monotonic(Z) == False)

    def test_is_monotonic_tdist_linkage(self):
        "Tests is_monotonic(Z) on clustering generated by single linkage on tdist data set. Expecting True."
        Z = linkage(_ytdist, 'single')
        self.failUnless(is_monotonic(Z) == True)

    def test_is_monotonic_tdist_linkage(self):
        "Tests is_monotonic(Z) on clustering generated by single linkage on tdist data set. Perturbing. Expecting False."
        Z = linkage(_ytdist, 'single')
        Z[2,2]=0.0
        self.failUnless(is_monotonic(Z) == False)

    def test_is_monotonic_iris_linkage(self):
        "Tests is_monotonic(Z) on clustering generated by single linkage on Iris data set. Expecting True."
        X = eo['iris']
        Y = pdist(X)
        Z = linkage(X, 'single')
        self.failUnless(is_monotonic(Z) == True)

class TestMaxDists(TestCase):

    def test_maxdists_empty_linkage(self):
        "Tests maxdists(Z) on empty linkage. Expecting exception."
        Z = np.zeros((0, 4), dtype=np.double)
        self.failUnlessRaises(ValueError, maxdists, Z)

    def test_maxdists_one_cluster_linkage(self):
        "Tests maxdists(Z) on linkage with one cluster."
        Z = np.asarray([[0, 1, 0.3, 4]], dtype=np.double)
        MD = maxdists(Z)
        eps = 1e-15
        expectedMD = calculate_maximum_distances(Z)
        self.failUnless(within_tol(MD, expectedMD, eps))

    def test_maxdists_Q_linkage_single(self):
        "Tests maxdists(Z) on the Q data set using single linkage."
        X = eo['Q-X']
        Y = pdist(X)
        Z = linkage(X, 'single')
        MD = maxdists(Z)
        eps = 1e-15
        expectedMD = calculate_maximum_distances(Z)
        self.failUnless(within_tol(MD, expectedMD, eps))

    def test_maxdists_Q_linkage_complete(self):
        "Tests maxdists(Z) on the Q data set using complete linkage."
        X = eo['Q-X']
        Y = pdist(X)
        Z = linkage(X, 'complete')
        MD = maxdists(Z)
        eps = 1e-15
        expectedMD = calculate_maximum_distances(Z)
        self.failUnless(within_tol(MD, expectedMD, eps))

    def test_maxdists_Q_linkage_ward(self):
        "Tests maxdists(Z) on the Q data set using Ward linkage."
        X = eo['Q-X']
        Y = pdist(X)
        Z = linkage(X, 'ward')
        MD = maxdists(Z)
        eps = 1e-15
        expectedMD = calculate_maximum_distances(Z)
        self.failUnless(within_tol(MD, expectedMD, eps))

    def test_maxdists_Q_linkage_centroid(self):
        "Tests maxdists(Z) on the Q data set using centroid linkage."
        X = eo['Q-X']
        Y = pdist(X)
        Z = linkage(X, 'centroid')
        MD = maxdists(Z)
        eps = 1e-15
        expectedMD = calculate_maximum_distances(Z)
        self.failUnless(within_tol(MD, expectedMD, eps))

    def test_maxdists_Q_linkage_median(self):
        "Tests maxdists(Z) on the Q data set using median linkage."
        X = eo['Q-X']
        Y = pdist(X)
        Z = linkage(X, 'median')
        MD = maxdists(Z)
        eps = 1e-15
        expectedMD = calculate_maximum_distances(Z)
        self.failUnless(within_tol(MD, expectedMD, eps))

class TestMaxInconsts(TestCase):

    def test_maxinconsts_empty_linkage(self):
        "Tests maxinconsts(Z, R) on empty linkage. Expecting exception."
        Z = np.zeros((0, 4), dtype=np.double)
        R = np.zeros((0, 4), dtype=np.double)
        self.failUnlessRaises(ValueError, maxinconsts, Z, R)

    def test_maxinconsts_difrow_linkage(self):
        "Tests maxinconsts(Z, R) on linkage and inconsistency matrices with different numbers of clusters. Expecting exception."
        Z = np.asarray([[0, 1, 0.3, 4]], dtype=np.double)
        R = np.random.rand(2, 4)
        self.failUnlessRaises(ValueError, maxinconsts, Z, R)

    def test_maxinconsts_one_cluster_linkage(self):
        "Tests maxinconsts(Z, R) on linkage with one cluster."
        Z = np.asarray([[0, 1, 0.3, 4]], dtype=np.double)
        R = np.asarray([[0, 0, 0, 0.3]], dtype=np.double)
        MD = maxinconsts(Z, R)
        eps = 1e-15
        expectedMD = calculate_maximum_inconsistencies(Z, R)
        self.failUnless(within_tol(MD, expectedMD, eps))

    def test_maxinconsts_Q_linkage_single(self):
        "Tests maxinconsts(Z, R) on the Q data set using single linkage."
        X = eo['Q-X']
        Y = pdist(X)
        Z = linkage(X, 'single')
        R = inconsistent(Z)
        MD = maxinconsts(Z, R)
        eps = 1e-15
        expectedMD = calculate_maximum_inconsistencies(Z, R)
        self.failUnless(within_tol(MD, expectedMD, eps))

    def test_maxinconsts_Q_linkage_complete(self):
        "Tests maxinconsts(Z, R) on the Q data set using complete linkage."
        X = eo['Q-X']
        Y = pdist(X)
        Z = linkage(X, 'complete')
        R = inconsistent(Z)
        MD = maxinconsts(Z, R)
        eps = 1e-15
        expectedMD = calculate_maximum_inconsistencies(Z, R)
        self.failUnless(within_tol(MD, expectedMD, eps))

    def test_maxinconsts_Q_linkage_ward(self):
        "Tests maxinconsts(Z, R) on the Q data set using Ward linkage."
        X = eo['Q-X']
        Y = pdist(X)
        Z = linkage(X, 'ward')
        R = inconsistent(Z)
        MD = maxinconsts(Z, R)
        eps = 1e-15
        expectedMD = calculate_maximum_inconsistencies(Z, R)
        self.failUnless(within_tol(MD, expectedMD, eps))

    def test_maxinconsts_Q_linkage_centroid(self):
        "Tests maxinconsts(Z, R) on the Q data set using centroid linkage."
        X = eo['Q-X']
        Y = pdist(X)
        Z = linkage(X, 'centroid')
        R = inconsistent(Z)
        MD = maxinconsts(Z, R)
        eps = 1e-15
        expectedMD = calculate_maximum_inconsistencies(Z, R)
        self.failUnless(within_tol(MD, expectedMD, eps))

    def test_maxinconsts_Q_linkage_median(self):
        "Tests maxinconsts(Z, R) on the Q data set using median linkage."
        X = eo['Q-X']
        Y = pdist(X)
        Z = linkage(X, 'median')
        R = inconsistent(Z)
        MD = maxinconsts(Z, R)
        eps = 1e-15
        expectedMD = calculate_maximum_inconsistencies(Z, R)
        self.failUnless(within_tol(MD, expectedMD, eps))

class TestMaxRStat(TestCase):

    def test_maxRstat_float_index(self):
        "Tests maxRstat(Z, R, 3.3). Expecting exception."
        Z = np.asarray([[0, 1, 0.3, 4]], dtype=np.double)
        R = np.asarray([[0, 0, 0, 0.3]], dtype=np.double)
        self.failUnlessRaises(TypeError, maxRstat, Z, R, 3.3)

    def test_maxRstat_neg_index(self):
        "Tests maxRstat(Z, R, -1). Expecting exception."
        Z = np.asarray([[0, 1, 0.3, 4]], dtype=np.double)
        R = np.asarray([[0, 0, 0, 0.3]], dtype=np.double)
        self.failUnlessRaises(ValueError, maxRstat, Z, R, -1)

    def test_maxRstat_oob_pos_index(self):
        "Tests maxRstat(Z, R, 4). Expecting exception."
        Z = np.asarray([[0, 1, 0.3, 4]], dtype=np.double)
        R = np.asarray([[0, 0, 0, 0.3]], dtype=np.double)
        self.failUnlessRaises(ValueError, maxRstat, Z, R, 4)

    def test_maxRstat_0_empty_linkage(self):
        "Tests maxRstat(Z, R, 0) on empty linkage. Expecting exception."
        Z = np.zeros((0, 4), dtype=np.double)
        R = np.zeros((0, 4), dtype=np.double)
        self.failUnlessRaises(ValueError, maxRstat, Z, R, 0)

    def test_maxRstat_0_difrow_linkage(self):
        "Tests maxRstat(Z, R, 0) on linkage and inconsistency matrices with different numbers of clusters. Expecting exception."
        Z = np.asarray([[0, 1, 0.3, 4]], dtype=np.double)
        R = np.random.rand(2, 4)
        self.failUnlessRaises(ValueError, maxRstat, Z, R, 0)

    def test_maxRstat_0_one_cluster_linkage(self):
        "Tests maxRstat(Z, R, 0) on linkage with one cluster."
        Z = np.asarray([[0, 1, 0.3, 4]], dtype=np.double)
        R = np.asarray([[0, 0, 0, 0.3]], dtype=np.double)
        MD = maxRstat(Z, R, 0)
        eps = 1e-15
        expectedMD = calculate_maximum_inconsistencies(Z, R, 0)
        self.failUnless(within_tol(MD, expectedMD, eps))

    def test_maxRstat_0_Q_linkage_single(self):
        "Tests maxRstat(Z, R, 0) on the Q data set using single linkage."
        X = eo['Q-X']
        Y = pdist(X)
        Z = linkage(X, 'single')
        R = inconsistent(Z)
        MD = maxRstat(Z, R, 0)
        eps = 1e-15
        expectedMD = calculate_maximum_inconsistencies(Z, R, 0)
        self.failUnless(within_tol(MD, expectedMD, eps))

    def test_maxRstat_0_Q_linkage_complete(self):
        "Tests maxRstat(Z, R, 0) on the Q data set using complete linkage."
        X = eo['Q-X']
        Y = pdist(X)
        Z = linkage(X, 'complete')
        R = inconsistent(Z)
        MD = maxRstat(Z, R, 0)
        eps = 1e-15
        expectedMD = calculate_maximum_inconsistencies(Z, R, 0)
        self.failUnless(within_tol(MD, expectedMD, eps))

    def test_maxRstat_0_Q_linkage_ward(self):
        "Tests maxRstat(Z, R, 0) on the Q data set using Ward linkage."
        X = eo['Q-X']
        Y = pdist(X)
        Z = linkage(X, 'ward')
        R = inconsistent(Z)
        MD = maxRstat(Z, R, 0)
        eps = 1e-15
        expectedMD = calculate_maximum_inconsistencies(Z, R, 0)
        self.failUnless(within_tol(MD, expectedMD, eps))

    def test_maxRstat_0_Q_linkage_centroid(self):
        "Tests maxRstat(Z, R, 0) on the Q data set using centroid linkage."
        X = eo['Q-X']
        Y = pdist(X)
        Z = linkage(X, 'centroid')
        R = inconsistent(Z)
        MD = maxRstat(Z, R, 0)
        eps = 1e-15
        expectedMD = calculate_maximum_inconsistencies(Z, R, 0)
        self.failUnless(within_tol(MD, expectedMD, eps))

    def test_maxRstat_0_Q_linkage_median(self):
        "Tests maxRstat(Z, R, 0) on the Q data set using median linkage."
        X = eo['Q-X']
        Y = pdist(X)
        Z = linkage(X, 'median')
        R = inconsistent(Z)
        MD = maxRstat(Z, R, 0)
        eps = 1e-15
        expectedMD = calculate_maximum_inconsistencies(Z, R, 0)
        self.failUnless(within_tol(MD, expectedMD, eps))

    def test_maxRstat_1_empty_linkage(self):
        "Tests maxRstat(Z, R, 1) on empty linkage. Expecting exception."
        Z = np.zeros((0, 4), dtype=np.double)
        R = np.zeros((0, 4), dtype=np.double)
        self.failUnlessRaises(ValueError, maxRstat, Z, R, 0)

    def test_maxRstat_1_difrow_linkage(self):
        "Tests maxRstat(Z, R, 1) on linkage and inconsistency matrices with different numbers of clusters. Expecting exception."
        Z = np.asarray([[0, 1, 0.3, 4]], dtype=np.double)
        R = np.random.rand(2, 4)
        self.failUnlessRaises(ValueError, maxRstat, Z, R, 0)

    def test_maxRstat_1_one_cluster_linkage(self):
        "Tests maxRstat(Z, R, 1) on linkage with one cluster."
        Z = np.asarray([[0, 1, 0.3, 4]], dtype=np.double)
        R = np.asarray([[0, 0, 0, 0.3]], dtype=np.double)
        MD = maxRstat(Z, R, 1)
        eps = 1e-15
        expectedMD = calculate_maximum_inconsistencies(Z, R, 1)
        self.failUnless(within_tol(MD, expectedMD, eps))

    def test_maxRstat_1_Q_linkage_single(self):
        "Tests maxRstat(Z, R, 1) on the Q data set using single linkage."
        X = eo['Q-X']
        Y = pdist(X)
        Z = linkage(X, 'single')
        R = inconsistent(Z)
        MD = maxRstat(Z, R, 1)
        eps = 1e-15
        expectedMD = calculate_maximum_inconsistencies(Z, R, 1)
        self.failUnless(within_tol(MD, expectedMD, eps))

    def test_maxRstat_1_Q_linkage_complete(self):
        "Tests maxRstat(Z, R, 1) on the Q data set using complete linkage."
        X = eo['Q-X']
        Y = pdist(X)
        Z = linkage(X, 'complete')
        R = inconsistent(Z)
        MD = maxRstat(Z, R, 1)
        eps = 1e-15
        expectedMD = calculate_maximum_inconsistencies(Z, R, 1)
        self.failUnless(within_tol(MD, expectedMD, eps))

    def test_maxRstat_1_Q_linkage_ward(self):
        "Tests maxRstat(Z, R, 1) on the Q data set using Ward linkage."
        X = eo['Q-X']
        Y = pdist(X)
        Z = linkage(X, 'ward')
        R = inconsistent(Z)
        MD = maxRstat(Z, R, 1)
        eps = 1e-15
        expectedMD = calculate_maximum_inconsistencies(Z, R, 1)
        self.failUnless(within_tol(MD, expectedMD, eps))

    def test_maxRstat_1_Q_linkage_centroid(self):
        "Tests maxRstat(Z, R, 1) on the Q data set using centroid linkage."
        X = eo['Q-X']
        Y = pdist(X)
        Z = linkage(X, 'centroid')
        R = inconsistent(Z)
        MD = maxRstat(Z, R, 1)
        eps = 1e-15
        expectedMD = calculate_maximum_inconsistencies(Z, R, 1)
        self.failUnless(within_tol(MD, expectedMD, eps))

    def test_maxRstat_1_Q_linkage_median(self):
        "Tests maxRstat(Z, R, 1) on the Q data set using median linkage."
        X = eo['Q-X']
        Y = pdist(X)
        Z = linkage(X, 'median')
        R = inconsistent(Z)
        MD = maxRstat(Z, R, 1)
        eps = 1e-15
        expectedMD = calculate_maximum_inconsistencies(Z, R, 1)
        self.failUnless(within_tol(MD, expectedMD, eps))

    def test_maxRstat_2_empty_linkage(self):
        "Tests maxRstat(Z, R, 2) on empty linkage. Expecting exception."
        Z = np.zeros((0, 4), dtype=np.double)
        R = np.zeros((0, 4), dtype=np.double)
        self.failUnlessRaises(ValueError, maxRstat, Z, R, 2)

    def test_maxRstat_2_difrow_linkage(self):
        "Tests maxRstat(Z, R, 2) on linkage and inconsistency matrices with different numbers of clusters. Expecting exception."
        Z = np.asarray([[0, 1, 0.3, 4]], dtype=np.double)
        R = np.random.rand(2, 4)
        self.failUnlessRaises(ValueError, maxRstat, Z, R, 2)

    def test_maxRstat_2_one_cluster_linkage(self):
        "Tests maxRstat(Z, R, 2) on linkage with one cluster."
        Z = np.asarray([[0, 1, 0.3, 4]], dtype=np.double)
        R = np.asarray([[0, 0, 0, 0.3]], dtype=np.double)
        MD = maxRstat(Z, R, 2)
        eps = 1e-15
        expectedMD = calculate_maximum_inconsistencies(Z, R, 2)
        self.failUnless(within_tol(MD, expectedMD, eps))

    def test_maxRstat_2_Q_linkage_single(self):
        "Tests maxRstat(Z, R, 2) on the Q data set using single linkage."
        X = eo['Q-X']
        Y = pdist(X)
        Z = linkage(X, 'single')
        R = inconsistent(Z)
        MD = maxRstat(Z, R, 2)
        eps = 1e-15
        expectedMD = calculate_maximum_inconsistencies(Z, R, 2)
        self.failUnless(within_tol(MD, expectedMD, eps))

    def test_maxRstat_2_Q_linkage_complete(self):
        "Tests maxRstat(Z, R, 2) on the Q data set using complete linkage."
        X = eo['Q-X']
        Y = pdist(X)
        Z = linkage(X, 'complete')
        R = inconsistent(Z)
        MD = maxRstat(Z, R, 2)
        eps = 1e-15
        expectedMD = calculate_maximum_inconsistencies(Z, R, 2)
        self.failUnless(within_tol(MD, expectedMD, eps))

    def test_maxRstat_2_Q_linkage_ward(self):
        "Tests maxRstat(Z, R, 2) on the Q data set using Ward linkage."
        X = eo['Q-X']
        Y = pdist(X)
        Z = linkage(X, 'ward')
        R = inconsistent(Z)
        MD = maxRstat(Z, R, 2)
        eps = 1e-15
        expectedMD = calculate_maximum_inconsistencies(Z, R, 2)
        self.failUnless(within_tol(MD, expectedMD, eps))

    def test_maxRstat_2_Q_linkage_centroid(self):
        "Tests maxRstat(Z, R, 2) on the Q data set using centroid linkage."
        X = eo['Q-X']
        Y = pdist(X)
        Z = linkage(X, 'centroid')
        R = inconsistent(Z)
        MD = maxRstat(Z, R, 2)
        eps = 1e-15
        expectedMD = calculate_maximum_inconsistencies(Z, R, 2)
        self.failUnless(within_tol(MD, expectedMD, eps))

    def test_maxRstat_2_Q_linkage_median(self):
        "Tests maxRstat(Z, R, 2) on the Q data set using median linkage."
        X = eo['Q-X']
        Y = pdist(X)
        Z = linkage(X, 'median')
        R = inconsistent(Z)
        MD = maxRstat(Z, R, 2)
        eps = 1e-15
        expectedMD = calculate_maximum_inconsistencies(Z, R, 2)
        self.failUnless(within_tol(MD, expectedMD, eps))

    def test_maxRstat_3_empty_linkage(self):
        "Tests maxRstat(Z, R, 3) on empty linkage. Expecting exception."
        Z = np.zeros((0, 4), dtype=np.double)
        R = np.zeros((0, 4), dtype=np.double)
        self.failUnlessRaises(ValueError, maxRstat, Z, R, 3)

    def test_maxRstat_3_difrow_linkage(self):
        "Tests maxRstat(Z, R, 3) on linkage and inconsistency matrices with different numbers of clusters. Expecting exception."
        Z = np.asarray([[0, 1, 0.3, 4]], dtype=np.double)
        R = np.random.rand(2, 4)
        self.failUnlessRaises(ValueError, maxRstat, Z, R, 3)

    def test_maxRstat_3_one_cluster_linkage(self):
        "Tests maxRstat(Z, R, 3) on linkage with one cluster."
        Z = np.asarray([[0, 1, 0.3, 4]], dtype=np.double)
        R = np.asarray([[0, 0, 0, 0.3]], dtype=np.double)
        MD = maxRstat(Z, R, 3)
        eps = 1e-15
        expectedMD = calculate_maximum_inconsistencies(Z, R, 3)
        self.failUnless(within_tol(MD, expectedMD, eps))

    def test_maxRstat_3_Q_linkage_single(self):
        "Tests maxRstat(Z, R, 3) on the Q data set using single linkage."
        X = eo['Q-X']
        Y = pdist(X)
        Z = linkage(X, 'single')
        R = inconsistent(Z)
        MD = maxRstat(Z, R, 3)
        eps = 1e-15
        expectedMD = calculate_maximum_inconsistencies(Z, R, 3)
        self.failUnless(within_tol(MD, expectedMD, eps))

    def test_maxRstat_3_Q_linkage_complete(self):
        "Tests maxRstat(Z, R, 3) on the Q data set using complete linkage."
        X = eo['Q-X']
        Y = pdist(X)
        Z = linkage(X, 'complete')
        R = inconsistent(Z)
        MD = maxRstat(Z, R, 3)
        eps = 1e-15
        expectedMD = calculate_maximum_inconsistencies(Z, R, 3)
        self.failUnless(within_tol(MD, expectedMD, eps))

    def test_maxRstat_3_Q_linkage_ward(self):
        "Tests maxRstat(Z, R, 3) on the Q data set using Ward linkage."
        X = eo['Q-X']
        Y = pdist(X)
        Z = linkage(X, 'ward')
        R = inconsistent(Z)
        MD = maxRstat(Z, R, 3)
        eps = 1e-15
        expectedMD = calculate_maximum_inconsistencies(Z, R, 3)
        self.failUnless(within_tol(MD, expectedMD, eps))

    def test_maxRstat_3_Q_linkage_centroid(self):
        "Tests maxRstat(Z, R, 3) on the Q data set using centroid linkage."
        X = eo['Q-X']
        Y = pdist(X)
        Z = linkage(X, 'centroid')
        R = inconsistent(Z)
        MD = maxRstat(Z, R, 3)
        eps = 1e-15
        expectedMD = calculate_maximum_inconsistencies(Z, R, 3)
        self.failUnless(within_tol(MD, expectedMD, eps))

    def test_maxRstat_3_Q_linkage_median(self):
        "Tests maxRstat(Z, R, 3) on the Q data set using median linkage."
        X = eo['Q-X']
        Y = pdist(X)
        Z = linkage(X, 'median')
        R = inconsistent(Z)
        MD = maxRstat(Z, R, 3)
        eps = 1e-15
        expectedMD = calculate_maximum_inconsistencies(Z, R, 3)
        self.failUnless(within_tol(MD, expectedMD, eps))

def calculate_maximum_distances(Z):
    "Used for testing correctness of maxdists. Very slow."
=======
            assert_equal(num_obs_linkage(Z), n)


class TestIsMonotonic(TestCase):
    def test_is_monotonic_empty(self):
        # Tests is_monotonic(Z) on an empty linkage.
        Z = np.zeros((0, 4))
        assert_raises(ValueError, is_monotonic, Z)

    def test_is_monotonic_1x4(self):
        # Tests is_monotonic(Z) on 1x4 linkage. Expecting True.
        Z = np.asarray([[0, 1, 0.3, 2]], dtype=np.double)
        assert_equal(is_monotonic(Z), True)

    def test_is_monotonic_2x4_T(self):
        # Tests is_monotonic(Z) on 2x4 linkage. Expecting True.
        Z = np.asarray([[0, 1, 0.3, 2],
                        [2, 3, 0.4, 3]], dtype=np.double)
        assert_equal(is_monotonic(Z), True)

    def test_is_monotonic_2x4_F(self):
        # Tests is_monotonic(Z) on 2x4 linkage. Expecting False.
        Z = np.asarray([[0, 1, 0.4, 2],
                        [2, 3, 0.3, 3]], dtype=np.double)
        assert_equal(is_monotonic(Z), False)

    def test_is_monotonic_3x4_T(self):
        # Tests is_monotonic(Z) on 3x4 linkage. Expecting True.
        Z = np.asarray([[0, 1, 0.3, 2],
                        [2, 3, 0.4, 2],
                        [4, 5, 0.6, 4]], dtype=np.double)
        assert_equal(is_monotonic(Z), True)

    def test_is_monotonic_3x4_F1(self):
        # Tests is_monotonic(Z) on 3x4 linkage (case 1). Expecting False.
        Z = np.asarray([[0, 1, 0.3, 2],
                        [2, 3, 0.2, 2],
                        [4, 5, 0.6, 4]], dtype=np.double)
        assert_equal(is_monotonic(Z), False)

    def test_is_monotonic_3x4_F2(self):
        # Tests is_monotonic(Z) on 3x4 linkage (case 2). Expecting False.
        Z = np.asarray([[0, 1, 0.8, 2],
                        [2, 3, 0.4, 2],
                        [4, 5, 0.6, 4]], dtype=np.double)
        assert_equal(is_monotonic(Z), False)

    def test_is_monotonic_3x4_F3(self):
        # Tests is_monotonic(Z) on 3x4 linkage (case 3). Expecting False
        Z = np.asarray([[0, 1, 0.3, 2],
                        [2, 3, 0.4, 2],
                        [4, 5, 0.2, 4]], dtype=np.double)
        assert_equal(is_monotonic(Z), False)

    def test_is_monotonic_tdist_linkage1(self):
        # Tests is_monotonic(Z) on clustering generated by single linkage on
        # tdist data set. Expecting True.
        Z = linkage(hierarchy_test_data.ytdist, 'single')
        assert_equal(is_monotonic(Z), True)

    def test_is_monotonic_tdist_linkage2(self):
        # Tests is_monotonic(Z) on clustering generated by single linkage on
        # tdist data set. Perturbing. Expecting False.
        Z = linkage(hierarchy_test_data.ytdist, 'single')
        Z[2,2] = 0.0
        assert_equal(is_monotonic(Z), False)

    def test_is_monotonic_Q_linkage(self):
        # Tests is_monotonic(Z) on clustering generated by single linkage on
        # Q data set. Expecting True.
        X = hierarchy_test_data.Q_X
        Z = linkage(X, 'single')
        assert_equal(is_monotonic(Z), True)


class TestMaxDists(object):
    def test_maxdists_empty_linkage(self):
        # Tests maxdists(Z) on empty linkage. Expecting exception.
        Z = np.zeros((0, 4), dtype=np.double)
        assert_raises(ValueError, maxdists, Z)

    def test_maxdists_one_cluster_linkage(self):
        # Tests maxdists(Z) on linkage with one cluster.
        Z = np.asarray([[0, 1, 0.3, 4]], dtype=np.double)
        MD = maxdists(Z)
        expectedMD = calculate_maximum_distances(Z)
        assert_allclose(MD, expectedMD, atol=1e-15)

    def test_maxdists_Q_linkage(self):
        for method in ['single', 'complete', 'ward', 'centroid', 'median']:
            yield self.check_maxdists_Q_linkage, method

    def check_maxdists_Q_linkage(self, method):
        # Tests maxdists(Z) on the Q data set
        X = hierarchy_test_data.Q_X
        Z = linkage(X, method)
        MD = maxdists(Z)
        expectedMD = calculate_maximum_distances(Z)
        assert_allclose(MD, expectedMD, atol=1e-15)


class TestMaxInconsts(object):
    def test_maxinconsts_empty_linkage(self):
        # Tests maxinconsts(Z, R) on empty linkage. Expecting exception.
        Z = np.zeros((0, 4), dtype=np.double)
        R = np.zeros((0, 4), dtype=np.double)
        assert_raises(ValueError, maxinconsts, Z, R)

    def test_maxinconsts_difrow_linkage(self):
        # Tests maxinconsts(Z, R) on linkage and inconsistency matrices with
        # different numbers of clusters. Expecting exception.
        Z = np.asarray([[0, 1, 0.3, 4]], dtype=np.double)
        R = np.random.rand(2, 4)
        assert_raises(ValueError, maxinconsts, Z, R)

    def test_maxinconsts_one_cluster_linkage(self):
        # Tests maxinconsts(Z, R) on linkage with one cluster.
        Z = np.asarray([[0, 1, 0.3, 4]], dtype=np.double)
        R = np.asarray([[0, 0, 0, 0.3]], dtype=np.double)
        MD = maxinconsts(Z, R)
        expectedMD = calculate_maximum_inconsistencies(Z, R)
        assert_allclose(MD, expectedMD, atol=1e-15)

    def test_maxinconsts_Q_linkage(self):
        for method in ['single', 'complete', 'ward', 'centroid', 'median']:
            yield self.check_maxinconsts_Q_linkage, method

    def check_maxinconsts_Q_linkage(self, method):
        # Tests maxinconsts(Z, R) on the Q data set
        X = hierarchy_test_data.Q_X
        Z = linkage(X, method)
        R = inconsistent(Z)
        MD = maxinconsts(Z, R)
        expectedMD = calculate_maximum_inconsistencies(Z, R)
        assert_allclose(MD, expectedMD, atol=1e-15)


class TestMaxRStat(object):
    def test_maxRstat_invalid_index(self):
        for i in [3.3, -1, 4]:
            yield self.check_maxRstat_invalid_index, i

    def check_maxRstat_invalid_index(self, i):
        # Tests maxRstat(Z, R, i). Expecting exception.
        Z = np.asarray([[0, 1, 0.3, 4]], dtype=np.double)
        R = np.asarray([[0, 0, 0, 0.3]], dtype=np.double)
        if isinstance(i, int):
            assert_raises(ValueError, maxRstat, Z, R, i)
        else:
            assert_raises(TypeError, maxRstat, Z, R, i)

    def test_maxRstat_empty_linkage(self):
        for i in range(4):
            yield self.check_maxRstat_empty_linkage, i

    def check_maxRstat_empty_linkage(self, i):
        # Tests maxRstat(Z, R, i) on empty linkage. Expecting exception.
        Z = np.zeros((0, 4), dtype=np.double)
        R = np.zeros((0, 4), dtype=np.double)
        assert_raises(ValueError, maxRstat, Z, R, i)

    def test_maxRstat_difrow_linkage(self):
        for i in range(4):
            yield self.check_maxRstat_difrow_linkage, i

    def check_maxRstat_difrow_linkage(self, i):
        # Tests maxRstat(Z, R, i) on linkage and inconsistency matrices with
        # different numbers of clusters. Expecting exception.
        Z = np.asarray([[0, 1, 0.3, 4]], dtype=np.double)
        R = np.random.rand(2, 4)
        assert_raises(ValueError, maxRstat, Z, R, i)

    def test_maxRstat_one_cluster_linkage(self):
        for i in range(4):
            yield self.check_maxRstat_one_cluster_linkage, i

    def check_maxRstat_one_cluster_linkage(self, i):
        # Tests maxRstat(Z, R, i) on linkage with one cluster.
        Z = np.asarray([[0, 1, 0.3, 4]], dtype=np.double)
        R = np.asarray([[0, 0, 0, 0.3]], dtype=np.double)
        MD = maxRstat(Z, R, 1)
        expectedMD = calculate_maximum_inconsistencies(Z, R, 1)
        assert_allclose(MD, expectedMD, atol=1e-15)

    def test_maxRstat_Q_linkage(self):
        for method in ['single', 'complete', 'ward', 'centroid', 'median']:
            for i in range(4):
                yield self.check_maxRstat_Q_linkage, method, i

    def check_maxRstat_Q_linkage(self, method, i):
        # Tests maxRstat(Z, R, i) on the Q data set
        X = hierarchy_test_data.Q_X
        Z = linkage(X, method)
        R = inconsistent(Z)
        MD = maxRstat(Z, R, 1)
        expectedMD = calculate_maximum_inconsistencies(Z, R, 1)
        assert_allclose(MD, expectedMD, atol=1e-15)


class TestDendrogram(object):
    def test_dendrogram_single_linkage_tdist(self):
        # Tests dendrogram calculation on single linkage of the tdist data set.
        Z = linkage(hierarchy_test_data.ytdist, 'single')
        R = dendrogram(Z, no_plot=True)
        leaves = R["leaves"]
        assert_equal(leaves, [2, 5, 1, 0, 3, 4])

    def test_valid_orientation(self):
        Z = linkage(hierarchy_test_data.ytdist, 'single')
        assert_raises(ValueError, dendrogram, Z, orientation="foo")

    @dec.skipif(not have_matplotlib)
    def test_dendrogram_plot(self):
        for orientation in ['top', 'bottom', 'left', 'right']:
            yield self.check_dendrogram_plot, orientation

    def check_dendrogram_plot(self, orientation):
        # Tests dendrogram plotting.
        Z = linkage(hierarchy_test_data.ytdist, 'single')
        expected = {'color_list': ['g', 'b', 'b', 'b', 'b'],
                    'dcoord': [[0.0, 138.0, 138.0, 0.0],
                               [0.0, 219.0, 219.0, 0.0],
                               [0.0, 255.0, 255.0, 219.0],
                               [0.0, 268.0, 268.0, 255.0],
                               [138.0, 295.0, 295.0, 268.0]],
                    'icoord': [[5.0, 5.0, 15.0, 15.0],
                               [45.0, 45.0, 55.0, 55.0],
                               [35.0, 35.0, 50.0, 50.0],
                               [25.0, 25.0, 42.5, 42.5],
                               [10.0, 10.0, 33.75, 33.75]],
                    'ivl': ['2', '5', '1', '0', '3', '4'],
                    'leaves': [2, 5, 1, 0, 3, 4]}

        fig = plt.figure()
        ax = fig.add_subplot(111)

        # test that dendrogram accepts ax keyword
        R1 = dendrogram(Z, ax=ax, orientation=orientation)
        plt.close()
        assert_equal(R1, expected)

        # test plotting to gca (will import pylab)
        R2 = dendrogram(Z, orientation=orientation)
        plt.close()
        assert_equal(R2, expected)

    @dec.skipif(not have_matplotlib)
    def test_dendrogram_truncate_mode(self):
        Z = linkage(hierarchy_test_data.ytdist, 'single')

        R = dendrogram(Z, 2, 'lastp', show_contracted=True)
        plt.close()
        assert_equal(R, {'color_list': ['b'],
                         'dcoord': [[0.0, 295.0, 295.0, 0.0]],
                         'icoord': [[5.0, 5.0, 15.0, 15.0]],
                         'ivl': ['(2)', '(4)'],
                         'leaves': [6, 9]})

        R = dendrogram(Z, 2, 'mtica', show_contracted=True)
        plt.close()
        assert_equal(R, {'color_list': ['g', 'b', 'b', 'b'],
                         'dcoord': [[0.0, 138.0, 138.0, 0.0],
                                    [0.0, 255.0, 255.0, 0.0],
                                    [0.0, 268.0, 268.0, 255.0],
                                    [138.0, 295.0, 295.0, 268.0]],
                         'icoord': [[5.0, 5.0, 15.0, 15.0],
                                    [35.0, 35.0, 45.0, 45.0],
                                    [25.0, 25.0, 40.0, 40.0],
                                    [10.0, 10.0, 32.5, 32.5]],
                         'ivl': ['2', '5', '1', '0', '(2)'],
                         'leaves': [2, 5, 1, 0, 7]})

    def test_dendrogram_colors(self):
        # Tests dendrogram plots with alternate colors
        Z = linkage(hierarchy_test_data.ytdist, 'single')

        set_link_color_palette(['c', 'm', 'y', 'k'])
        R = dendrogram(Z, no_plot=True,
                       above_threshold_color='g', color_threshold=250)
        set_link_color_palette(['g', 'r', 'c', 'm', 'y', 'k'])

        color_list = R['color_list']
        assert_equal(color_list, ['c', 'm', 'g', 'g', 'g'])


def calculate_maximum_distances(Z):
    # Used for testing correctness of maxdists.
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    n = Z.shape[0] + 1
    B = np.zeros((n-1,))
    q = np.zeros((3,))
    for i in xrange(0, n - 1):
        q[:] = 0.0
        left = Z[i, 0]
        right = Z[i, 1]
        if left >= n:
<<<<<<< HEAD
            q[0] = B[left - n]
        if right >= n:
            q[1] = B[right - n]
=======
            q[0] = B[int(left) - n]
        if right >= n:
            q[1] = B[int(right) - n]
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        q[2] = Z[i, 2]
        B[i] = q.max()
    return B

<<<<<<< HEAD
def calculate_maximum_inconsistencies(Z, R, k=3):
    "Used for testing correctness of maxinconsts. Very slow."
    n = Z.shape[0] + 1
    B = np.zeros((n-1,))
    q = np.zeros((3,))
    #print R.shape
=======

def calculate_maximum_inconsistencies(Z, R, k=3):
    # Used for testing correctness of maxinconsts.
    n = Z.shape[0] + 1
    B = np.zeros((n-1,))
    q = np.zeros((3,))
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    for i in xrange(0, n - 1):
        q[:] = 0.0
        left = Z[i, 0]
        right = Z[i, 1]
        if left >= n:
<<<<<<< HEAD
            q[0] = B[left - n]
        if right >= n:
            q[1] = B[right - n]
=======
            q[0] = B[int(left) - n]
        if right >= n:
            q[1] = B[int(right) - n]
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        q[2] = R[i, k]
        B[i] = q.max()
    return B

<<<<<<< HEAD
def within_tol(a, b, tol):
    return np.abs(a - b).max() < tol
=======

def test_euclidean_linkage_value_error():
    for method in scipy.cluster.hierarchy._cpy_euclid_methods:
        assert_raises(ValueError,
                linkage, [[1, 1], [1, 1]], method=method, metric='cityblock')


def test_2x2_linkage():
    Z1 = linkage([1], method='single', metric='euclidean')
    Z2 = linkage([[0, 1], [0, 0]], method='single', metric='euclidean')
    assert_allclose(Z1, Z2)

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

if __name__ == "__main__":
    run_module_suite()
