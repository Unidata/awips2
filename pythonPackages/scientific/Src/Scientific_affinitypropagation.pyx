# Implementation of Affinity Propagation in Pyrex
# Accelerates DataSet.findCluster by a factor of 5.
#
# Written by Konrad Hinsen
# last revision: 2007-3-20
#

include 'Scientific/numeric.pxi'

from Scientific import N

def _affinityPropagation(dataset, ArrayType s, ArrayType a,
                         ArrayType r, float damping):
    cdef ArrayType as
    cdef ArrayType r_new
    cdef ArrayType a_new
    cdef ArrayType rpos
    cdef ArrayType ind_array
    cdef long *ind
    cdef double *dptr
    cdef double v
    cdef int i
    cdef int j

    as = a + s
    r_new = N.zeros((dataset.nsimilarities,), N.Float)
    for i from 0 <= i < dataset.nsimilarities:
        ind_array = dataset.r_update_indices[i]
        ind = <long *>ind_array.data
        dptr = <double *>as.data
        v = dptr[ind[0]]
        for j from 1 <= j < ind_array.dimensions[0]:
            if dptr[ind[j]] > v:
                v = dptr[ind[j]]
        r_new[i] = s[i] - v
    r = damping*r + (1-damping)*r_new

    rpos = N.maximum(0., r)
    a_new = N.take(r, dataset.a_update_indices_1)
    a_new[-dataset.nitems:] = 0.
    for i from 0 <= i < dataset.nsimilarities:
        ind_array = dataset.a_update_indices_2[i]
        ind = <long *>ind_array.data
        dptr = <double *>rpos.data
        v = dptr[ind[0]]
        for j from 1 <= j < ind_array.dimensions[0]:
            v = v + dptr[ind[j]]
        a_new[i] = a_new[i] + v
    a_new[:-dataset.nitems] = N.minimum(0., a_new[:-dataset.nitems])
    a = damping*a + (1-damping)*a_new

    return a, r
