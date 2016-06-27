# Clustering by affinity propagation.
#
# Written by Konrad Hinsen <hinsen@cnrs-orleans.fr>
# last revision: 2008-8-22
#

"""
Clustering by Affinity Propagation

This clustering algorithm identifies clusters in a set of data items
based on a list of similarities between the items. The result is a
list of clusters, each cluster being defined by one 'exemplar' (the
item that is most representative of the cluster) and by other items.
The number of clusters is not specified in advance. Instead, a
parameter called 'preference' indicates how likely each item is to be
an exemplar. Often it is set to the same value for all items.
Low preference values yield few big clusters, whereas high preference
values yield many small clusters.

The algorithm is described in:
B.J. Frey & D. Dueck, Science 315, 972-976 (2007)
"""

from Scientific import N
import random

class DataSet(object):

    """A collection of data items with similarities
    """

    def __init__(self, items, similarities,
                 symmetric = False, minimal_similarity=None):
        """
        @param items: a sequence of data items
        @type items: sequence

        @param similarities: similarity values for item pairs.
           This parameter can have one of three forms:
            - a list if triples (index1, index2, similarity),
              where the indices point into the item list and the
              similarity is a real number.
            - a callable object (typically a function or a bound method)
              that is called with two items and returns the similarity.
            - an array of shape (N, N), where N is the number of items,
              containing the similarities. The diagonal elements are not used.

        @param symmetric: if C{True}, the similarity measure is assumed to be
            symmetric. If C{False}, no such assumption is made and the input
            data (if a list) must contain both directions for each pair.
            If the similarity is defined by a function, it will be called
            twice of symmtric=False and once if symmetric=True. If the
            similarity is defined by an array, this parameter is not used.
        @type symmetric: C{bool}

        @param minimal_similarity: a cutoff value for the similarities;
            values smaller than this cutoff are discarded. This is of use
            for large data sets because both the runtime and the memory
            consumption increase with the number of similarity values.
        @type minimal_similarity: C{float}
        """
        self.items = items
        self.nitems = len(items)
        self._setupSimilarities(items, similarities,
                                symmetric, minimal_similarity)
        self._setupIndices()

    def _setupSimilarities(self, items, similarities,
                           symmetric, minimal_similarity):
        self.similarities = []
        self.index = {}
        self.smallest_similarity = None
        self.largest_similarity = None
        self.median_similarity = None

        # Check for array of similarities
        if isinstance(similarities, N.ArrayType):
            if similarities.shape != 2*(self.nitems,):
                raise ValueError("Similarity array has wrong shape")
            for i in range(self.nitems):
                for k in range(self.nitems):
                    if i != k:
                        self._storeSimilarity(i, k, similarities[i, k],
                                              minimal_similarity)

        # Check for callable similarity function
        elif callable(similarities):
            for i in range(self.nitems):
                for k in range(i+1, self.nitems):
                    s = similarities(self.items[i], self.items[k])
                    self._storeSimilarity(i, k, s, minimal_similarity)
                    if not symmetric:
                        s = similarities(self.items[k], self.items[i])
                    self._storeSimilarity(k, i, s, minimal_similarity)

        # Assume list of (i, k, s) triples
        else:
            for i, k, s in similarities:
                if i < 0 or i > self.nitems or k < 0 or k > self.nitems:
                    raise ValueError("Index out of range in " + str((i, k, s)))
                if i == k:
                    raise ValueError("Equal indices in " + str((i, k, s)))
                self._storeSimilarity(i, k, s, minimal_similarity)
                if symmetric:
                    self._storeSimilarity(k, i, s, minimal_similarity)

        # Add indices for self terms
        for i in range(self.nitems):
            self._storeSimilarity(i, i, None, None)

        # Convert similarities to array
        self.nsimilarities = len(self.similarities)
        self.similarities = N.array(self.similarities[:-self.nitems])

        # Find smallest, largest, and median
        self.smallest_similarity = N.minimum.reduce(self.similarities)
        self.largest_similarity = N.maximum.reduce(self.similarities)
        sort_indices = N.argsort(self.similarities)
        ns = len(self.similarities)
        if ns % 2 == 1:
            self.median_similarity = self.similarities[sort_indices[ns/2]]
        else:
            self.median_similarity = \
              (self.similarities[sort_indices[ns/2]]
               + self.similarities[sort_indices[ns/2-1]])/2.

    def _storeSimilarity(self, i, k, s, minimal_similarity):
        if s >= minimal_similarity:
            index_by_i = self.index.setdefault(i, {})
            index_by_i[k] = len(self.similarities)
            self.similarities.append(s)

    def _setupIndices(self):
        self._setupRIndices()
        self._setupAIndices1()
        self._setupAIndices2()
        self._setupEIndices()

    def _setupRIndices(self):
        indices = []
        for i in range(self.nsimilarities):
            indices.append([])
        for i in range(self.nitems):
            for k1, index1 in self.index[i].items():
                for k2, index2 in self.index[i].items():
                    if k2 != k1:
                        indices[index1].append(index2)
        self.r_update_indices = [N.array(i) for i in indices]

    def _setupAIndices1(self):
        indices = N.zeros((self.nsimilarities,), N.Int)
        for i in range(self.nitems):
            for k, index in self.index[i].items():
                indices[index] = self.index[k][k]
        self.a_update_indices_1 = indices

    def _setupAIndices2(self):
        index_inv = {}
        for i in range(self.nitems):
            index_inv[i] = {}
        for i in range(self.nitems):
            for k, index in self.index[i].items():
                index_inv[k][i] = index
        indices = self.nsimilarities*[None]
        for k in range(self.nitems):
            all = index_inv[k].items()
            for i, index in all:
                indices[index] = N.array([x[1] for x in all
                                          if x[0] != i and x[0] != k])
        self.a_update_indices_2 = indices

    def _setupEIndices(self):
        indices = []
        for i in range(self.nitems):
            ii = []
            ik = []
            for k, index in self.index[i].items():
                ii.append(index)
                ik.append(k)
            indices.append((N.array(ii), N.array(ik)))
        self.e_indices = indices

    def findClusters(self, preferences, max_iterations=500,
                     convergence = 50, damping=0.5):
        """
        @param preferences: the preference values for the cluster
            identification. This can be either a single number,
            or a sequence with one value per item.
        @type preferences: C{float} or sequence of C{float}

        @param max_iterations: the number of iterations at which the
            algorithm is stopped even if there is no convergence.
        @type max_iterations: C{int}

        @param convergence: the number of iterations during which the
            cluster decomposition must remain stable before it is
            returned as converged.
        @type convergence: C{int}

        @param damping: a number between 0 and 1 that influences
            by fast affinity and responsibility values can change.
        @type damping: C{float}
        """
        preferences = N.array(preferences)
        if len(preferences.shape) == 0:
            preferences = preferences + N.zeros((self.nitems,), N.Float)
        if len(preferences) != self.nitems:
            raise ValueError("Number of preferences != number of items")

        noise_scale = 1.e-12*(self.largest_similarity-self.smallest_similarity)
        s = N.concatenate([self.similarities, preferences])
        for i in range(len(s)):
            s[i] += noise_scale*random.random()
        a = N.zeros(s.shape, N.Float)
        r = N.zeros(s.shape, N.Float)
        iterations_left = max_iterations
        convergence_count = 0
        self.exemplar = N.zeros((self.nitems,), N.Int)
        while True:
            a, r = _affinityPropagation(self, s, a, r, damping)
            e = a + r
            exemplar = N.zeros((self.nitems,), N.Int)
            for i in range(self.nitems):
                ii, ik = self.e_indices[i]
                exemplar[i] = ik[N.argmax(N.take(e, ii))]
            if N.logical_and.reduce(exemplar == self.exemplar):
                convergence_count += 1
                if convergence_count == convergence:
                    break
            else:
                self.exemplar = exemplar
            iterations_left -= 1
            if iterations_left == 0:
                raise ValueError("no convergence in %d iterations"
                                 % max_iterations)
        clusters = []
        indices = N.arange(self.nitems)
        exemplar_indices = N.repeat(indices, self.exemplar == indices)
        for i in exemplar_indices:
            members = list(N.repeat(indices, self.exemplar == self.exemplar[i]))
            members.remove(i)
            members.insert(0, i)
            clusters.append([self.items[m] for m in members])
        return clusters


try:

    from Scientific_affinitypropagation import _affinityPropagation

except ImportError:

    def _affinityPropagation(dataset, s, a, r, damping):
        aps = a + s
        r_new = N.zeros(s.shape, N.Float)
        for i in range(dataset.nsimilarities):
            r_new[i] = s[i] \
                       - N.maximum.reduce(N.take(aps,
                                                 dataset.r_update_indices[i]))
        r = damping*r + (1-damping)*r_new

        rpos = N.maximum(0., r)
        a_new = N.take(r, dataset.a_update_indices_1)
        a_new[-dataset.nitems:] = 0.
        for i in range(dataset.nsimilarities):
            a_new[i] += N.add.reduce(N.take(rpos,
                                            dataset.a_update_indices_2[i]))
        a_new[:-dataset.nitems] = N.minimum(0., a_new[:-dataset.nitems])
        a = damping*a + (1-damping)*a_new

        return a, r

if __name__ == "__main__":

    points = N.array([[-2.341500, 3.696800],
                      [-1.109200, 3.111700],
                      [-1.566900, 1.835100],
                      [-2.658500, 0.664900],
                      [-4.031700, 2.845700],
                      [-3.081000, 2.101100],
                      [2.588000, 1.781900],
                      [3.292300, 3.058500],
                      [4.031700, 1.622300],
                      [3.081000, -0.611700],
                      [0.264100, 0.398900],
                      [1.320400, 2.207400],
                      [0.193700, 3.643600],
                      [1.954200, -0.505300],
                      [1.637300, 1.409600],
                      [-0.123200, -1.516000],
                      [-1.355600, -3.058500],
                      [0.017600, -4.016000],
                      [1.003500, -3.590400],
                      [0.017600, -2.420200],
                      [-1.531700, -0.930900],
                      [-1.144400, 0.505300],
                      [0.616200, -1.516000],
                      [1.707700, -2.207400],
                      [2.095100, 3.430900]])
    def simfunc(p1, p2):
        return -N.sum((p1-p2)**2)
    data = DataSet(points, simfunc, symmetric=True)
    clusters = [N.array(c) for c in
                data.findClusters(data.median_similarity)]
    for c in clusters:
        print c
    from Gnuplot import plot
    apply(plot, clusters)
