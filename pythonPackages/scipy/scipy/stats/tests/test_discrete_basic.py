<<<<<<< HEAD
import numpy.testing as npt
import numpy as np
import nose

from scipy import stats

DECIMAL_meanvar = 0#1  # was 0

distdiscrete = [
    ['bernoulli',(0.3,)],
    ['binom',    (5, 0.4)],
    ['boltzmann',(1.4, 19)],
    ['dlaplace', (0.8,)], #0.5
    ['geom',     (0.5,)],
    ['hypergeom',(30, 12, 6)],
    ['hypergeom',(21,3,12)],  #numpy.random (3,18,12) numpy ticket:921
    ['hypergeom',(21,18,11)],  #numpy.random (18,3,11) numpy ticket:921
    ['logser',   (0.6,)],  # reenabled, numpy ticket:921
    ['nbinom',   (5, 0.5)],
    ['nbinom',   (0.4, 0.4)], #from tickets: 583
    ['planck',   (0.51,)],   #4.1
    ['poisson',  (0.6,)],
    ['randint',  (7, 31)],
    ['skellam',  (15, 8)],
    ['zipf',     (4,)] ]   # arg=4 is ok,
                           # Zipf broken for arg = 2, e.g. weird .stats
                           # looking closer, mean, var should be inf for arg=2


#@npt.dec.slow
def test_discrete_basic():
    for distname, arg in distdiscrete:
        distfn = getattr(stats,distname)
        #assert stats.dlaplace.rvs(0.8) is not None
        np.random.seed(9765456)
        rvs = distfn.rvs(size=2000,*arg)
        supp = np.unique(rvs)
        m,v = distfn.stats(*arg)
        #yield npt.assert_almost_equal(rvs.mean(), m, decimal=4,err_msg='mean')
        #yield npt.assert_almost_equal, rvs.mean(), m, 2, 'mean' # does not work
        yield check_sample_meanvar, rvs.mean(), m, distname + ' sample mean test'
        yield check_sample_meanvar, rvs.var(), v, distname + ' sample var test'
        yield check_cdf_ppf, distfn, arg, distname + ' cdf_ppf'
        yield check_cdf_ppf2, distfn, arg, supp, distname + ' cdf_ppf'
        yield check_pmf_cdf, distfn, arg, distname + ' pmf_cdf'
        yield check_oth, distfn, arg, distname + ' oth'
        skurt = stats.kurtosis(rvs)
        sskew = stats.skew(rvs)
        yield check_sample_skew_kurt, distfn, arg, skurt, sskew, \
                      distname + ' skew_kurt'
        if not distname in ['']:#['logser']:  #known failure, fixed
            alpha = 0.01
            yield check_discrete_chisquare, distfn, arg, rvs, alpha, \
                          distname + ' chisquare'

@npt.dec.slow
def test_discrete_extra():
    for distname, arg in distdiscrete:
        distfn = getattr(stats,distname)
        yield check_ppf_limits, distfn, arg, distname + \
              ' ppf limit test'
        yield check_isf_limits, distfn, arg, distname + \
              ' isf limit test'
        yield check_entropy, distfn, arg, distname + \
              ' entropy nan test'

@npt.dec.skipif(True)
def test_discrete_private():
    #testing private methods mostly for debugging
    #   some tests might fail by design,
    #   e.g. incorrect definition of distfn.a and distfn.b
    for distname, arg in distdiscrete:
        distfn = getattr(stats,distname)
        rvs = distfn.rvs(size=10000,*arg)
        m,v = distfn.stats(*arg)

        yield check_ppf_ppf, distfn, arg
        yield check_cdf_ppf_private, distfn, arg, distname
        yield check_generic_moment, distfn, arg, m, 1, 3   # last is decimal
        yield check_generic_moment, distfn, arg, v+m*m, 2, 3 # last is decimal
        yield check_moment_frozen, distfn, arg, m, 1, 3   # last is decimal
        yield check_moment_frozen, distfn, arg, v+m*m, 2, 3 # last is decimal


def check_sample_meanvar(sm,m,msg):
    if not np.isinf(m):
        npt.assert_almost_equal(sm, m, decimal=DECIMAL_meanvar, err_msg=msg + \
                                ' - finite moment')
    else:
        assert sm > 10000, 'infinite moment, sm = ' + str(sm)

def check_sample_var(sm,m,msg):
    npt.assert_almost_equal(sm, m, decimal=DECIMAL_meanvar, err_msg= msg + 'var')

def check_cdf_ppf(distfn,arg,msg):
    ppf05 = distfn.ppf(0.5,*arg)
    cdf05 = distfn.cdf(ppf05,*arg)
    npt.assert_almost_equal(distfn.ppf(cdf05-1e-6,*arg),ppf05,
                            err_msg=msg + 'ppf-cdf-median')
    assert (distfn.ppf(cdf05+1e-4,*arg)>ppf05), msg + 'ppf-cdf-next'

def check_cdf_ppf2(distfn,arg,supp,msg):
    npt.assert_array_equal(distfn.ppf(distfn.cdf(supp,*arg),*arg),
                           supp, msg + '-roundtrip')
    npt.assert_array_equal(distfn.ppf(distfn.cdf(supp,*arg)-1e-8,*arg),
                           supp, msg + '-roundtrip')
    # -1e-8 could cause an error if pmf < 1e-8


def check_cdf_ppf_private(distfn,arg,msg):
    ppf05 = distfn._ppf(0.5,*arg)
    cdf05 = distfn.cdf(ppf05,*arg)
    npt.assert_almost_equal(distfn._ppf(cdf05-1e-6,*arg),ppf05,
                            err_msg=msg + '_ppf-cdf-median ')
    assert (distfn._ppf(cdf05+1e-4,*arg)>ppf05), msg + '_ppf-cdf-next'

def check_ppf_ppf(distfn, arg):
    assert distfn.ppf(0.5,*arg) < np.inf
    ppfs = distfn.ppf([0.5,0.9],*arg)
    ppf_s = [distfn._ppf(0.5,*arg), distfn._ppf(0.9,*arg)]
    assert np.all(ppfs < np.inf)
    assert ppf_s[0] == distfn.ppf(0.5,*arg)
    assert ppf_s[1] == distfn.ppf(0.9,*arg)
    assert ppf_s[0] == ppfs[0]
    assert ppf_s[1] == ppfs[1]

def check_pmf_cdf(distfn, arg, msg):
    startind = np.int(distfn._ppf(0.01,*arg)-1)
    index = range(startind,startind+10)
    cdfs = distfn.cdf(index,*arg)
    npt.assert_almost_equal(cdfs, distfn.pmf(index, *arg).cumsum() + \
                            cdfs[0] - distfn.pmf(index[0],*arg),
                            decimal=4, err_msg= msg + 'pmf-cdf')

def check_generic_moment(distfn, arg, m, k, decim):
    npt.assert_almost_equal(distfn.generic_moment(k,*arg), m, decimal=decim,
                            err_msg= str(distfn) + ' generic moment test')

def check_moment_frozen(distfn, arg, m, k, decim):
    npt.assert_almost_equal(distfn(*arg).moment(k), m, decimal=decim,
                            err_msg= str(distfn) + ' frozen moment test')

def check_oth(distfn, arg, msg):
    #checking other methods of distfn
    meanint = round(distfn.stats(*arg)[0]) # closest integer to mean
    npt.assert_almost_equal(distfn.sf(meanint, *arg), 1 - \
                            distfn.cdf(meanint, *arg), decimal=8)
    median_sf = distfn.isf(0.5, *arg)

    assert distfn.sf(median_sf - 1, *arg) > 0.5
    assert distfn.cdf(median_sf + 1, *arg) > 0.5
    npt.assert_equal(distfn.isf(0.5, *arg), distfn.ppf(0.5, *arg))

#next 3 functions copied from test_continous_extra
#    adjusted

def check_ppf_limits(distfn,arg,msg):
    below,low,upp,above = distfn.ppf([-1,0,1,2], *arg)
    #print distfn.name, distfn.a, low, distfn.b, upp
    #print distfn.name,below,low,upp,above
    assert_equal_inf_nan(distfn.a-1,low, msg + 'ppf lower bound')
    assert_equal_inf_nan(distfn.b,upp, msg + 'ppf upper bound')
    assert np.isnan(below), msg + 'ppf out of bounds - below'
    assert np.isnan(above), msg + 'ppf out of bounds - above'

def check_isf_limits(distfn,arg,msg):
    below,low,upp,above = distfn.isf([-1,0,1,2], *arg)
    #print distfn.name, distfn.a, low, distfn.b, upp
    #print distfn.name,below,low,upp,above
    assert_equal_inf_nan(distfn.a-1,upp, msg + 'isf lower bound')
    assert_equal_inf_nan(distfn.b,low, msg + 'isf upper bound')
    assert np.isnan(below), msg + 'isf out of bounds - below'
    assert np.isnan(above), msg + 'isf out of bounds - above'

def assert_equal_inf_nan(v1,v2,msg):
    assert not np.isnan(v1)
    if not np.isinf(v1):
        npt.assert_almost_equal(v1, v2, decimal=10, err_msg = msg + \
                                   ' - finite')
    else:
        assert np.isinf(v2) or np.isnan(v2), \
               msg + ' - infinite, v2=%s' % str(v2)

def check_sample_skew_kurt(distfn, arg, sk, ss, msg):
    k,s = distfn.stats(moment='ks',*arg)
    check_sample_meanvar, sk, k, msg + 'sample skew test'
    check_sample_meanvar, ss, s, msg + 'sample kurtosis test'


def check_entropy(distfn,arg,msg):
    ent = distfn.entropy(*arg)
    #print 'Entropy =', ent
    assert not np.isnan(ent), msg + 'test Entropy is nan'\



def check_discrete_chisquare(distfn, arg, rvs, alpha, msg):
    '''perform chisquare test for random sample of a discrete distribution
=======
from __future__ import division, print_function, absolute_import

import numpy.testing as npt
import numpy as np
from scipy._lib.six import xrange

from scipy import stats
from common_tests import (check_normalization, check_moment, check_mean_expect,
        check_var_expect, check_skew_expect, check_kurt_expect,
        check_entropy, check_private_entropy, check_edge_support,
        check_named_args, check_random_state_property)
from scipy.stats._distr_params import distdiscrete
knf = npt.dec.knownfailureif

vals = ([1, 2, 3, 4], [0.1, 0.2, 0.3, 0.4])
distdiscrete += [[stats.rv_discrete(values=vals), ()]]


def test_discrete_basic():
    for distname, arg in distdiscrete:
        try:
            distfn = getattr(stats, distname)
        except TypeError:
            distfn = distname
            distname = 'sample distribution'
        np.random.seed(9765456)
        rvs = distfn.rvs(size=2000, *arg)
        supp = np.unique(rvs)
        m, v = distfn.stats(*arg)
        yield check_cdf_ppf, distfn, arg, supp, distname + ' cdf_ppf'

        yield check_pmf_cdf, distfn, arg, distname
        yield check_oth, distfn, arg, supp, distname + ' oth'
        yield check_edge_support, distfn, arg

        alpha = 0.01
        yield check_discrete_chisquare, distfn, arg, rvs, alpha, \
                      distname + ' chisquare'

    seen = set()
    for distname, arg in distdiscrete:
        if distname in seen:
            continue
        seen.add(distname)
        try:
            distfn = getattr(stats, distname)
        except TypeError:
            distfn = distname
            distname = 'sample distribution'
        locscale_defaults = (0,)
        meths = [distfn.pmf, distfn.logpmf, distfn.cdf, distfn.logcdf,
                 distfn.logsf]
        # make sure arguments are within support
        spec_k = {'randint': 11, 'hypergeom': 4, 'bernoulli': 0, }
        k = spec_k.get(distname, 1)
        yield check_named_args, distfn, k, arg, locscale_defaults, meths
        if distname != 'sample distribution':
            yield check_scale_docstring, distfn
        yield check_random_state_property, distfn, arg

        # Entropy
        yield check_entropy, distfn, arg, distname
        if distfn.__class__._entropy != stats.rv_discrete._entropy:
            yield check_private_entropy, distfn, arg, stats.rv_discrete


def test_moments():
    for distname, arg in distdiscrete:
        try:
            distfn = getattr(stats, distname)
        except TypeError:
            distfn = distname
            distname = 'sample distribution'
        m, v, s, k = distfn.stats(*arg, moments='mvsk')
        yield check_normalization, distfn, arg, distname

        # compare `stats` and `moment` methods
        yield check_moment, distfn, arg, m, v, distname
        yield check_mean_expect, distfn, arg, m, distname
        yield check_var_expect, distfn, arg, m, v, distname
        yield check_skew_expect, distfn, arg, m, v, s, distname

        cond = distname in ['zipf']
        msg = distname + ' fails kurtosis'
        yield knf(cond, msg)(check_kurt_expect), distfn, arg, m, v, k, distname

        # frozen distr moments
        yield check_moment_frozen, distfn, arg, m, 1
        yield check_moment_frozen, distfn, arg, v+m*m, 2


def check_cdf_ppf(distfn, arg, supp, msg):
    # cdf is a step function, and ppf(q) = min{k : cdf(k) >= q, k integer}
    npt.assert_array_equal(distfn.ppf(distfn.cdf(supp, *arg), *arg),
                           supp, msg + '-roundtrip')
    npt.assert_array_equal(distfn.ppf(distfn.cdf(supp, *arg) - 1e-8, *arg),
                           supp, msg + '-roundtrip')
    supp1 = supp[supp < distfn.b]
    npt.assert_array_equal(distfn.ppf(distfn.cdf(supp1, *arg) + 1e-8, *arg),
                     supp1 + distfn.inc, msg + 'ppf-cdf-next')
    # -1e-8 could cause an error if pmf < 1e-8


def check_pmf_cdf(distfn, arg, distname):
    startind = int(distfn.ppf(0.01, *arg) - 1)
    index = list(range(startind, startind + 10))
    cdfs, pmfs_cum = distfn.cdf(index,*arg), distfn.pmf(index, *arg).cumsum()

    atol, rtol = 1e-10, 1e-10
    if distname == 'skellam':    # ncx2 accuracy
        atol, rtol = 1e-5, 1e-5
    npt.assert_allclose(cdfs - cdfs[0], pmfs_cum - pmfs_cum[0],
            atol=atol, rtol=rtol)


def check_moment_frozen(distfn, arg, m, k):
    npt.assert_allclose(distfn(*arg).moment(k), m,
            atol=1e-10, rtol=1e-10)


def check_oth(distfn, arg, supp, msg):
    # checking other methods of distfn
    npt.assert_allclose(distfn.sf(supp, *arg), 1. - distfn.cdf(supp, *arg),
            atol=1e-10, rtol=1e-10)

    q = np.linspace(0.01, 0.99, 20)
    npt.assert_allclose(distfn.isf(q, *arg), distfn.ppf(1. - q, *arg),
            atol=1e-10, rtol=1e-10)

    median_sf = distfn.isf(0.5, *arg)
    npt.assert_(distfn.sf(median_sf - 1, *arg) > 0.5)
    npt.assert_(distfn.cdf(median_sf + 1, *arg) > 0.5)


def check_discrete_chisquare(distfn, arg, rvs, alpha, msg):
    """Perform chisquare test for random sample of a discrete distribution
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    Parameters
    ----------
    distname : string
        name of distribution function
    arg : sequence
        parameters of distribution
    alpha : float
        significance level, threshold for p-value

    Returns
    -------
    result : bool
        0 if test passes, 1 if test fails

    uses global variable debug for printing results
<<<<<<< HEAD
    '''

    # define parameters for test
##    n=2000
=======

    """
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    n = len(rvs)
    nsupp = 20
    wsupp = 1.0/nsupp

<<<<<<< HEAD
##    distfn = getattr(stats, distname)
##    np.random.seed(9765456)
##    rvs = distfn.rvs(size=n,*arg)

    # construct intervals with minimum mass 1/nsupp
    # intervalls are left-half-open as in a cdf difference
=======
    # construct intervals with minimum mass 1/nsupp
    # intervals are left-half-open as in a cdf difference
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    distsupport = xrange(max(distfn.a, -1000), min(distfn.b, 1000) + 1)
    last = 0
    distsupp = [max(distfn.a, -1000)]
    distmass = []
    for ii in distsupport:
        current = distfn.cdf(ii,*arg)
<<<<<<< HEAD
        if  current - last >= wsupp-1e-14:
=======
        if current - last >= wsupp-1e-14:
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
            distsupp.append(ii)
            distmass.append(current - last)
            last = current
            if current > (1-wsupp):
                break
<<<<<<< HEAD
    if distsupp[-1]  < distfn.b:
=======
    if distsupp[-1] < distfn.b:
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        distsupp.append(distfn.b)
        distmass.append(1-last)
    distsupp = np.array(distsupp)
    distmass = np.array(distmass)

    # convert intervals to right-half-open as required by histogram
    histsupp = distsupp+1e-8
    histsupp[0] = distfn.a

    # find sample frequencies and perform chisquare test
    freq,hsupp = np.histogram(rvs,histsupp)
    cdfs = distfn.cdf(distsupp,*arg)
    (chis,pval) = stats.chisquare(np.array(freq),n*distmass)

<<<<<<< HEAD
    assert (pval > alpha), 'chisquare - test for %s' \
           'at arg = %s with pval = %s' % (msg,str(arg),str(pval))


if __name__ == "__main__":
    #nose.run(argv=['', __file__])
    nose.runmodule(argv=[__file__,'-s'], exit=False)
=======
    npt.assert_(pval > alpha, 'chisquare - test for %s'
           ' at arg = %s with pval = %s' % (msg,str(arg),str(pval)))


def check_scale_docstring(distfn):
    if distfn.__doc__ is not None:
        # Docstrings can be stripped if interpreter is run with -OO
        npt.assert_('scale' not in distfn.__doc__)


if __name__ == "__main__":
    npt.run_module_suite()
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
