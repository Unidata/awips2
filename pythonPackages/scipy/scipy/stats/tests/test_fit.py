<<<<<<< HEAD
# NOTE: contains only one test, _est_cont_fit, that is renamed so that
#       nose doesn't run it
# I put this here for the record and for the case when someone wants to
# verify the quality of fit
# with current parameters: relatively small sample size, default starting values
#       Ran 84 tests in 401.797s
#       FAILED (failures=15)


import numpy.testing as npt
import numpy as np

from scipy import stats

from test_continuous_basic import distcont

# this is not a proper statistical test for convergence, but only
# verifies that the estimate and true values don't differ by too much
n_repl1 = 1000 # sample size for first run
n_repl2 = 5000 # sample size for second run, if first run fails
thresh_percent = 0.25 # percent of true parameters for fail cut-off
thresh_min = 0.75  # minimum difference estimate - true to fail test

#distcont = [['genextreme', (3.3184017469423535,)]]

def _est_cont_fit():
    # this tests the closeness of the estimated parameters to the true
    # parameters with fit method of continuous distributions
    # Note: is slow, some distributions don't converge with sample size <= 10000

    for distname, arg in distcont:
        yield check_cont_fit, distname,arg


def check_cont_fit(distname,arg):        
    distfn = getattr(stats, distname)
    rvs = distfn.rvs(size=n_repl1,*arg)
    est = distfn.fit(rvs)  #,*arg) # start with default values

    truearg = np.hstack([arg,[0.0,1.0]])
    diff = est-truearg
        
    txt = ''
    diffthreshold = np.max(np.vstack([truearg*thresh_percent,
                    np.ones(distfn.numargs+2)*thresh_min]),0)
    # threshold for location
    diffthreshold[-2] = np.max([np.abs(rvs.mean())*thresh_percent,thresh_min])
    
    if np.any(np.isnan(est)):
        raise AssertionError, 'nan returned in fit'
    else:  
        if np.any((np.abs(diff) - diffthreshold) > 0.0):
##            txt = 'WARNING - diff too large with small sample'
##            print 'parameter diff =', diff - diffthreshold, txt
            rvs = np.concatenate([rvs,distfn.rvs(size=n_repl2-n_repl1,*arg)])
            est = distfn.fit(rvs) #,*arg)
            truearg = np.hstack([arg,[0.0,1.0]])
            diff = est-truearg
            if np.any((np.abs(diff) - diffthreshold) > 0.0):
                txt  = 'parameter: %s\n' % str(truearg)
                txt += 'estimated: %s\n' % str(est)
                txt += 'diff     : %s\n' % str(diff)
                raise AssertionError, 'fit not very good in %s\n' % distfn.name + txt
                


if __name__ == "__main__":
    import nose
    #nose.run(argv=['', __file__])
    nose.runmodule(argv=[__file__,'-s'], exit=False)
=======
from __future__ import division, print_function, absolute_import

import os

import numpy as np
from numpy.testing import dec, assert_allclose

from scipy import stats

from test_continuous_basic import distcont

# this is not a proper statistical test for convergence, but only
# verifies that the estimate and true values don't differ by too much

fit_sizes = [1000, 5000]  # sample sizes to try

thresh_percent = 0.25  # percent of true parameters for fail cut-off
thresh_min = 0.75  # minimum difference estimate - true to fail test

failing_fits = [
        'burr',
        'chi2',
        'gausshyper',
        'genexpon',
        'gengamma',
        'ksone',
        'mielke',
        'ncf',
        'ncx2',
        'pearson3',
        'powerlognorm',
        'truncexpon',
        'tukeylambda',
        'vonmises',
        'wrapcauchy',
        'levy_stable'
]

# Don't run the fit test on these:
skip_fit = [
    'erlang',  # Subclass of gamma, generates a warning.
]


@dec.slow
def test_cont_fit():
    # this tests the closeness of the estimated parameters to the true
    # parameters with fit method of continuous distributions
    # Note: is slow, some distributions don't converge with sample size <= 10000

    for distname, arg in distcont:
        if distname not in skip_fit:
            yield check_cont_fit, distname,arg


def check_cont_fit(distname,arg):
    if distname in failing_fits:
        # Skip failing fits unless overridden
        xfail = True
        try:
            xfail = not int(os.environ['SCIPY_XFAIL'])
        except:
            pass
        if xfail:
            msg = "Fitting %s doesn't work reliably yet" % distname
            msg += " [Set environment variable SCIPY_XFAIL=1 to run this test nevertheless.]"
            dec.knownfailureif(True, msg)(lambda: None)()

    distfn = getattr(stats, distname)

    truearg = np.hstack([arg,[0.0,1.0]])
    diffthreshold = np.max(np.vstack([truearg*thresh_percent,
                                      np.ones(distfn.numargs+2)*thresh_min]),0)

    for fit_size in fit_sizes:
        # Note that if a fit succeeds, the other fit_sizes are skipped
        np.random.seed(1234)

        with np.errstate(all='ignore'):
            rvs = distfn.rvs(size=fit_size, *arg)
            est = distfn.fit(rvs)  # start with default values

        diff = est - truearg

        # threshold for location
        diffthreshold[-2] = np.max([np.abs(rvs.mean())*thresh_percent,thresh_min])

        if np.any(np.isnan(est)):
            raise AssertionError('nan returned in fit')
        else:
            if np.all(np.abs(diff) <= diffthreshold):
                break
    else:
        txt = 'parameter: %s\n' % str(truearg)
        txt += 'estimated: %s\n' % str(est)
        txt += 'diff     : %s\n' % str(diff)
        raise AssertionError('fit not very good in %s\n' % distfn.name + txt)


def _check_loc_scale_mle_fit(name, data, desired, atol=None):
    d = getattr(stats, name)
    actual = d.fit(data)[-2:]
    assert_allclose(actual, desired, atol=atol,
                    err_msg='poor mle fit of (loc, scale) in %s' % name)


def test_non_default_loc_scale_mle_fit():
    data = np.array([1.01, 1.78, 1.78, 1.78, 1.88, 1.88, 1.88, 2.00])
    yield _check_loc_scale_mle_fit, 'uniform', data, [1.01, 0.99], 1e-3
    yield _check_loc_scale_mle_fit, 'expon', data, [1.01, 0.73875], 1e-3


if __name__ == "__main__":
    np.testing.run_module_suite()
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
