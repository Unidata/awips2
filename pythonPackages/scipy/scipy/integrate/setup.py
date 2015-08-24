#!/usr/bin/env python
<<<<<<< HEAD

from os.path import join

=======
from __future__ import division, print_function, absolute_import

from os.path import join

from scipy._build_utils import numpy_nodepr_api


>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
def configuration(parent_package='',top_path=None):
    from numpy.distutils.misc_util import Configuration
    from numpy.distutils.system_info import get_info
    config = Configuration('integrate', parent_package, top_path)

<<<<<<< HEAD
    blas_opt = get_info('blas_opt',notfound_action=2)

    config.add_library('linpack_lite',
                       sources=[join('linpack_lite','*.f')])
    config.add_library('mach',
                       sources=[join('mach','*.f')],
                       config_fc={'noopt':(__file__,1)})
    config.add_library('quadpack',
                       sources=[join('quadpack','*.f')])
    config.add_library('odepack',
                       sources=[join('odepack','*.f')])
    config.add_library('dop',
                       sources=[join('dop','*.f')])
    # should we try to weed through files and replace with calls to
    # LAPACK routines?
    # Yes, someday...



    # Extensions
    # quadpack:

    config.add_extension('_quadpack',
                         sources=['_quadpackmodule.c'],
                         libraries=['quadpack', 'linpack_lite', 'mach'],
                         depends=['quadpack.h','__quadpack.h'])
    # odepack
    libs = ['odepack','linpack_lite','mach']
    

    # Remove libraries key from blas_opt
    if 'libraries' in blas_opt:    # key doesn't exist on OS X ...
        libs.extend(blas_opt['libraries'])
    newblas = {}
    for key in blas_opt.keys():
        if key == 'libraries':
            continue
        newblas[key] = blas_opt[key]
    config.add_extension('_odepack',
                         sources=['_odepackmodule.c'],
                         libraries=libs,
                         depends=['__odepack.h','multipack.h'],
                         **newblas)
=======
    # Get a local copy of lapack_opt_info
    lapack_opt = dict(get_info('lapack_opt',notfound_action=2))
    # Pop off the libraries list so it can be combined with
    # additional required libraries
    lapack_libs = lapack_opt.pop('libraries', [])

    mach_src = [join('mach','*.f')]
    quadpack_src = [join('quadpack','*.f')]
    odepack_src = [join('odepack','*.f')]
    dop_src = [join('dop','*.f')]
    quadpack_test_src = [join('tests','_test_multivariate.c')]
    odeint_banded_test_src = [join('tests', 'banded5x5.f')]

    config.add_library('mach', sources=mach_src,
                       config_fc={'noopt':(__file__,1)})
    config.add_library('quadpack', sources=quadpack_src)
    config.add_library('odepack', sources=odepack_src)
    config.add_library('dop', sources=dop_src)

    # Extensions
    # quadpack:
    config.add_extension('_quadpack',
                         sources=['_quadpackmodule.c'],
                         libraries=(['quadpack', 'mach'] + lapack_libs),
                         depends=(['quadpack.h','__quadpack.h']
                                  + quadpack_src + mach_src),
                         **lapack_opt)

    # odepack
    odepack_libs = ['odepack','mach'] + lapack_libs

    odepack_opts = lapack_opt.copy()
    odepack_opts.update(numpy_nodepr_api)
    config.add_extension('_odepack',
                         sources=['_odepackmodule.c'],
                         libraries=odepack_libs,
                         depends=(odepack_src + mach_src),
                         **odepack_opts)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    # vode
    config.add_extension('vode',
                         sources=['vode.pyf'],
<<<<<<< HEAD
                         libraries=libs,
                         **newblas)
=======
                         libraries=odepack_libs,
                         depends=(odepack_src
                                  + mach_src),
                         **lapack_opt)

    # lsoda
    config.add_extension('lsoda',
                         sources=['lsoda.pyf'],
                         libraries=odepack_libs,
                         depends=(odepack_src
                                  + mach_src),
                         **lapack_opt)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    # dop
    config.add_extension('_dop',
                         sources=['dop.pyf'],
<<<<<<< HEAD
                         libraries=['dop'])
=======
                         libraries=['dop'],
                         depends=dop_src)

    config.add_extension('_test_multivariate',
                         sources=quadpack_test_src)

    # Fortran+f2py extension module for testing odeint.
    config.add_extension('_test_odeint_banded',
                         sources=odeint_banded_test_src,
                         libraries=odepack_libs,
                         depends=(odepack_src + mach_src),
                         **lapack_opt)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    config.add_data_dir('tests')
    return config

if __name__ == '__main__':
    from numpy.distutils.core import setup
    setup(**configuration(top_path='').todict())
