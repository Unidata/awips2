#!/usr/bin/env python

<<<<<<< HEAD
=======
from __future__ import division, print_function, absolute_import

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
import os
import sys
from os.path import join
from distutils.sysconfig import get_python_inc
import numpy
from numpy.distutils.misc_util import get_numpy_include_dirs

try:
    from numpy.distutils.misc_util import get_info
except ImportError:
<<<<<<< HEAD
    raise ValueError("numpy >= 1.4 is required (detected %s from %s)" % \
                     (numpy.__version__, numpy.__file__))

def configuration(parent_package='',top_path=None):
    from numpy.distutils.misc_util import Configuration
    config = Configuration('special', parent_package, top_path)

    define_macros = []
    if sys.platform=='win32':
#        define_macros.append(('NOINFINITIES',None))
#        define_macros.append(('NONANS',None))
        define_macros.append(('_USE_MATH_DEFINES',None))

    # C libraries
    config.add_library('sc_c_misc',sources=[join('c_misc','*.c')],
                       include_dirs=[get_python_inc(), get_numpy_include_dirs()],
                       macros=define_macros)
    config.add_library('sc_cephes',sources=[join('cephes','*.c')],
                       include_dirs=[get_python_inc(), get_numpy_include_dirs()],
                       macros=define_macros)

    # Fortran libraries
    config.add_library('sc_mach',sources=[join('mach','*.f')],
                       config_fc={'noopt':(__file__,1)})
    config.add_library('sc_toms',sources=[join('amos','*.f')])
    config.add_library('sc_amos',sources=[join('toms','*.f')])
    config.add_library('sc_cdf',sources=[join('cdflib','*.f')])
    config.add_library('sc_specfun',sources=[join('specfun','*.f')])

    # Extension _cephes
    sources = ['_cephesmodule.c', 'amos_wrappers.c', 'specfun_wrappers.c',
               'toms_wrappers.c','cdf_wrappers.c','ufunc_extras.c']
    config.add_extension('_cephes', sources=sources,
                         libraries=['sc_amos','sc_toms','sc_c_misc','sc_cephes','sc_mach',
                                    'sc_cdf', 'sc_specfun'],
                         depends=["ufunc_extras.h", "cephes.h",
                                  "amos_wrappers.h", "toms_wrappers.h",
                                  "cdf_wrappers.h", "specfun_wrappers.h",
                                  "c_misc/misc.h", "cephes_doc.h",
                                  "cephes/mconf.h", "cephes/cephes_names.h"],
                         define_macros = define_macros,
                         extra_info=get_info("npymath")
                         )
=======
    raise ValueError("numpy >= 1.4 is required (detected %s from %s)" %
                     (numpy.__version__, numpy.__file__))


def configuration(parent_package='',top_path=None):
    from numpy.distutils.misc_util import Configuration
    from numpy.distutils.system_info import get_info as get_system_info

    config = Configuration('special', parent_package, top_path)

    define_macros = []
    if sys.platform == 'win32':
        # define_macros.append(('NOINFINITIES',None))
        # define_macros.append(('NONANS',None))
        define_macros.append(('_USE_MATH_DEFINES',None))

    curdir = os.path.abspath(os.path.dirname(__file__))
    inc_dirs = [get_python_inc(), os.path.join(curdir, "c_misc")]
    if inc_dirs[0] != get_python_inc(plat_specific=1):
        inc_dirs.append(get_python_inc(plat_specific=1))
    inc_dirs.insert(0, get_numpy_include_dirs())

    # C libraries
    c_misc_src = [join('c_misc','*.c')]
    c_misc_hdr = [join('c_misc','*.h')]
    cephes_src = [join('cephes','*.c')]
    cephes_hdr = [join('cephes', '*.h')]
    config.add_library('sc_c_misc',sources=c_misc_src,
                       include_dirs=[curdir] + inc_dirs,
                       depends=(cephes_hdr + cephes_src
                                + c_misc_hdr + cephes_hdr
                                + ['*.h']),
                       macros=define_macros)
    config.add_library('sc_cephes',sources=cephes_src,
                       include_dirs=[curdir] + inc_dirs,
                       depends=(cephes_hdr + ['*.h']),
                       macros=define_macros)

    # Fortran/C++ libraries
    mach_src = [join('mach','*.f')]
    amos_src = [join('amos','*.f')]
    cdf_src = [join('cdflib','*.f')]
    specfun_src = [join('specfun','*.f')]
    config.add_library('sc_mach',sources=mach_src,
                       config_fc={'noopt':(__file__,1)})
    config.add_library('sc_amos',sources=amos_src)
    config.add_library('sc_cdf',sources=cdf_src)
    config.add_library('sc_specfun',sources=specfun_src)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    # Extension specfun
    config.add_extension('specfun',
                         sources=['specfun.pyf'],
                         f2py_options=['--no-wrap-functions'],
<<<<<<< HEAD
                         define_macros=[],
                         libraries=['sc_specfun'])

    # Extension orthogonal_eval
    config.add_extension('orthogonal_eval',
                         sources=['orthogonal_eval.c'],
                         define_macros=[],
                         extra_info=get_info("npymath"))

    # Extension lambertw
    config.add_extension('lambertw',
                         sources=['lambertw.c'],
                         define_macros=[],
                         extra_info=get_info("npymath"))

=======
                         depends=specfun_src,
                         define_macros=[],
                         libraries=['sc_specfun'])

    # Extension _ufuncs
    headers = ['*.h', join('c_misc', '*.h'), join('cephes', '*.h')]
    ufuncs_src = ['_ufuncs.c', 'sf_error.c', '_logit.c.src',
                  "amos_wrappers.c", "cdf_wrappers.c", "specfun_wrappers.c"]
    ufuncs_dep = (headers + ufuncs_src + amos_src + c_misc_src + cephes_src
                  + mach_src + cdf_src + specfun_src)
    cfg = dict(get_system_info('lapack_opt'))
    cfg.setdefault('include_dirs', []).extend([curdir] + inc_dirs + [numpy.get_include()])
    cfg.setdefault('libraries', []).extend(['sc_amos','sc_c_misc','sc_cephes','sc_mach',
                                            'sc_cdf', 'sc_specfun'])
    cfg.setdefault('define_macros', []).extend(define_macros)
    config.add_extension('_ufuncs',
                         depends=ufuncs_dep,
                         sources=ufuncs_src,
                         extra_info=get_info("npymath"),
                         **cfg)

    # Extension _ufuncs_cxx
    ufuncs_cxx_src = ['_ufuncs_cxx.cxx', 'sf_error.c',
                      '_faddeeva.cxx', 'Faddeeva.cc']
    ufuncs_cxx_dep = (headers + ufuncs_cxx_src + cephes_src
                      + ['*.hh'])
    config.add_extension('_ufuncs_cxx',
                         sources=ufuncs_cxx_src,
                         depends=ufuncs_cxx_dep,
                         include_dirs=[curdir],
                         define_macros=define_macros,
                         extra_info=get_info("npymath"))

    cfg = dict(get_system_info('lapack_opt'))
    config.add_extension('_ellip_harm_2',
                         sources=['_ellip_harm_2.c', 'sf_error.c',],
                         **cfg
                         )

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    config.add_data_files('tests/*.py')
    config.add_data_files('tests/data/README')
    config.add_data_files('tests/data/*.npz')

    return config

if __name__ == '__main__':
    from numpy.distutils.core import setup
    setup(**configuration(top_path='').todict())
