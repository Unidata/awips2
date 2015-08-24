#!/usr/bin/env python
<<<<<<< HEAD
from os.path import join, dirname
import sys
import os
=======
from __future__ import division, print_function, absolute_import

from os.path import join, dirname
import sys
import os
import glob

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

def configuration(parent_package='',top_path=None):
    from numpy.distutils.misc_util import Configuration
    from numpy.distutils.system_info import get_info
<<<<<<< HEAD
=======
    from scipy._build_utils import get_sgemv_fix
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    config = Configuration('dsolve',parent_package,top_path)
    config.add_data_dir('tests')

    lapack_opt = get_info('lapack_opt',notfound_action=2)
<<<<<<< HEAD
    if sys.platform=='win32':
=======
    if sys.platform == 'win32':
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        superlu_defs = [('NO_TIMER',1)]
    else:
        superlu_defs = []
    superlu_defs.append(('USE_VENDOR_BLAS',1))

<<<<<<< HEAD
    superlu_src = os.path.join(dirname(__file__), 'SuperLU', 'SRC')

    config.add_library('superlu_src',
                       sources = [join(superlu_src,'*.c')],
                       macros = superlu_defs,
=======
    superlu_src = join(dirname(__file__), 'SuperLU', 'SRC')

    sources = list(glob.glob(join(superlu_src, '*.c')))
    headers = list(glob.glob(join(superlu_src, '*.h')))
    if os.name == 'nt' and ('FPATH' in os.environ or 'MKLROOT' in os.environ):
        # when using MSVC + MKL, lsame is already in MKL
        sources.remove(join(superlu_src, 'lsame.c'))

    config.add_library('superlu_src',
                       sources=sources,
                       macros=superlu_defs,
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
                       include_dirs=[superlu_src],
                       )

    # Extension
<<<<<<< HEAD
    config.add_extension('_superlu',
                         sources = ['_superlumodule.c',
                                    '_superlu_utils.c',
                                    '_superluobject.c'],
                         libraries = ['superlu_src'],
                         extra_info = lapack_opt,
                         )

    config.add_subpackage('umfpack')

=======
    ext_sources = ['_superlumodule.c',
                   '_superlu_utils.c',
                   '_superluobject.c']
    ext_sources += get_sgemv_fix(lapack_opt)

    config.add_extension('_superlu',
                         sources=ext_sources,
                         libraries=['superlu_src'],
                         depends=(sources + headers),
                         extra_info=lapack_opt,
                         )

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    return config

if __name__ == '__main__':
    from numpy.distutils.core import setup
    setup(**configuration(top_path='').todict())
