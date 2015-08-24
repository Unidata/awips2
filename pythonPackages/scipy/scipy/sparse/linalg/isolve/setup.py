#!/usr/bin/env python
<<<<<<< HEAD

import os
import sys
import re
from distutils.dep_util import newer_group, newer
from glob import glob
from os.path import join

def configuration(parent_package='',top_path=None):
    from numpy.distutils.system_info import get_info, NotFoundError

    from numpy.distutils.misc_util import Configuration
=======
from __future__ import division, print_function, absolute_import

from os.path import join


def configuration(parent_package='',top_path=None):
    from numpy.distutils.system_info import get_info, NotFoundError
    from numpy.distutils.misc_util import Configuration
    from scipy._build_utils import get_g77_abi_wrappers
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    config = Configuration('isolve',parent_package,top_path)

    lapack_opt = get_info('lapack_opt')

    if not lapack_opt:
<<<<<<< HEAD
        raise NotFoundError,'no lapack/blas resources found'
=======
        raise NotFoundError('no lapack/blas resources found')
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    # iterative methods
    methods = ['BiCGREVCOM.f.src',
               'BiCGSTABREVCOM.f.src',
               'CGREVCOM.f.src',
               'CGSREVCOM.f.src',
#               'ChebyREVCOM.f.src',
               'GMRESREVCOM.f.src',
#               'JacobiREVCOM.f.src',
               'QMRREVCOM.f.src',
#               'SORREVCOM.f.src'
               ]
<<<<<<< HEAD
    Util = ['STOPTEST2.f.src','getbreak.f.src']
    sources = Util + methods + ['_iterative.pyf.src']
    config.add_extension('_iterative',
                         sources = [join('iterative',x) for x in sources],
                         extra_info = lapack_opt
                         )
=======

    Util = ['STOPTEST2.f.src','getbreak.f.src']
    sources = Util + methods + ['_iterative.pyf.src']
    sources = [join('iterative', x) for x in sources]
    sources += get_g77_abi_wrappers(lapack_opt)

    config.add_extension('_iterative',
                         sources=sources,
                         extra_info=lapack_opt)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    config.add_data_dir('tests')

    return config

<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
if __name__ == '__main__':
    from numpy.distutils.core import setup

    setup(**configuration(top_path='').todict())
