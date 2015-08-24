#!/usr/bin/env python
<<<<<<< HEAD

from os.path import join

def configuration(parent_package = '', top_path = None):
    from numpy.distutils.misc_util import Configuration, get_numpy_include_dirs
    config = Configuration('cluster', parent_package, top_path)

    config.add_data_dir('tests')

    config.add_extension('_vq',
        sources=[join('src', 'vq_module.c'), join('src', 'vq.c')],
        include_dirs = [get_numpy_include_dirs()])

    config.add_extension('_hierarchy_wrap',
        sources=[join('src', 'hierarchy_wrap.c'), join('src', 'hierarchy.c')],
        include_dirs = [get_numpy_include_dirs()])
=======
from __future__ import division, print_function, absolute_import

import sys

if sys.version_info[0] >= 3:
    DEFINE_MACROS = [("SCIPY_PY3K", None)]
else:
    DEFINE_MACROS = []


def configuration(parent_package='', top_path=None):
    from numpy.distutils.system_info import get_info
    from numpy.distutils.misc_util import Configuration, get_numpy_include_dirs
    config = Configuration('cluster', parent_package, top_path)

    blas_opt = get_info('lapack_opt')

    config.add_data_dir('tests')

    config.add_extension('_vq',
        sources=[('_vq.c')],
        include_dirs=[get_numpy_include_dirs()],
        extra_info=blas_opt)

    config.add_extension('_hierarchy',
        sources=[('_hierarchy.c')],
        include_dirs=[get_numpy_include_dirs()])
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    return config

if __name__ == '__main__':
    from numpy.distutils.core import setup
<<<<<<< HEAD
    setup(maintainer = "SciPy Developers",
          author = "Eric Jones",
          maintainer_email = "scipy-dev@scipy.org",
          description = "Clustering Algorithms (Information Theory)",
          url = "http://www.scipy.org",
          license = "SciPy License (BSD Style)",
=======
    setup(maintainer="SciPy Developers",
          author="Eric Jones",
          maintainer_email="scipy-dev@scipy.org",
          description="Clustering Algorithms (Information Theory)",
          url="http://www.scipy.org",
          license="SciPy License (BSD Style)",
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
          **configuration(top_path='').todict()
          )
