#!/usr/bin/env python
<<<<<<< HEAD
=======
from __future__ import division, print_function, absolute_import
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

from os.path import join


def configuration(parent_package='',top_path=None):
    from numpy.distutils.misc_util import Configuration
    config = Configuration('stats', parent_package, top_path)

    config.add_data_dir('tests')

<<<<<<< HEAD
    config.add_library('statlib',
                       sources=[join('statlib', '*.f')])
=======
    statlib_src = [join('statlib', '*.f')]
    config.add_library('statlib', sources=statlib_src)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    # add statlib module
    config.add_extension('statlib',
        sources=['statlib.pyf'],
        f2py_options=['--no-wrap-functions'],
        libraries=['statlib'],
<<<<<<< HEAD
=======
        depends=statlib_src
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    )

    # add vonmises_cython module
    config.add_extension('vonmises_cython',
<<<<<<< HEAD
        sources=['vonmises_cython.c'], # FIXME: use cython source
=======
        sources=['vonmises_cython.c'],  # FIXME: use cython source
    )

    # add _rank module
    config.add_extension('_rank',
        sources=['_rank.c'],          # FIXME: use cython source
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    )

    # add futil module
    config.add_extension('futil',
        sources=['futil.f'],
    )

    # add mvn module
    config.add_extension('mvn',
        sources=['mvn.pyf','mvndst.f'],
    )

    return config

if __name__ == '__main__':
    from numpy.distutils.core import setup
    setup(**configuration(top_path='').todict())
