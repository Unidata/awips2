#!/usr/bin/env python
<<<<<<< HEAD

from os.path import join

def configuration(parent_package='',top_path=None):
    from numpy.distutils.misc_util import Configuration

    config = Configuration('interpolate', parent_package, top_path)

    config.add_library('fitpack',
                       sources=[join('fitpack', '*.f')],
                      )
=======
from __future__ import division, print_function, absolute_import

from os.path import join


def configuration(parent_package='',top_path=None):
    from numpy.distutils.misc_util import Configuration
    from numpy.distutils.system_info import get_info

    lapack_opt = get_info('lapack_opt', notfound_action=2)

    config = Configuration('interpolate', parent_package, top_path)

    fitpack_src = [join('fitpack', '*.f')]
    config.add_library('fitpack', sources=fitpack_src)

    config.add_extension('interpnd',
                         sources=['interpnd.c'])

    config.add_extension('_ppoly',
                         sources=['_ppoly.c'],
                         **lapack_opt)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    config.add_extension('_fitpack',
                         sources=['src/_fitpackmodule.c'],
                         libraries=['fitpack'],
<<<<<<< HEAD
                         depends = ['src/__fitpack.h','src/multipack.h']
                        )
=======
                         depends=(['src/__fitpack.h','src/multipack.h']
                                  + fitpack_src)
                         )
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    config.add_extension('dfitpack',
                         sources=['src/fitpack.pyf'],
                         libraries=['fitpack'],
<<<<<<< HEAD
                        )

    config.add_extension('_interpolate',
                         sources=['src/_interpolate.cpp'],
                         include_dirs = ['src'],
                         depends = ['src/interpolate.h'])
=======
                         depends=fitpack_src,
                         )

    config.add_extension('_interpolate',
                         sources=['src/_interpolate.cpp'],
                         include_dirs=['src'],
                         depends=['src/interpolate.h'])
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    config.add_data_dir('tests')

    return config

if __name__ == '__main__':
    from numpy.distutils.core import setup
    setup(**configuration(top_path='').todict())
