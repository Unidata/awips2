#!/usr/bin/env python
<<<<<<< HEAD
=======
from __future__ import division, print_function, absolute_import

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

def configuration(parent_package='io',top_path=None):
    from numpy.distutils.misc_util import Configuration
    config = Configuration('matlab', parent_package, top_path)
    config.add_extension('streams', sources=['streams.c'])
    config.add_extension('mio_utils', sources=['mio_utils.c'])
    config.add_extension('mio5_utils', sources=['mio5_utils.c'])
    config.add_data_dir('tests')
<<<<<<< HEAD
    config.add_data_dir('benchmarks')
=======
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    return config

if __name__ == '__main__':
    from numpy.distutils.core import setup
    setup(**configuration(top_path='').todict())
