#!/usr/bin/env python
<<<<<<< HEAD
=======
from __future__ import division, print_function, absolute_import

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

def configuration(parent_package='',top_path=None):
    from numpy.distutils.misc_util import Configuration
    config = Configuration('io', parent_package, top_path)

    config.add_data_dir('tests')
<<<<<<< HEAD
    config.add_data_dir('docs')
    config.add_subpackage('matlab')
    config.add_subpackage('arff')
=======
    config.add_subpackage('matlab')
    config.add_subpackage('arff')
    config.add_subpackage('harwell_boeing')
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    return config

if __name__ == '__main__':
    from numpy.distutils.core import setup
    setup(**configuration(top_path='').todict())
