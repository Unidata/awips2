<<<<<<< HEAD
def configuration(parent_package='',top_path=None):
    from numpy.distutils.misc_util import Configuration
    config = Configuration('constants',parent_package,top_path)

    config.add_subpackage('*')

    return config

=======
from __future__ import division, print_function, absolute_import


def configuration(parent_package='', top_path=None):
    from numpy.distutils.misc_util import Configuration
    config = Configuration('constants', parent_package, top_path)
    config.add_data_dir('tests')
    return config


>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
if __name__ == '__main__':
    from numpy.distutils.core import setup
    setup(**configuration(top_path='').todict())
