#!/usr/bin/env python
# Created by Pearu Peterson, August 2002
<<<<<<< HEAD

from os.path import join

=======
from __future__ import division, print_function, absolute_import


from os.path import join


>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
def configuration(parent_package='',top_path=None):
    from numpy.distutils.misc_util import Configuration

    config = Configuration('fftpack',parent_package, top_path)

    config.add_data_dir('tests')
<<<<<<< HEAD
    config.add_data_dir('benchmarks')

    config.add_library('dfftpack',
                       sources=[join('src/dfftpack','*.f')])

    config.add_library('fftpack',
                       sources=[join('src/fftpack','*.f')])

    sources = ['fftpack.pyf','src/zfft.c','src/drfft.c','src/zrfft.c',
               'src/zfftnd.c', 'src/dct.c.src']
=======

    dfftpack_src = [join('src/dfftpack','*.f')]
    config.add_library('dfftpack', sources=dfftpack_src)

    fftpack_src = [join('src/fftpack','*.f')]
    config.add_library('fftpack', sources=fftpack_src)

    sources = ['fftpack.pyf','src/zfft.c','src/drfft.c','src/zrfft.c',
               'src/zfftnd.c', 'src/dct.c.src', 'src/dst.c.src']
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    config.add_extension('_fftpack',
        sources=sources,
        libraries=['dfftpack', 'fftpack'],
<<<<<<< HEAD
        include_dirs=['src'])
=======
        include_dirs=['src'],
        depends=(dfftpack_src + fftpack_src))
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    config.add_extension('convolve',
        sources=['convolve.pyf','src/convolve.c'],
        libraries=['dfftpack'],
<<<<<<< HEAD
=======
        depends=dfftpack_src,
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    )
    return config

if __name__ == '__main__':
    from numpy.distutils.core import setup
    from fftpack_version import fftpack_version
    setup(version=fftpack_version,
          description='fftpack - Discrete Fourier Transform package',
          author='Pearu Peterson',
<<<<<<< HEAD
          author_email = 'pearu@cens.ioc.ee',
          maintainer_email = 'scipy-dev@scipy.org',
          license = 'SciPy License (BSD Style)',
=======
          author_email='pearu@cens.ioc.ee',
          maintainer_email='scipy-dev@scipy.org',
          license='SciPy License (BSD Style)',
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
          **configuration(top_path='').todict())
