#!/usr/bin/env python
"""
A setup.py script to use setuptools, which gives egg goodness, etc.
"""

from setuptools import setup
<<<<<<< HEAD
execfile('setup.py')
=======
exec(compile(open('setup.py').read(), 'setup.py', 'exec'))
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
