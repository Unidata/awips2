<<<<<<< HEAD
=======
from __future__ import division, print_function, absolute_import

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
from subprocess import call, PIPE, Popen
import sys
import re

<<<<<<< HEAD
import numpy as np
from numpy.testing import TestCase, dec

from scipy.linalg import flapack

# XXX: this is copied from numpy trunk. Can be removed when we will depend on
# numpy 1.3
=======
from numpy.testing import TestCase, dec
from numpy.compat import asbytes

from scipy.linalg import _flapack as flapack

# XXX: this is copied from numpy trunk. Can be removed when we will depend on
# numpy 1.3


>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
class FindDependenciesLdd:
    def __init__(self):
        self.cmd = ['ldd']

        try:
            st = call(self.cmd, stdout=PIPE, stderr=PIPE)
        except OSError:
            raise RuntimeError("command %s cannot be run" % self.cmd)

    def get_dependencies(self, file):
        p = Popen(self.cmd + [file], stdout=PIPE, stderr=PIPE)
        stdout, stderr = p.communicate()
        if not (p.returncode == 0):
<<<<<<< HEAD
            raise RuntimeError("Failed to check dependencies for %s" % libfile)
=======
            raise RuntimeError("Failed to check dependencies for %s" % file)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

        return stdout

    def grep_dependencies(self, file, deps):
        stdout = self.get_dependencies(file)

<<<<<<< HEAD
        rdeps = dict([(dep, re.compile(dep)) for dep in deps])
=======
        rdeps = dict([(asbytes(dep), re.compile(asbytes(dep))) for dep in deps])
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        founds = []
        for l in stdout.splitlines():
            for k, v in rdeps.items():
                if v.search(l):
                    founds.append(k)

        return founds

<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
class TestF77Mismatch(TestCase):
    @dec.skipif(not(sys.platform[:5] == 'linux'),
                "Skipping fortran compiler mismatch on non Linux platform")
    def test_lapack(self):
        f = FindDependenciesLdd()
        deps = f.grep_dependencies(flapack.__file__,
                                   ['libg2c', 'libgfortran'])
<<<<<<< HEAD
        self.failIf(len(deps) > 1,
=======
        self.assertFalse(len(deps) > 1,
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
"""Both g77 and gfortran runtimes linked in scipy.linalg.flapack ! This is
likely to cause random crashes and wrong results. See numpy INSTALL.txt for
more information.""")
