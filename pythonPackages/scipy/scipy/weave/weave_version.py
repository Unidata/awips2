<<<<<<< HEAD
=======
from __future__ import absolute_import, print_function

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
major = 0
minor = 4
micro = 9
#release_level = 'alpha'
release_level = ''

if release_level:
    weave_version = '%(major)d.%(minor)d.%(micro)d_%(release_level)s'\
<<<<<<< HEAD
                    % (locals ())
else:
    weave_version = '%(major)d.%(minor)d.%(micro)d'\
                    % (locals ())
=======
                    % (locals())
else:
    weave_version = '%(major)d.%(minor)d.%(micro)d'\
                    % (locals())
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
