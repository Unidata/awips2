import Scientific_numerics_package_id
package = Scientific_numerics_package_id.getNumericsPackageName()
del Scientific_numerics_package_id

if package == "Numeric":

    import os
    import sys
    def pythonImport(name):
        current_path = os.path.dirname(os.path.abspath(__file__))
        base_name = os.path.basename(current_path).split('.')[0]
        sys.path[:] = [path for path in sys.path
                       if os.path.abspath(path) != os.path.abspath(current_path)]
        try:
            original_module = sys.modules[name]
            del sys.modules[name]
        except KeyError:
            original_module = None
        python_module = __import__(name)
        python_module_name = 'python_%s' % name
        sys.modules[python_module_name] = python_module
        sys.path.append(current_path)
        if original_module is None:
            sys.modules[name] = original_module
        return python_module_name, python_module
    pythonImport('FFT')
    del os
    del sys
    del pythonImport
    from python_FFT import *

elif package == "NumPy":

    from numpy.oldnumeric.fft import *

elif package == "Numarray":

    from numarray.fft import *

else:

    raise ImportError("Unknown numerics package " + package)
