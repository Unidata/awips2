<<<<<<< HEAD
=======
from __future__ import absolute_import, print_function

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
import sys
sys.path.insert(0,'..')
import inline_tools


support_code = """
               PyObject* length(std::string a)
               {
                   int l = a.length();
                   return PyInt_FromLong(l);
               }
               """
<<<<<<< HEAD
a='some string'
val = inline_tools.inline("return_val = length(a);",['a'],
                          support_code=support_code)
print val
=======
a = 'some string'
val = inline_tools.inline("return_val = length(a);",['a'],
                          support_code=support_code)
print(val)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
