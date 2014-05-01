import sys
import string

exportfile = 'Scientific_mpi.export'

exports = open(exportfile).readlines()
exports = map(lambda s: s[:-1], exports)
exports = filter(None, exports)

modulename = exports[0]
moduleimport = exports[1]
prefix = exports[2]
moduledef = exports[3]
exports = exports[4:]

header = open(prefix+'_API.h', 'w')

header.write("/*\n * C API functions\n */\n\n")

apicounter = 0
index = 0
while index < len(exports):

    if exports[index] == 'type':
        header.write("#define %s_Type_NUM %d\n\n"
                     % (exports[index+1], apicounter))
        apicounter = apicounter + 1
        index = index + 2

    elif exports[index] == 'function' or exports[index] == 'exportfunc':
        header.write("#define %s_RET %s\n"
                     % (exports[index+2], exports[index+1]))
        header.write("#define %s_PROTO Py_PROTO(%s)\n"
                     % (exports[index+2], exports[index+3]))
        header.write("#define %s_NUM %d\n\n"
                     % (exports[index+2], apicounter))
        apicounter = apicounter + 1
        index = index + 4

    elif exports[index] == 'typedef':
        index = index + 3
        
    else:
        raise ValueError, "unknown export object " + exports[index]

header.write("#define %s_API_pointers %d\n\n" % (prefix, apicounter))

header.write("#ifdef %s\n\n" % moduledef)

index = 0
while index < len(exports):

    if exports[index] == 'type':
        header.write("statichere PyTypeObject %s_Type;\n" % exports[index+1])
        header.write("#define %s_Check(op) ((op)->ob_type == &%s_Type)\n\n"
                     % (exports[index+1], exports[index+1]))
        index = index + 2

    elif exports[index] == 'function':
        header.write("static %s_RET %s %s_PROTO;\n\n"
                     % (exports[index+2], exports[index+2], exports[index+2]))
        index = index + 4

    elif exports[index] == 'exportfunc':
	index = index + 4
	
    elif exports[index] == 'typedef':
        index = index + 3
        
    else:
        raise ValueError, "unknown export object " + exports[index]

header.write("#define set_%s_API_pointers(){ \\\n" % prefix)

index = 0
while index < len(exports):

    if exports[index] == 'type':
        header.write("   %s_API[%s_Type_NUM] = (void *)&%s_Type; \\\n"
                     % (prefix, exports[index+1], exports[index+1]))
        index = index + 2

    elif exports[index] == 'function' or exports[index] == 'exportfunc':
        header.write("   %s_API[%s_NUM] = (void *)&%s; \\\n"
                     % (prefix, exports[index+2], exports[index+2]))
        index = index + 4

    elif exports[index] == 'typedef':
        index = index + 3
        
    else:
        raise ValueError, "unknown export object " + exports[index]

header.write("}\n\n")

header.write("#else\n\n")
linkage = "%s_API_LINKAGE" % (string.upper(prefix),)
header.write("#ifndef %s\n" % linkage)
header.write("#define %s static\n" % linkage)
header.write("#endif\n\n")
header.write("%s void **%s_API;\n\n" % (linkage, prefix))

index = 0
while index < len(exports):

    if exports[index] == 'type':
        header.write("#define %s_Check(op) \\\n" % exports[index+1])
        header.write("  ((op)->ob_type == (PyTypePbject *)")
        header.write("%s_API[%s_Type_Num])\n\n"
                     % (prefix, exports[index+1]))
        index = index + 2

    elif exports[index] == 'function' or exports[index] == 'exportfunc':
        header.write("#define %s \\\n" % exports[index+2])
        header.write("  (*(%s_RET (*)%s_PROTO) \\\n"
                     % (exports[index+2], exports[index+2]))
        header.write("   %s_API[%s_NUM])\n\n" % (prefix, exports[index+2]))
        index = index + 4

    elif exports[index] == 'typedef':
        header.write("typedef %s %s;\n\n" % (exports[index+1],
                                            exports[index+2]))
        index = index + 3
        
    else:
        raise ValueError, "unknown export object " + exports[index]

header.write('''\n
#define %s() \\
{ \\
  PyObject *module = PyImport_ImportModule("%s"); \\
  if (module != NULL) { \\
    PyObject *module_dict = PyModule_GetDict(module); \\
    PyObject *c_api_object = PyDict_GetItemString(module_dict, "_C_API"); \\
    if (PyCObject_Check(c_api_object)) { \\
      %s_API = (void **)PyCObject_AsVoidPtr(c_api_object); \\
    } \\
  } \\
}

''' %  (moduleimport, modulename, prefix))

header.write("#endif\n");

header.close()
