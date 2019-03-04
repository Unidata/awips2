# #
# #

#
# Provides a method to dynamically load a python module into memory. The method
# was relocated to this module from LocalizationUtil. 
#   
#
#    
#    SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    11/11/13                      bkowal         Initial Creation.
#    01/20/14        2712          bkowal         Always add the directory path for
#                                                 the desired module to the beginning
#                                                 of the system path.
#
import os, sys, imp
    
def loadModule(filename):
    '''
    @param filename: the fully qualified name of the file
    @return: the module that was loaded
    @summary: This function takes a filename and find the module,
    loads it and returns that module
    '''
    
    path = os.path.splitext(filename)[0]
    directory = os.path.dirname(filename)
    # ensure the module containing directory is on the python path.
    sys.path.insert(0, directory)
    try:
        filename = os.path.split(path)[1]
        fp, pathname, description = imp.find_module(filename)
        module = imp.load_module(filename, fp, pathname, description)
    finally:
        sys.path.pop(0)
    
    return module