##############################################################################
# COPYRIGHT (c), 2009, RAYTHEON COMPANY
# ALL RIGHTS RESERVED, An Unpublished Work 
# 
# RAYTHEON PROPRIETARY
# If the end user is not the U.S. Government or any agency thereof, use
# or disclosure of data contained in this source code file is subject to
# the proprietary restrictions set forth in the Master Rights File.
# 
#  U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
# If the end user is the U.S. Government or any agency thereof, this source
# code is provided to the U.S. Government with Government Purpose Rights.
# Use or disclosure of data contained in this source code file is subject to
# the "Government Purpose Rights" restriction in the Master Rights File.
# 
#  U.S. EXPORT CONTROLLED TECHNICAL DATA
#  Use or disclosure of data contained in this source code file is subject to
#  the export restrictions set forth in the Master Rights File.
##############################################################################

#
# Python import hook (PEP 302) for importing Java classes through JEP
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    12/03/09                      njensen       Initial Creation.
#    
# 
#


import sys, types
import jep

PACKAGES = [
            'java',
            'com',
            'gov',
            'org',
            'net',
            'edu',            
            ]

class JavaImporter:
    
    def __init__(self):
        pass
    
    def find_module(self, fullname, path=None):                
        found = False
        for p in PACKAGES:
            if fullname.startswith(p):
                found = True
                break
        if not found:
            return None
        return self 
        
    
    def load_module(self, fullname):
        split = fullname.split('.')
        parentModule = None        
        size = len(split)
        for i in range(size):
            s = split[i]
            if not parentModule:
                if sys.modules.has_key(s):
                    parentModule = sys.modules[s]
                else:
                    mod = sys.modules.setdefault(s, JavaModule(s))
                    mod.__loader__ = self
                    mod = sys.modules[s]
                    parentModule = mod
            else:                        
                attr = None
                try:
                    attr = parentModule.__getattribute__(s)
                except AttributeError:
                    pass 
                if attr:
                    parentModule = attr
                else:
                    if s[0].islower(): # it's still part of the package name
                        mod = JavaModule(s)
                        mod.__loader__ = self
                        parentModule.__setattr__(s, mod)
                        parentModule = mod
                    else: # it's a java class, add it           
                        jclz = jep.jimport(fullname)
                        parentModule.__setattr__(s, jclz)
                        parentModule = jclz
                        
        return parentModule                        

class JavaModule(types.ModuleType):
    
    def __getattribute__(self, attr):   
        if attr == '__path__':
           return 'java'
        else:
            return types.ModuleType.__getattribute__(self, attr)    
        

sys.modules['JavaImporter'] = sys.modules[__name__]
sys.meta_path[:] = [JavaImporter()]

