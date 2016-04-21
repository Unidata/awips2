##
# This software was developed and / or modified by Raytheon Company,
# pursuant to Contract DG133W-05-CQ-1067 with the US Government.
# 
# U.S. EXPORT CONTROLLED TECHNICAL DATA
# This software product contains export-restricted data whose
# export/transfer/disclosure is restricted by U.S. law. Dissemination
# to non-U.S. persons whether in the United States or abroad requires
# an export license or other authorization.
# 
# Contractor Name:        Raytheon Company
# Contractor Address:     6825 Pine Street, Suite 340
#                         Mail Stop B8
#                         Omaha, NE 68106
#                         402.291.0100
# 
# See the AWIPS II Master Rights File ("Master Rights File.pdf") for
# further licensing information.
##

#
# Python import hook (PEP 302) for importing Java classes through JEP
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    12/03/09                      njensen        Initial Creation.
#    06/12/14        3262          njensen        More precise java package names  
#    
# 
#


import sys, types
import jep

PACKAGES = [
            'java',
            'javax',
            'com',
            'gov',
            'org',
            'net',
            'edu',            
            ]

class JavaImporter:
    
    def __init__(self):
        pass
    
    #
    # Following PEP 302, this method checks if this importer applies to
    # this package/module.  When python goes to import, it will loop through
    # the importers of sys.meta_path in order and use the first one that
    # returns True to load the module.  If none return True, it falls back to
    # the built-in importer.
    #
    # For example, if a piece of python code has:
    #
    # from java.util import ArrayList
    #
    # that will pseudo-translate to:
    #
    # if JavaImporterInstance.find_module("java"):
    #    if JavaImporterInstance.find_module("java.util"):
    #       JavaImporterInstance.load_module("java.util.ArrayList")
    #
    # For a python module, this method should ideally always return False so
    # that python will fall back to its built-in importer.
    #    
    def find_module(self, fullname, path=None):                
        found = False        
        for pkg in PACKAGES:
            if fullname == pkg or fullname.startswith(pkg + "."):
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

