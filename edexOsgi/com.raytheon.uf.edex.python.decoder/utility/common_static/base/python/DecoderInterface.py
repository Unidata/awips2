##
##

#
# Interface for python decoders
#   
#
#    
# SOFTWARE HISTORY
#    
# Date          Ticket#  Engineer    Description
# ------------- -------- ----------- --------------------------
# Sep 22, 2008           njensen     Initial Creation.
# Oct 03, 2013  2402     bsteffen    Make PythonDecoder more extendable.
# Aug 04, 2014  3427     bclement    loadModule() now takes full path to jar
# Nov 21, 2016  5959     njensen     Made pythonic
#
#

##
# This is a base file that is not intended to be overridden.
##



import zipimport


def loadModule(jarpath, moduleName):

    if not sys.modules.has_key(moduleName):
        jar = zipimport.zipimporter(jarpath)
        jar.load_module(moduleName)

def decode(moduleName, **kwargs):
    mod = sys.modules[moduleName]
    exec 'dec = mod.' + moduleName + '(**kwargs)'
    result = dec.decode()
    resultList = []
    if result is not None:
        resultList = [resultDict for resultDict in result]
    return resultList
