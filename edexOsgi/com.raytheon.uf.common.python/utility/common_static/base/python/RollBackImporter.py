import sys, __builtin__, LogStream, re

class RollBackImporter:
    def __init__(self):
        "Creates an instance and installs as the global importer"
        self.previousModules = sys.modules.copy()
        self.realImport = __builtin__.__import__
        __builtin__.__import__ = self._import
        self.newModules = {}
        self.pattern = re.compile("/.*?/edex/data/utility/.*?/(.*?)/")

    def _import(self, name, globals=None, locals=None, fromlist=[], level=-1):
        result = self.realImport(name, globals, locals, fromlist, level)
        
        if hasattr(result, '__file__'):
            match = re.match(self.pattern, result.__file__)
            if match is not None:
                level = match.group(1)
                if level != 'base':
#                    LogStream.logDebug("IMPORTING:", name, result)
                    self.newModules[result.__name__] = 1
#                else:
#                    LogStream.logDebug("IGNORING BASE:", name, result)
#            else:
#                LogStream.logDebug("IGNORING NON-LOCALIZED:", name, result)
#        else:
#            LogStream.logDebug("IGNORING BUILTIN:", name, result)
        return result

    def rollback(self):
        for modname in self.newModules.keys():
            if not self.previousModules.has_key(modname):
                # Force reload when modname next imported
#                LogStream.logDebug("UNLOADING:", modname, sys.modules[modname])
                del(sys.modules[modname])
#            else:
#                LogStream.logDebug("SKIPPING PRELOADED:", modname)

        self.newModules = {}
                
