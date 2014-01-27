#
# GempakGridDiagnostic
#   
# This code has been developed by the SIB for use in the AWIPS2 system.
# Performs a calculation of a grid diagnostic using GEMPAK so, and returns
# the data.
#
#     Usage:
#    import GempakGridDiagnostic
#    gd = GempakGridDiagnostic.GempakGridDiagnostic()
#    gd.setGlevel('750')
#    gd.setGdattim('2009-07-01_13:00:00.0_(5)')
#    gd.setGdfile('ruc130')
#    gd.setGvect('wnd')  or gd.setGfunc('urel')
#    gd.setGvcord('PRES')
#    gd.setGarea('ks')
#    gd.setScale('0')
#    gd.setProj('mer')
#    return gd.execute()
#
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer             Description
#    ------------    ----------    -----------          --------------------------
#    04/10           173_partC     mgamazaychikov       Initial Creation.
#    05/10           173_partC     T. Lee               Split setParm method to separate methods,
#                                                       renamed the module.
#    06/10           173_partC     mgamazaychikov       Added documentation
#

from com.raytheon.uf.common.message.response import ResponseMessageGeneric
from gov.noaa.nws.ncep.edex.uengine.tasks.gempak import Dgdriv
from gov.noaa.nws.ncep.edex.uengine.utility import GempakConvert

class GempakGridDiagnostic():

    def __init__(self):
        self.Dgdriv = Dgdriv()

    def setGlevel(self, glevel):
        self.Dgdriv.setGlevel(glevel)

    def setGdattim(self, aDbTime):
        convert = GempakConvert()
        aGdattim = convert.dbtimeToDattim(aDbTime.replace('_', ' '))
        #print aGdattim
        self.Dgdriv.setGdattim (aGdattim)

    def setGdfile(self, gdfile):
        self.Dgdriv.setGdfile(gdfile)

    def setGfunc(self, gfunc):
        self.Dgdriv.setGfunc(gfunc)
        
    def setGvect(self, gfunc):
        self.Dgdriv.setGvect(gfunc)

    def setGvcord(self, gvcord):
        self.Dgdriv.setGvcord(gvcord)

    def setGarea(self, garea):
        self.Dgdriv.setGarea(garea)

    def setScale(self, scale):
        self.Dgdriv.setScale(scale)

    def setProj(self, proj):
        self.Dgdriv.setProj(proj)

    def makeResponse(self):
        return ResponseMessageGeneric(self.result)

    def makeNullResponse(self):
        return ResponseMessageGeneric("No grid data is available")

    def execute(self):
        self.result = self.Dgdriv.execute()
        if self.result is None:
            return self.makeNullResponse()
        else:
            return self.makeResponse()