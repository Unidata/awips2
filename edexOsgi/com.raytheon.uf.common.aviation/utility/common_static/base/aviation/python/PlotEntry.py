##
##

##
# This is a base file that is not intended to be overridden.
##

import logging
import cPickle as pickle
import Avn, AvnParser, AvnLib
import TafDecoder
import JUtil
import MetarData, EtaData, MosData

_Logger = logging.getLogger(Avn.CATEGORY)

#
# Entry point for Weather Plot data retrieval
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    08/24/09                      avarani        Initial Creation.
#    04/28/11        8065          rferrel        Use cached site objects
#    
#

def getNam(siteObj):
    model = 'etabuf'
    o = pickle.loads(siteObj)
    siteID = o['siteID']
    etaData = o['data']
#    print 'PlotEntry.getNam: model, siteID:', model, siteID
    if etaData is not None:
        data = [{'data' : eta.data} for eta in etaData]
    else:
        data = None
    return JUtil.pyValToJavaObj(data)

def getMos(siteObj, model):
    o = pickle.loads(siteObj)
    siteID = o['siteID']
    mosData = o['data']
#    print 'PlotEntry.getMos: model, siteID:', model, siteID
    if mosData is not None:
        data = [{'data' : mos.data} for mos in mosData]
    else:
        data = None
    return JUtil.pyValToJavaObj(data)

def getMetars(siteObj, size=99):
    o = pickle.loads(siteObj)
    siteID = o['siteID']
    data = o['data']
#    print 'PlotEntry.getMetars siteID, size:', siteID, size
    if data is not None:
        data = [{'header' : d.header, 'text' : d.text, 'dcd' : d.dcd} for d in data]
        data.sort(lambda x, y: cmp(y['dcd']['itime']['str'], x['dcd']['itime']['str']))
    return JUtil.pyValToJavaObj(data)
    
    
def decodeTaf(taf, wmoHeader):
#    print 'plotEntry.decodeTaf: taf<%s>,\nwmoHeader<%s>:' % ( taf, wmoHeader)
    decoder = TafDecoder.Decoder()    
    try:            
        bbb = wmoHeader.split()[3]
    except IndexError:
        bbb = '   '
    dcd = decoder(taf, bbb)
    tafDict = {'header': wmoHeader, 'text': taf, 'dcd': dcd}
    return JUtil.pyValToJavaObj(tafDict)