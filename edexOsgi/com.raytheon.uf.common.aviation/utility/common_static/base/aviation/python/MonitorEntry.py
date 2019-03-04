##
##

import logging
import Avn, AvnParser, AvnLib
import TafDecoder
import JUtil

_Logger = logging.getLogger(Avn.CATEGORY)
_DecoderCache = { 'value' : Avn.Bunch(header=None, text=None, dcd=None) }

#
# Entry point for Java on monitoring
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    08/24/09                      njensen       Initial Creation.
#    
# 
#

##
# This is a base file that is not intended to be overridden.
##

def monitor(request):
    taf = decodeTaf(request.getTaf(), request.getWmoHeader())
    if taf.dcd.has_key('fatal'):
        _Logger.error(taf.dcd['fatal'])
        return JUtil.pyValToJavaObj({'fatal': taf.dcd['fatal']})
        
    monitorModuleName = request.getCfg().getClassName()
    monitorModule = __import__(monitorModuleName)
    info = AvnParser.getTafSiteCfg(request.getSiteID())
    m = transformCfg(monitorModule, request)
    monitor = monitorModule.Monitor(info, m)
    result = {'ident': info['ident'], 'taf': taf, \
            'newtaf': False, 'status': {}}
    result['taf'].hourly = AvnLib.TafData( \
                        result['taf'].dcd['group'])
    result['status'] = monitor.compare(result['taf'])
    result = bunchToDict(result)
    if result['taf'].has_key('hourly'):
        hourly = result['taf']['hourly']        
        result['taf']['hourly'] = None
    return JUtil.pyValToJavaObj(result)

def bunchToDict(d):
    for i in d:
        if type(d[i]) is Avn.Bunch:
            d[i] = d[i].__dict__
        if type(d[i]) is dict:
            d[i] = bunchToDict(d[i])
    return d

def transformCfg(module, req):
    from com.raytheon.uf.common.util import StringUtil
    m = {'module':module}
    cfg = req.getCfg()
    it = StringUtil.split(cfg.getMonitorItems(), ",")
    lb = StringUtil.split(cfg.getMonitorLabels(), ",")
    items = []
    labels = {}    
    for i in range(len(it)):
        item = it[i]
        items.append(item)
        labels[item] = lb[i]
    m['labels'] = labels
    m['items'] = items
    if req.getArgs():
        args = req.getArgs()
        itr = args.keySet().iterator()
        while itr.hasNext():
            key = itr.next()
            val = int(str(args.get(key)))
            m[str(key)] = val
        
    return m

def decodeTaf(taf, wmoHeader):
    if taf != _DecoderCache['value'].text \
       or wmoHeader !=  _DecoderCache['value'].header :
        try:            
            bbb = wmoHeader.split()[3]
        except IndexError:
            bbb = '   '
        decoder = TafDecoder.Decoder()
        dcd = decoder(taf, bbb)
        _DecoderCache['value'] = Avn.Bunch(header=wmoHeader, text=taf, dcd=dcd)
    
    return _DecoderCache['value']    
    
            
    
