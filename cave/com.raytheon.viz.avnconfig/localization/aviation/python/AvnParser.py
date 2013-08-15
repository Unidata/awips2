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
#    Name:
#       AvnParser.py
#       GFS1-NHD:A7852.0000-SCRIPT;1.23
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 1.23 (DELIVERED)
#         Created:  30-JUN-2009 14:43:05      OBERFIEL
#           Modifications to allow collective PIL to be written and
#           read in TAF cfg files.
#       
#       Revision 1.22 (DELIVERED)
#         Created:  15-MAY-2009 15:05:37      OBERFIEL
#           Added afos id to dictionary returned by getTafHeaders
#           routine.
#       
#       Revision 1.21 (DELIVERED)
#         Created:  15-MAY-2008 15:10:56      GILMOREDM
#           Now parses QC section from site config file
#       
#       Revision 1.20 (DELIVERED)
#         Created:  17-MAR-2008 14:41:50      OBERFIEL
#           Remove routines trying to read in TWB configuration files.
#       
#       Revision 1.19 (DELIVERED)
#         Created:  04-MAY-2007 15:52:14      OBERFIEL
#           Changes to reflect OB8.2/AvnFPS3.5 values and clean up in
#           installation staging scripts. cvt3.py
#           updated to obsolete fields and add new one, tafduration.
#           AvnParser and DataRequestServ code cleaned up 
#           to remove references to avnmos, xtfs attributes and removed
#           obsolete modules.
#       
#       Revision 1.18 (INITIALIZE)
#         Created:  18-APR-2007 12:29:26      SOLSON
#           Took out the CR characters from the previous rev of this
#           file.
#       
#       Revision 1.17 (DELIVERED)
#         Created:  06-DEC-2006 14:10:40      BLI
#           Modified to make xmit configurable for each user
#       
#       Revision 1.16 (DELIVERED)
#         Created:  23-MAR-2006 15:14:13      TROJAN
#           spr 7109 - modified method to determine work PIL
#       
#       Revision 1.15 (DELIVERED)
#         Created:  22-MAR-2006 13:05:14      TROJAN
#           spr 7110. Revised TAF handling for consistency
#       
#       Revision 1.14 (DELIVERED)
#         Created:  31-JAN-2006 18:09:31      TROJAN
#           spr 7081
#       
#       Revision 1.13 (APPROVED)
#         Created:  23-JAN-2006 08:23:11      TROJAN
#           stdr 956
#       
#       Revision 1.12 (APPROVED)
#         Created:  30-NOV-2005 11:42:52      OBERFIEL
#           Updated code based on review
#       
#       Revision 1.11 (DELIVERED)
#         Created:  09-SEP-2005 19:05:47      TROJAN
#           spr 7012
#       
#       Revision 1.10 (DELIVERED)
#         Created:  06-JUL-2005 18:16:33      TROJAN
#           spr 6548
#       
#       Revision 1.9 (DELIVERED)
#         Created:  01-JUN-2005 17:40:58      OBERFIEL
#           Bug fixes since RHE3 initial snapshot
#       
#       Revision 1.8 (DELIVERED)
#         Created:  07-MAY-2005 11:29:59      OBERFIEL
#           Added Item Header Block
#       
#       Revision 1.7 (DELIVERED)
#         Created:  18-APR-2005 17:31:34      OBERFIEL
#           Changes to support gamin
#       
#       Revision 1.6 (DELIVERED)
#         Created:  02-APR-2005 17:02:15      TROJAN
#           spr 6763
#       
#       Revision 1.5 (DELIVERED)
#         Created:  24-JAN-2005 15:51:12      TROJAN
#           spr 6259
#       
#       Revision 1.4 (APPROVED)
#         Created:  30-SEP-2004 18:56:01      TROJAN
#           stdr 874
#       
#       Revision 1.3 (APPROVED)
#         Created:  19-AUG-2004 20:20:45      OBERFIEL
#           Fixed
#       
#       Revision 1.2 (REVIEW)
#         Created:  09-JUL-2004 19:28:38      OBERFIEL
#           Added climate QC
#       
#       Revision 1.1 (APPROVED)
#         Created:  01-JUL-2004 16:44:08      OBERFIEL
#           date and time created -2147483647/-2147483648/-2147481748
#           -2147483648:-2147483648:-2147483648 by oberfiel
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7417
#       	Action Date:       06-OCT-2009 09:42:01
#       	Relationship Type: In Response to
#       	Status:           CLOSED
#       	Title:             AvnFPS: TUG code does not handle transition from warm to cold seasons
#       
#
import logging, os
import ConfigParser
import Avn

_Logger = logging.getLogger(Avn.CATEGORY)

_ServerCfg = os.path.join('etc', 'server.cfg')
_GuiCfg = os.path.join('etc', 'gui.cfg')
_PlotCfg = os.path.join('etc', 'wxplot.cfg')
ForecasterFile = os.path.join('etc', 'forecasters')

def _guess(arg):
    '''Tries to guess and convert arg, passed as a string'''
    if type(arg) != type(''):
        return
    try:
        return int(arg)
    except ValueError:
        try:
            return float(arg)
        except ValueError:
            return arg

def _getProducts(direct):
    # Returns *.cfg file names with .cfg stripped
    # Default is first on the list
    try:
        plist = [f.split('.')[0] for f in os.listdir(direct) if \
            f.endswith('.cfg')]
        plist.sort()
        default = file(os.path.join(direct, 'DEFAULT')).read().rstrip()
        plist.remove(default)
        return [default] + plist
    except (IOError, ValueError):
        return plist
    except OSError:
        _Logger.exception('Error accessing products in %s', direct)
        return []

def _tafProdFilename(product):
    #return os.path.join('etc', 'tafs', product)+'.cfg'
    direct = Avn.PATH_MGR.getStaticFile(os.path.join(Avn.ConfigDir, 'tafs'))
    return os.path.join(direct.getPath(), product) +'.cfg'

###############################################################################
def split(strng, sep=','):
    '''Splits strng into sep (default: a comma) delineated chunks.
Strips leading and trailing blanks'''
    return filter(None, [s.strip() for s in strng.split(sep)])

def getClimQCConfig(ident):
    # file containing category definitions: thresholds
#    fname = os.path.join('etc', 'tafs', ident, 'climqc.cfg')
#    if not os.path.isfile(fname):
#        fname = os.path.join('etc', 'tafs', 'XXXX', 'climqc.cfg')
    fname = Avn.getTafPath(ident, 'climqc.cfg')    
    cp = ConfigParser.SafeConfigParser()
    cp.read(fname)
    d = dict([(t, [_guess(x) for x in cp.get('thresholds', t).split(',')]) \
        for t in ['vsby', 'cig', 'ff', 'dd']])
    opts = dict(cp.items('args'))
    for k in opts:
        opts[k] = _guess(opts[k])
    d.update(opts)
    return d

def getServerCfg():
    try:
        if not os.path.isfile(_ServerCfg):
            raise Avn.AvnError('File %s does not exist' % _ServerCfg)
        cp = ConfigParser.RawConfigParser()
        cp.read(_ServerCfg)
        d = {}
        tags = split(cp.get('dis', 'tags'))
        d['dis'] = [dict(cp.items('dis_'+tag)) for tag in tags]     
        # access control
        d['valid'] = split(cp.get('valid', 'hosts'))
        return d
    except Exception:
        msg = 'Cannot process %s' % _ServerCfg
        _Logger.exception(msg)
        return None

def getGuiCfg():
    try:
        if not os.path.isfile(_GuiCfg):
            raise Avn.AvnError('File %s does not exist' % _GuiCfg)
        cp = ConfigParser.RawConfigParser()
        cp.read(_GuiCfg)
        d = {'features': {}}
        # miscellaneous
        for opt in cp.options('features'):
            d['features'][opt] = cp.getboolean('features', opt)
        # servers
        d['ns'] = split(cp.get('ns', 'tags'))
        # colors
        d['colors'] = split(cp.get('colors', 'tags'))
        # editor tags
        d['edittags'] = dict(cp.items('editor_tags'))
        # monitors
        n = int(cp.get('menus', 'number'))
        d['menus'] = [split(cp.get('menu_'+str(k), 'items')) for k in range(n)]
        allitems = {}
        for items in d['menus']:
            allitems.update(dict.fromkeys(items))
        d['monitors'] = {}
        for tag in allitems.keys():
            opts = dict(cp.items('monitor_'+tag))
            for k in opts:
                if k in ['items', 'labels']: 
                    opts[k] = filter(None, [x.strip() for x in \
                        opts[k].split(',')])
                else:
                    opts[k] = _guess(opts[k])
            opts['labels'] = dict(zip(opts['items'], opts['labels']))
            d['monitors'][tag] = opts
        # make label dictionary from list
        # viewers - used in the editor
        d['viewers'] = []
        for tag in split(cp.get('viewers', 'tags')):
            opts = dict(cp.items('viewer_'+tag))
            m = __import__(opts['module']); del opts['module']
            d['viewers'].append(Avn.Bunch(tag=tag, module=m, args=opts))
        return d
    except Exception:
        msg = 'Cannot process %s' % _GuiCfg
        _Logger.exception(msg)
        return None

def getPlotCfg():
    d = {'hours': {}, 'cig': {}, 'vsby': {}, 'viewers': []}
    try:
        if not os.path.isfile(_PlotCfg):
            raise Avn.AvnError('File %s does not exist' % _PlotCfg)
        cp = ConfigParser.RawConfigParser()
        cp.read(_PlotCfg)
        for option in cp.options('vsby'):
            d['vsby'][option] = cp.getfloat('vsby', option)
        for option in cp.options('cig'):
            d['cig'][option] = cp.getint('cig', option)
        for option in cp.options('hours'):
            d['hours'][option] = cp.getint('hours', option)
        for tag in split(cp.get('viewers', 'tags')):
            opts = dict(cp.items('viewer_'+tag))
            m = __import__(opts['module']); del opts['module']
            l = opts['label']; del opts['label']
            d['viewers'].append(Avn.Bunch(tag=tag, label=l, module=m, 
                args=opts))
        d['selected'] = split(cp.get('selected', 'tags'))
        d['printcmd'] = cp.get('print', 'cmd')
        return d
    except Exception:
#       Avn.printExcPlus()
        msg = 'Cannot process %s' % _PlotCfg
        _Logger.exception(msg)
        return None

def getTafProductCfg(product):
    try:
        fname = _tafProdFilename(product)
        if not os.path.isfile(fname):
            raise Avn.AvnError('File %s does not exist' % fname)
        cp = ConfigParser.RawConfigParser()
        cp.read(fname)
        d = {'sites': split(cp.get('sites', 'idents'))}
        if cp.has_option('sites', 'workpil'):
            d['workpil'] = cp.get('sites', 'workpil')
        else:
            d['workpil'] = Avn.TAFWorkPIL
        if cp.has_option('sites', 'collective'):
            d['collective'] = cp.get('sites', 'collective')
        return d
    except Exception, ex:
        msg = 'Cannot get configuration for %s: %s' % (product, str(ex))
        _Logger.exception(msg)
        return None

def getTafSiteCfg(ident):
    try:
#        fname = os.path.join('etc', 'tafs', ident, 'info.cfg')
        fname = os.path.join(Avn.ConfigDir, 'tafs', ident, 'info.cfg')
        f = Avn.PATH_MGR.getStaticFile(fname)
        if f:
            fname = f.getPath()
        if not os.path.isfile(fname):
            raise Avn.AvnError('File %s does not exist' % fname)
        cp = ConfigParser.RawConfigParser()
        cp.read(fname)
        d = {'ident': ident}
        d['headers'] = dict(cp.items('headers'))
        d['geography'] = dict(cp.items('geography'))
        d['geography']['runway'] = [int(x) for x in \
                                    split(d['geography']['runway'])]
        d['sites'] = {}
        for key, value in cp.items('sites'):
            d['sites'][key] = split(value)
        d['thresholds'] = {}
        vsby = [float(x) for x in split(cp.get('thresholds', 'vsby'))]
        d['thresholds']['vsby'] = vsby
        cig = [int(x) for x in split(cp.get('thresholds', 'cig'))]
        d['thresholds']['cig'] = cig
        if 'radar_cutoff' in cp.options('thresholds'):
            cutoffs = [int(x) for x in split(cp.get('thresholds', 
                                                    'radar_cutoff'))]
        else:
            cutoffs = [0]*len(d['sites'].get('radars', []))
        d['thresholds']['radar_cutoff'] = cutoffs
        if 'profiler_cutoff' in cp.options('thresholds'):
            cutoffs = [int(x) for x in split(cp.get('thresholds', 
                                                    'profiler_cutoff'))]
        else:
            cutoffs = [0]*len(d['sites'].get('profilers', []))
       
        d['thresholds']['profiler_cutoff'] = cutoffs

        d['thresholds']['tafduration'] = cp.get('thresholds','tafduration')
        d['qc'] = {'currentwx':1, 'climate':1, 'impact':1}

	if 'qc' in cp.sections():
	    for x in ['currentwx','impact','climate']:
		if x in cp.options('qc'): d['qc'][x] = int(cp.get('qc',x))
            
        return d
    
    except Exception:
        msg = 'Cannot process %s' % fname
        _Logger.exception(msg)
        return None

def getTafTemplate(ident, hour):
    return file(os.path.join('etc', 'tafs', ident, 
        '%02d.template' % hour)).read()

def getTafProducts():
    direct = Avn.PATH_MGR.getStaticFile(os.path.join(Avn.ConfigDir, 'tafs'))
    #return _getProducts(os.path.join('etc', 'tafs'))
    return _getProducts(direct.getPath())

def getForecasters():
    # Returns list of forecasters as a dictionary (name, id)
    d = {}
    for line in file(ForecasterFile):
        try:
            if line[0] == '#':
                continue
            ident, xmit, name = line.split(None, 2)
            d[name.rstrip()] = {'id':int(ident),'xmit':int(xmit)}
        except (IndexError, ValueError):
            pass
    return d

def getTafHeaders():
    # Returns dictionary of awips and wmo headers for all TAF sites
    tmp = []
    plist = getTafProducts()
    for p in plist:
        sitelist = getTafProductCfg(p)['sites']
        if sitelist is not None:
            tmp.extend(sitelist)
    d = dict.fromkeys(tmp)
    for ident in d.keys():
        siteCfg = getTafSiteCfg(ident)
        if siteCfg is None:
            del d[ident]
            continue
        header = siteCfg['headers']
        awipsid = header['wmo'].split()[1] + header['afos'][3:]
        d[ident] = {'wmo': header['wmo'], 'awips': awipsid, 'afos': header['afos']}

    return d

def getAllSiteIds():
    iddict = {}
    # read all TAF configuration files
    tafids = {}
    for prod in getTafProducts():
        ids = getTafProductCfg(prod)['sites']
        if ids:
            tafids.update(dict.fromkeys(ids))
    iddict['taf'] = {}
    for ident in tafids:
        cfg = getTafSiteCfg(ident)
        if cfg is None:
            continue
        iddict['taf'][ident] = None
        for tag in cfg['sites']:
            if tag not in iddict:
                iddict[tag] = {}
            i = cfg['sites'][tag]
            if type(i) == type([]):
                for item in i:
                    iddict[tag][item] = None
            else:
                iddict[tag][i] = None

    # create sorted lists
    retids = dict.fromkeys(iddict.keys())
    for key in retids:
        tmp = iddict[key].keys()
        tmp.sort()
        retids[key] = tmp
    return retids
