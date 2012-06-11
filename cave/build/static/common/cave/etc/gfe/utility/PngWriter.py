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
import string, LogStream, getopt, sys, os, time
import time, TimeRange, AbsTime
import GFEPainter
import loadConfig
from operator import attrgetter
from com.raytheon.uf.common.time import DataTime
from com.raytheon.uf.viz.core import RGBColors
from com.raytheon.viz.gfe.core.parm import ParmDisplayAttributes_VisMode as VisMode

class PngWriter:
    def __init__(self, conf="testIFPImage", userName="", baseTime=None,
      timeRange=None, usrTimeRange=None):
        from com.raytheon.viz.gfe.core import DataManager
        from com.raytheon.uf.viz.core.localization import LocalizationManager
        self.site = LocalizationManager.getInstance().getCurrentSite()
        
        self._topo = 0
                
        # import the config file
        self.config = __import__(conf)
        loadConfig.loadPreferences(self.config)
        
        self.baseTime = baseTime

        self.dm = DataManager.getInstance(None)

        LogStream.logEvent("Configuration File: ", conf)

        self.pgons = None
        self.imgParm = None
        self.ipn = self.getConfig('Png_image', '')
        print "ipn:",self.ipn

        # user named time range specified?
        if usrTimeRange is not None:
            s_tr = self.dm.getSelectTimeRangeManager().getRange(usrTimeRange)
            if s_tr is None:
                LogStream.logProblem(usrTimeRange, \
                  " is not a valid time range name.")
                sys.exit(1)
            else:
                tr = TimeRange.TimeRange(s_tr.toTimeRange())
                self.pngTimeRange = tr
        else:
            self.pngTimeRange = timeRange


    def __del__(self):
        self.dm = None
        self.dbss = None

    def getParms(self):
        rval = []
        pids = self.initParms()
        self.dm.getParmManager().setDisplayedParms(pids)
        for p in pids:
            rval.append(self.dm.getParmManager().getParm(p))

        if self._topo:
            tparm = self.dm.getParmManager().parmInExpr("Topo", 1)
            self.dm.parmMgr().setParmDisplayable(tparm, 1)
            rval.append(tparm)
        return rval

    def getBG(self):
        bgColor = self.getConfig('bgColor', "black")
        trans = self.getConfig('Png_transBG', 0, int)
        return bgColor, trans
    
    def getFileName(self, dir, setime):
        # calculate output filename, baseTime is AbsTime
        baseTimeFormat = self.getConfig('Png_baseTimeFormat', "%Y%m%d_%H%M")
        prefix = self.getConfig('Png_filenamePrefix', '')
        if self.baseTime is None:
            timeString = setime.stringFmt(baseTimeFormat)
        else:
            deltaTime = (setime - self.baseTime)/3600   #in hours
            timeString = self.baseTime.stringFmt(baseTimeFormat) + \
                         '_F' + `deltaTime`
        fname = dir + "/" + prefix + timeString
        return fname

    def getFileType(self):
        ext = self.getConfig('Png_fileType', 'png')
        return ext

    def writeInfo(self, dir, setime, visualInfo):
        if len(visualInfo) > 0:
            fname = self.getFileName(dir, setime) + ".info"
            infoWanted = self.getConfig('Png_infoFiles', 1, int)
            if infoWanted != 1:
                return

            # Write out information file
            fmt = "%Y%m%d_%H%M"
            file = open(fname, 'w')
            file.write('ValidTime: ' + setime.stringFmt(fmt) + '\n')
            file.write('NumOfGrids: ' + `len(visualInfo)` + '\n')
            for id, start, end, color, image in visualInfo:
                if image:
                    imgString='IMAGE'
                else:
                    imgString='GRAPHIC'
                file.write('Grid: ' + `id` + ' ' + start.stringFmt(fmt)
                  + ' ' + end.stringFmt(fmt) + ' ' + color + ' '
                  + imgString + '\n')
            file.close()

    def initSamples(self):
        from com.raytheon.uf.common.dataplugin.gfe.sample import SampleId
        # Load default sample sets
        samplesets = self.getConfig('DefaultSamples', [])
        sampleParms = self.getConfig('SampleParms', [])
        if samplesets is not None:
            from com.raytheon.uf.common.dataplugin.gfe.sample import SampleId
            self.dm.getSampleSetManager().setShowLatLon(False)
            # command SampleSet to load each sample set
            sampleInv = self.dm.getSampleSetManager().getInventoryAsStrings()
            for id in samplesets:
                sid = SampleId(id)
                for inv in sampleInv:
                    if sid.getName() == inv:
                        self.dm.getSampleSetManager().loadSampleSet(sid,'ADD')

    def initParms(self):
        dm = self.dm
        btext = self.getConfig('Png_parms', [])
        if len(btext) == 0:
            LogStream.logProblem("Png_parms missing or empty")
            raise UserWarning, "Png_parms missing or empty"

        if "Topo" in btext:
            self._topo = 1
            btext.remove("Topo")

        ip = self.getConfig('Png_image', None)
        if ip == "Topo":
            self._topo = 1

        # Attempt to decode pseudo parms in the config file
        from com.raytheon.uf.common.dataplugin.gfe.db.objects import ParmID
        from com.raytheon.uf.common.dataplugin.gfe.weatherelement import WEItem, WEGroup
        wegroup = WEGroup()
        wegroup.setName('png')
        weItems = jep.jarray(len(btext), WEItem)
        for i in range(len(btext)):
            text = btext[i].split(' ')
            parmid = text[0] + '_00000000_0000'            
            parmid = string.replace(parmid, ':', ':SITE_GRID_')            
            cycles = text[1]            
            p = ParmID(parmid)
            weItems[i] = WEItem(p, int(cycles))
        wegroup.setWeItems(weItems)            

        # make the text file
        # id = AFPS.TextFileID("png",'BUNDLE')
        # txtfile = AFPS.TextFile(id, ctext)

        # process the bundle
        dbIDs = dm.getParmManager().getAvailableDbs()
        availableParmIDs = []
        for i in range(dbIDs.size()):
            dbID = dbIDs.get(i)
            nextAvailable = dm.getParmManager().getAvailableParms(dbID)
            for next in nextAvailable: 
                availableParmIDs.append(next)
        
        size = len(availableParmIDs)
        jparmIds = jep.jarray(size, ParmID)
        for i in range(size):
            jparmIds[i] = availableParmIDs[i]
        vv = dm.getWEGroupManager().getParmIDs(wegroup,jparmIds)
        if len(vv) == 0:
            LogStream.logProblem("Png_parms contains no valid weather "
                                 + "elements")
            raise UserWarning, "Png_parms contains no valid weather elements"
        
        return vv

    def getConfig(self, opt, default, cast=None):
        if hasattr(self.config, opt):
            try:
                getter = attrgetter(opt)
                val = getter(self.config)
                #val = self.config[opt]
                if cast is not None:
                    return cast(val)
                return val
            except KeyError:
                return default
        else:                
            return default

    def paint(self, dir):
        #mmgr = self.dm.mapMgr()
        mv = []
        mids = []
        height = self.getConfig('Png_height', None, int)
        width = self.getConfig('Png_width', None, int)
        localFlag = self.getConfig('Png_localTime', 0, int)
        snapshotTime = self.getConfig('Png_snapshotTime', 0, int)
        useLegend = self.getConfig('Png_legend', 1, int)
        maps = self.getConfig('MapBackgrounds_default', [])
        leftExpand = self.getConfig('OfficeDomain_expandLeft', 10, int)
        rightExpand = self.getConfig('OfficeDomain_expandRight', 10, int)
        topExpand = self.getConfig('OfficeDomain_expandTop', 10, int)
        bottomExpand = self.getConfig('OfficeDomain_expandBottom', 10, int)
        fitToDataArea = self.getConfig('Png_fitToDataArea', None)
        omitColorbar = self.getConfig('Png_omitColorBar', 0, int)
        showLogo = self.getConfig('Png_logo', None)
        logoString = self.getConfig('Png_logoString', None)  
        smooth = self.getConfig('Png_smoothImage', 0, int)      
        fexten = self.getFileType()
        mask = self.getConfig(self.site + '_mask', None) 

        # get the fit to data edit area, and set the active edit area
        if fitToDataArea is not None:
            availableSets = self.dm.getRefManager().getAvailableSets()
            setSize = availableSets.size()
            for x in range(setSize):
                s = availableSets.get(x)            
                if fitToDataArea == s.getName():
                    refdata = self.dm.getRefManager().loadRefSet(s)
                    self.dm.getRefManager().setActiveRefSet(refdata) 

        maskBasedOnHistory = self.getConfig('Png_historyMask', 0, int)
        wholeDomain = self.getConfig('Png_wholeDomain', 0, int)

        viz = GFEPainter.GFEPainter(width, height, leftExpand, rightExpand, topExpand, bottomExpand, mask, wholeDomain)
        prms = self.getParms()
                        
        # allow user to specify precise interval for creation of images
        # rather than the automatically generated set
        try:
            paintInterval  = self.getConfig('Png_interval', 6, int)
            paintIntervalOffset = self.getConfig('Png_intervalOffset', 0, int)
            if paintInterval < 0:
                paintInterval = 1
            if paintInterval > 24:
                paintInterval = 24
            paintInterval = paintInterval * 3600   # into seconds
            paintIntervalOffset = paintIntervalOffset * 3600
            systemTime = TimeRange.TimeRange(self.dm.getParmManager().getSystemTimeRange())
            firstTime = systemTime.startTime()
            lastTime = systemTime.endTime()
            times = []
            unixT = firstTime.unixTime()
            t = int(int(unixT)/int(paintInterval))*paintInterval\
                + paintIntervalOffset
            t = AbsTime.AbsTime(t)
            while t <= lastTime:
                if t >= firstTime:
                    times.append(t)
                t = t + paintInterval
        except KeyError:
            times = Graphics.calcStepTimes(prms,
                       self.dm.parmMgr().systemTimeRange())
        if len(times) == 0:
            LogStream.logEvent("No grids to generate")
                           
        if useLegend:
            snapshotTime = self.getConfig('Png_snapshotTime', 1, int)
            descName = self.getConfig('Png_descriptiveWeName', 'SHORT')
            
            localTime = self.getConfig('Png_localTime', 0, int)
            if localTime:
                selector = 'Png_legendFormat_LT_'
            else:
                selector = 'Png_legendFormat_Zulu_' 
                
            durFmt = self.getConfig(selector + 'dur', '')
            startFmt = self.getConfig(selector + 'start', '%b %d %H%M%Z to ')
            endFmt = self.getConfig(selector + 'end', '%b %d %H%M%Z')
            snapshotFmt = self.getConfig(selector + 'snapshot', '%b%d%H%M%Z')
            overrideColors = {}
            for p in prms:
                pname = p.getParmID().compositeNameUI()
                if pname == self.ipn:
                    overrideColors[pname] = "White"

                color = self.getConfig(pname + "_Legend_color", None)
                if color:
                    overrideColors[pname] = color
            lang = self.getConfig('Png_legendLanguage', '');
            viz.setupLegend(localTime, snapshotTime, snapshotFmt, descName, durFmt, startFmt, endFmt, overrideColors, lang)        
        
        #TODO handle transparent background
        bgColor, trans = self.getBG()   
        
        if not omitColorbar:
            viz.enableColorbar()
            
        xOffset = self.getConfig("MapLabelXOffset", None, int)
        yOffset = self.getConfig("MapLabelYOffset", None, int)
        for map in maps:
            color = self.getConfig(map + "_graphicColor", None)
            lineWidth = self.getConfig(map + "_lineWidth", None, int)
            linePattern = self.getConfig(map + "_linePattern", None)
            labelAttribute = self.getConfig(map + "_labelAttribute", None)
            fontOffset = self.getConfig(map + "_fontOffset", None, int)
            viz.addMapBackground(map, color, lineWidth, linePattern, xOffset, 
                                 yOffset, labelAttribute, fontOffset)
            
        graphicParms = []
        fitToDataAlg = None
        for p in prms:
            pname = p.getParmID().compositeNameUI()
            colormap = self.getConfig(pname + '_defaultColorTable', None)
            colorMax = self.getConfig(pname + '_maxColorTableValue', None, float)
            colorMin = self.getConfig(pname + '_minColorTableValue', None, float)
            color = self.getConfig(pname + '_graphicColor', None)    
            lineWidth = self.getConfig(pname + '_lineWidth', None, int)
            viz.addGfeResource(pname, colormap=colormap, colorMin=colorMin, colorMax=colorMax, \
                               smooth=smooth, color=color, lineWidth=lineWidth)
            fitToDataAlg = self.getConfig(pname + '_fitToDataColorTable', None)
            if fitToDataAlg is not None:
                from com.raytheon.viz.gfe.rsc.colorbar import FitToData
                fit = FitToData(self.dm, p)
                if fitToDataAlg == 'All Grids':
                    fit.fitToData()
                    fitToDataAlg = None
                elif fitToDataAlg == 'All Grids over Area':
                    fit.fitToData(self.dm.getRefManager().getActiveRefSet())
                    fitToDataAlg = None
                    
            if pname == self.ipn:
                print "setting",pname,"to IMAGE"
                p.getDisplayAttributes().setVisMode(VisMode.IMAGE)
            else:
                print "setting",pname,"to GRAPHIC"
                p.getDisplayAttributes().setVisMode(VisMode.GRAPHIC)
                
        self.initSamples()
        
        # paint once to get map retrieval started
        if len(times) > 0:
            viz.paint(times[0], backgroundColor=bgColor)

        for t in times:
            paintTime = t
            if paintTime and self.overlapsWithGrids(prms, paintTime):
                self.dm.getSpatialDisplayManager().setSpatialEditorTime(paintTime.javaDate())
                visualInfo = []
                for p in prms:
                    griddata = p.overlappingGrid(paintTime.javaDate())
                    if griddata is None:
                        continue
                                            
                    # fit to data special cases
                    if p.getDisplayAttributes().getVisMode().toString() == 'Image':
                        fitToDataAlg = self.getConfig(p.getParmID().compositeNameUI() + '_fitToDataColorTable', None)
                        if fitToDataAlg:
                            from com.raytheon.viz.gfe.rsc.colorbar import FitToData
                            fit = FitToData(self.dm, p)
                            from com.raytheon.viz.gfe.edittool import GridID
                            gridid = GridID(p, paintTime.javaDate())
                            if fitToDataAlg == 'Single Grid':                                                
                                fit.fitToData(gridid)
                            elif fitToDataAlg == 'Single Grid over Area':                        
                                fit.fitToData(gridid, self.dm.getRefManager().getActiveRefSet())  
                            
                    info = (p.getParmID().toString(), AbsTime.AbsTime(griddata.getGridTime().getStart()),
                            AbsTime.AbsTime(griddata.getGridTime().getEnd()),                                
                            RGBColors.getColorName(p.getDisplayAttributes().getBaseColor()), p.getDisplayAttributes().getVisMode().toString() == 'Image')                        
                    visualInfo.append(info)

                viz.paint(paintTime, backgroundColor=bgColor)
                fname = self.getFileName(dir, t) + '.' + fexten
                viz.outputFiles(fname, showLogo, logoString)
                self.writeInfo(dir, paintTime, visualInfo)
            else:
                LogStream.logEvent("No grids to generate for ", `t`)

        visuals = None
        mv = None
        iv = None
        lv = None

    # return true if there is grid data that overlaps with time t
    def overlapsWithGrids(self, prms, t):
        totalTR = None
        for p in prms:
            grid = p.overlappingGrid(t.javaDate())
            if grid is not None:
                gridTime = TimeRange.TimeRange(grid.getGridTime())
                if totalTR is None:
                    totalTR = gridTime
                else:
                    totalTR.combineWith(gridTime)
        if totalTR is not None and totalTR.contains(t) and \
          self.pngTimeRange.overlaps(totalTR):
            return 1
        else:
            return 0

def usage():
    msg = """
    usage: ifpIMAGE [-c config] [-u username] [-h host] [-p port] -o directory
      [-b baseTime] [-s startTime] [-e endTime] [-t usrTimeRng]

         config   : Name of GFE style config file to use.
         directory: Where you wan't the png files written to.
         username : The name of the user (for config file lookup).
         baseTime : Output filenames are relative to baseTime. Basetime
                    format is yyyymmdd_hhmm.
         host     : The host the ifpServer is running on.
         port     : The rpc port number the ifpServer is using.
         startTime: starting time for images in format yyyymmdd_hhmm
         endTime  : ending time for images in format yyyymmdd_hhmm\n\n
         usrTimeRng: used to specify a user selected time range (e.g., "Day_3")
                    'usrTimeRng' overrides the start/endTime switches.
         drtTime  : The GFE may be started in the displaced real time mode.
                    The format of this entry is yyyymmdd_hhmm.
"""


    LogStream.logProblem(msg)
    sys.stderr.write(msg)

#---------------------------------------------------------------------------
### Makes an AbsTime from the specified string
#---------------------------------------------------------------------------
def decodeTimeString(timeStr):
    try:
        intTime = time.strptime(timeStr, "%Y%m%d_%H%M")
    except:
        LogStream.logProblem(timeStr, \
          "is not a valid time string.  Use YYYYMMDD_HHMM")
        raise SyntaxError, "Bad date format YYYYMMDD_HHMM"

    return AbsTime.absTimeYMD(intTime[0], intTime[1], intTime[2],
      intTime[3], intTime[4], 0)


def main():
    LogStream.logEvent("ifpIMAGE Starting")
    
    DEFAULT_OUTPUT_DIR = '../products/IMAGE'

    #import siteConfig
    config = 'gfeConfig'
    userNameOption = 'SITE'
    #outDir = siteConfig.GFESUITE_PRDDIR + "/IMAGE"
    outDir = DEFAULT_OUTPUT_DIR
    tr = TimeRange.allTimes()
    startTime = tr.startTime()
    baseTime = None
    endTime = tr.endTime()
    usrTimeName = None    

    #port = int(siteConfig.GFESUITE_PORT)
    try:
        optlist, oargs = getopt.getopt(sys.argv[1:], "c:u:h:p:o:b:s:e:t:")
        for opt in optlist:
            if opt[0] == '-c':
                config = opt[1]
            elif opt[0] == '-u':
                userNameOption = opt[1]
            elif opt[0] == '-o':
                outDir = opt[1]
            elif opt[0] == '-s':
                startTime = decodeTimeString(opt[1])
            elif opt[0] == '-e':
                endTime = decodeTimeString(opt[1])
            elif opt[0] == '-t':
                usrTimeName = opt[1]
            elif opt[0] == '-b':
                baseTime = decodeTimeString(opt[1])

    except getopt.GetoptError, e:
        LogStream.logProblem(e)
        usage()
        raise SyntaxError, "Bad command line argument specified"
    
    if outDir == DEFAULT_OUTPUT_DIR:
        settings = __import__(config)
        if hasattr(settings, "GFESUITE_PRDDIR"):
            outDir = getattr(settings, "GFESUITE_PRDDIR") + '/IMAGE'
            
    LogStream.logEvent("Using output directory: " + outDir)
    
    if not os.path.exists(outDir):
        s = "Missing output directory: " + outDir
        LogStream.logProblem(s)
        usage()
        raise SyntaxError, s
    
    if not os.path.isdir(outDir):
        s = "Specified output directory is not a directory: " + outDir
        LogStream.logProblem(s)
        usage()
        raise SyntaxError, s

    if not os.access(outDir, os.W_OK):
        s = "Output directory is not writable: " + outDir
        LogStream.logProblem(s)
        usage()
        raise SyntaxError, s

    pngTimeRange = TimeRange.TimeRange(startTime, endTime)    

    try:
        pngw = PngWriter(config,userNameOption, baseTime,
          pngTimeRange, usrTimeName)
        pngw.paint(outDir)
    except Exception, e:
        LogStream.logProblem(LogStream.exc())
        sys.exit(1)
    LogStream.logEvent("ifpIMAGE Finished")    

if __name__ == "__main__":
    main()
