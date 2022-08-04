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
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer  Description
# ------------- -------- --------- ---------------------------------------------
# Aug 20, 2012  1077     randerso  Fixed backgroundColor setting
# Aug 20, 2012  1082     randerso  Fixed 1 image per grid
# Aug 29, 2012  1081     dgilling  Update usage statement.
# Apr 25, 2015  4952     njensen   Updated for new JEP API
# Feb 06, 2017  5959     randerso  Removed Java .toString() calls
# Feb 07, 2017  6092     randerso  Refactored to support calling validateArgs()
#                                  from gfeClient.py
# Feb 05, 2018  6762     randerso  Make color bar always display for image parm
# Feb 20, 2018  6864     randerso  Changed to create output directory if it
#                                  doesn't exist
# Mar 15, 2018  6967     randerso  Removed extra paint call now that
#                                  VizPainter.paint() waits for resources
#                                  to be painted
# Dec 04, 2018  7667     dgilling  Removed "no grids to generate" message.
#
##



def runIfpImage(args):
    ############################################################################
    # PngWriter and required imports nested in this function because they
    # can only be run under Jep. This allows validateArgs to be called from
    # a pure Python environment
    ############################################################################

    import LogStream, getopt, os, time, errno
    import TimeRange, AbsTime
    import GFEPainter
    import loadConfig

    from operator import attrgetter

    from java.util import ArrayList
    from com.raytheon.uf.common.dataplugin.gfe.db.objects import ParmID
    from com.raytheon.uf.common.dataplugin.gfe.sample import SampleId
    from com.raytheon.uf.common.dataplugin.gfe.weatherelement import WEItem, WEGroup
    from com.raytheon.uf.common.time import DataTime
    from com.raytheon.uf.viz.core import RGBColors
    from com.raytheon.uf.viz.core.localization import LocalizationManager
    from com.raytheon.viz.gfe.core import GFEIntervalTimeMatcher

    from com.raytheon.viz.gfe.edittool import GridID
    from com.raytheon.viz.gfe.rsc.colorbar import FitToData

    class PngWriter:
        def __init__(self, conf="testIFPImage", baseTime=None,
          timeRange=None, usrTimeRange=None):
            self.site = LocalizationManager.getInstance().getCurrentSite()

            self._topo = 0

            # import the config file
            self.config = __import__(conf)
            loadConfig.loadPreferences(self.config)

            self.baseTime = baseTime

            # Create GFEPainter first and get DataManager from painter
            self.viz = self.createPainter()
            self.dm = self.viz.getDataManager()

            LogStream.logEvent("Configuration File: ", conf)

            self.ipn = self.getConfig('Png_image', '')

            # user named time range specified?
            if usrTimeRange is not None:
                s_tr = self.dm.getSelectTimeRangeManager().getRange(usrTimeRange)
                if s_tr is None:
                    s = usrTimeRange + " is not a valid time range name."
                    LogStream.logProblem(s)
                    raise ValueError(s)
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

        def getFileName(self, directory, setime):
            # calculate output filename, baseTime is AbsTime
            baseTimeFormat = self.getConfig('Png_baseTimeFormat', "%Y%m%d_%H%M")
            prefix = self.getConfig('Png_filenamePrefix', '')
            if self.baseTime is None:
                timeString = setime.stringFmt(baseTimeFormat)
            else:
                deltaTime = (setime - self.baseTime) // 3600  # in hours
                timeString = self.baseTime.stringFmt(baseTimeFormat) + \
                             '_F' + repr(deltaTime)
            fname = directory + "/" + prefix + timeString
            return fname

        def getFileType(self):
            ext = self.getConfig('Png_fileType', 'png')
            return ext

        def writeInfo(self, directory, setime, visualInfo):
            if len(visualInfo) > 0:
                fname = self.getFileName(directory, setime) + ".info"
                infoWanted = self.getConfig('Png_infoFiles', 1, int)
                if infoWanted != 1:
                    return

                # Write out information f
                fmt = "%Y%m%d_%H%M"
                with open(fname, 'w') as f:
                    f.write('ValidTime: ' + setime.stringFmt(fmt) + '\n')
                    f.write('NumOfGrids: ' + repr(len(visualInfo)) + '\n')
                    for parmID, start, end, color, image in visualInfo:
                        if image:
                            imgString = 'IMAGE'
                        else:
                            imgString = 'GRAPHIC'
                        f.write('Grid: ' + repr(parmID) + ' ' + start.stringFmt(fmt)
                          + ' ' + end.stringFmt(fmt) + ' ' + color + ' '
                          + imgString + '\n')

        def initSamples(self):
            # Load default sample sets
            samplesets = self.getConfig('DefaultSamples', [])
            if samplesets is not None:
                self.dm.getSampleSetManager().setShowLatLon(False)
                # command SampleSet to load each sample set
                sampleInv = self.dm.getSampleSetManager().getInventoryAsStrings()
                for sampleSet in samplesets:
                    sid = SampleId(sampleSet)
                    for inv in sampleInv:
                        if sid.getName() == inv:
                            self.dm.getSampleSetManager().loadSampleSet(sid, 'ADD')

        def initParms(self):
            dm = self.dm
            btext = self.getConfig('Png_parms', [])
            if len(btext) == 0:
                LogStream.logProblem("Png_parms missing or empty")
                raise UserWarning("Png_parms missing or empty")

            if "Topo" in btext:
                self._topo = 1
                btext.remove("Topo")

            ip = self.getConfig('Png_image', None)
            if ip == "Topo":
                self._topo = 1

            # Attempt to decode pseudo parms in the config file
            wegroup = WEGroup()
            wegroup.setName('png')
            weItems = jep.jarray(len(btext), WEItem)
            for i in range(len(btext)):
                text = btext[i].split(' ')
                parmid = text[0] + '_00000000_0000'
                parmid = parmid.replace(':', ':SITE_GRID_')
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
            for dbID in dbIDs:
                availableParmIDs += dm.getParmManager().getAvailableParms(dbID)

            size = len(availableParmIDs)
            jparmIds = jep.jarray(size, ParmID)
            for i in range(size):
                jparmIds[i] = availableParmIDs[i]
            vv = dm.getWEGroupManager().getParmIDs(wegroup, jparmIds)
            if len(vv) == 0:
                LogStream.logProblem("Png_parms contains no valid weather "
                                     + "elements")
                raise UserWarning("Png_parms contains no valid weather elements")

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

        def createPainter(self):
            # Extract properties needed to construct painter
            height = self.getConfig('Png_height', None, int)
            width = self.getConfig('Png_width', None, int)
            leftExpand = self.getConfig('OfficeDomain_expandLeft', 10, int)
            rightExpand = self.getConfig('OfficeDomain_expandRight', 10, int)
            topExpand = self.getConfig('OfficeDomain_expandTop', 10, int)
            bottomExpand = self.getConfig('OfficeDomain_expandBottom', 10, int)
            mask = self.getConfig(self.site + '_mask', None)
            wholeDomain = self.getConfig('Png_wholeDomain', 0, int)

            #TODO handle transparent background
            bgColor, trans = self.getBG()

            return GFEPainter.GFEPainter(width, height, leftExpand, rightExpand, topExpand, bottomExpand, mask, wholeDomain, bgColor)

        def paint(self, directory):
            snapshotTime = self.getConfig('Png_snapshotTime', 0, int)
            useLegend = self.getConfig('Png_legend', 1, int)
            maps = self.getConfig('MapBackgrounds_default', [])
            fitToDataArea = self.getConfig('Png_fitToDataArea', None)
            omitColorbar = self.getConfig('Png_omitColorBar', 0, int)
            showLogo = self.getConfig('Png_logo', None)
            logoString = self.getConfig('Png_logoString', None)
            smooth = self.getConfig('Png_smoothImage', 0, int)
            fexten = self.getFileType()

            # get the fit to data edit area, and set the active edit area
            if fitToDataArea is not None:
                availableSets = self.dm.getRefManager().getAvailableSets()
                setSize = availableSets.size()
                for x in range(setSize):
                    s = availableSets.get(x)
                    if fitToDataArea == s.getName():
                        refdata = self.dm.getRefManager().loadRefSet(s)
                        self.dm.getRefManager().setActiveRefSet(refdata)

            # TODO: determine if this should be implemented
            maskBasedOnHistory = self.getConfig('Png_historyMask', 0, int)

            viz = self.viz

            if not omitColorbar:
                viz.enableColorbar()

            prms = self.getParms()

            # allow user to specify precise interval for creation of images
            # rather than the automatically generated set
            paintInterval = self.getConfig('Png_interval', None, int)
            if paintInterval is not None:
                # Interval specified, create interval time matcher
                paintIntervalOffset = self.getConfig('Png_intervalOffset', 0, int)
                if paintInterval < 0:
                    paintInterval = 1
                if paintInterval > 24:
                    paintInterval = 24
                tm = GFEIntervalTimeMatcher()
                tm.setTimeMatchingInterval(paintInterval, paintIntervalOffset, self.dm.getParmManager().getSystemTimeRange())
                viz.getDescriptor().setTimeMatcher(tm)

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
                lang = self.getConfig('Png_legendLanguage', '')
                viz.setupLegend(localTime, snapshotTime, snapshotFmt, descName, durFmt, startFmt, endFmt, overrideColors, lang)

            xOffset = self.getConfig("MapLabelXOffset", None, int)
            yOffset = self.getConfig("MapLabelYOffset", None, int)
            for m in maps:
                color = self.getConfig(m + "_graphicColor", None)
                lineWidth = self.getConfig(m + "_lineWidth", None, int)
                linePattern = self.getConfig(m + "_linePattern", None)
                labelAttribute = self.getConfig(m + "_labelAttribute", None)
                fontOffset = self.getConfig(m + "_fontOffset", None, int)
                viz.addMapBackground(m, color, lineWidth, linePattern, xOffset,
                                     yOffset, labelAttribute, fontOffset)

            imageParm = None
            fitToDataAlg = None
            for p in prms:
                pname = p.getParmID().compositeNameUI()
                colormap = self.getConfig(pname + '_defaultColorTable', None)
                colorMax = self.getConfig(pname + '_maxColorTableValue', None, float)
                colorMin = self.getConfig(pname + '_minColorTableValue', None, float)
                color = self.getConfig(pname + '_graphicColor', None)
                lineWidth = self.getConfig(pname + '_lineWidth', None, int)
                viz.addGfeResource(p, colormap=colormap, colorMin=colorMin, colorMax=colorMax, \
                                   smooth=smooth, color=color, lineWidth=lineWidth)
                fitToDataAlg = self.getConfig(pname + '_fitToDataColorTable', None)
                if fitToDataAlg is not None:
                    fit = FitToData(self.dm, p)
                    if fitToDataAlg == 'All Grids':
                        fit.fitToData()
                        fitToDataAlg = None
                    elif fitToDataAlg == 'All Grids over Area':
                        fit.fitToData(self.dm.getRefManager().getActiveRefSet())
                        fitToDataAlg = None

                if pname == self.ipn:
                    imageParm = p

            if imageParm:
                self.dm.getSpatialDisplayManager().activateParm(imageParm)

            self.initSamples()

            # Verify all resources are time matched before painting
            desc = viz.getDescriptor()
            desc.redoTimeMatching()
            times = desc.getFramesInfo().getFrameTimes()

            for frame in times:
                paintTime = AbsTime.AbsTime(frame.getRefTime())
                if self.overlapsWithGrids(prms, paintTime.javaDate()):
                    visualInfo = []
                    for p in prms:
                        griddata = p.overlappingGrid(paintTime.javaDate())
                        if griddata is None:
                            continue

                        # fit to data special cases
                        if str(p.getDisplayAttributes().getVisMode()) == 'Image':
                            fitToDataAlg = self.getConfig(p.getParmID().compositeNameUI() + '_fitToDataColorTable', None)
                            if fitToDataAlg:
                                fit = FitToData(self.dm, p)
                                gridid = GridID(p, paintTime.javaDate())
                                if fitToDataAlg == 'Single Grid':
                                    fit.fitToData(gridid)
                                elif fitToDataAlg == 'Single Grid over Area':
                                    fit.fitToData(gridid, self.dm.getRefManager().getActiveRefSet())

                        info = (str(p.getParmID()), AbsTime.AbsTime(griddata.getGridTime().getStart()),
                                AbsTime.AbsTime(griddata.getGridTime().getEnd()),
                                RGBColors.getColorName(p.getDisplayAttributes().getBaseColor()), str(p.getDisplayAttributes().getVisMode()) == 'Image')
                        visualInfo.append(info)

                    viz.paint(frame)
                    fname = self.getFileName(directory, paintTime) + '.' + fexten
                    viz.outputFiles(fname, showLogo, logoString)
                    self.writeInfo(directory, paintTime, visualInfo)

        # return true if there is grid data that overlaps with time t
        def overlapsWithGrids(self, prms, t):
            for p in prms:
                grid = p.overlappingGrid(t)
                if grid is not None:
                    gridTime = TimeRange.TimeRange(grid.getGridTime())
                    if self.pngTimeRange.overlaps(gridTime):
                        return 1
            return 0

################################################################################
# body of runIfpImage method
################################################################################
    def decodeTimeStruct(timeStruct):
        return AbsTime.absTimeYMD(timeStruct.tm_year, timeStruct.tm_mon,
                                  timeStruct.tm_mday,
                                  timeStruct.tm_hour, timeStruct.tm_min)

    tr = TimeRange.allTimes()
    startTime = tr.startTime()
    endTime = tr.endTime()
    baseTime = None

    if args.startTime:
        startTime = decodeTimeStruct(args.startTime)

    if args.endTime:
        endTime = decodeTimeStruct(args.endTime)

    if args.baseTime:
        baseTime = decodeTimeStruct(args.baseTime)

    pngTimeRange = TimeRange.TimeRange(startTime, endTime)

    outDir = args.outDir
    if outDir == DEFAULT_OUTPUT_DIR:
        settings = __import__(args.configFile)
        if hasattr(settings, "GFESUITE_PRDDIR"):
            outDir = getattr(settings, "GFESUITE_PRDDIR") + '/IMAGE'

    try:
        os.makedirs(outDir)
    except OSError as e:
        if e.errno != errno.EEXIST:
            s = "Unable to create output directory: " + outDir
            LogStream.logProblem(s)
            raise IOError(s)

    if not os.path.isdir(outDir):
        s = "Specified output directory is not a directory: " + outDir
        LogStream.logProblem(s)
        raise IOError(s)

    if not os.access(outDir, os.W_OK):
        s = "Output directory is not writable: " + outDir
        LogStream.logProblem(s)
        raise IOError(s)

    LogStream.logEvent("ifpIMAGE Starting")
    LogStream.logEvent("Using output directory: " + outDir)

    try:
        pngw = PngWriter(args.configFile, baseTime, pngTimeRange, args.usrTimeName)
        pngw.paint(outDir)
    except Exception as e:
        LogStream.logProblem(LogStream.exc())
    LogStream.logEvent("ifpIMAGE Finished")

def validateArgs(args=None, parents=[]):
    ############################################################################
    # imports required for this method must be here so it can be invoked
    # from gfeClient.py
    ############################################################################
    from ufpy import UsageArgumentParser
    from ufpy.UsageArgumentParser import StoreTimeAction

    global DEFAULT_OUTPUT_DIR
    DEFAULT_OUTPUT_DIR = '../products/IMAGE'

    parser = UsageArgumentParser.UsageArgumentParser(conflict_handler="resolve",
                                                     parents=parents,
                                                     prog='ifpIMAGE')
    parser.add_argument("-c", "--config", action="store", dest="configFile", required=False,
                      default="gfeConfig",
                      help="GFE config file -- default gfeConfig",
                      metavar="configFile")
    parser.add_argument("-u", action="store", dest="userName", required=False,
                        help="user name -- default SITE",
                        default="SITE",
                        metavar="userName")
    parser.add_argument("-o", action="store", dest="outDir", required=False,
                        help="Where you want the png files written",
                        default=DEFAULT_OUTPUT_DIR,
                        metavar="directory")
    parser.add_argument("-b", action=StoreTimeAction, dest="baseTime", required=False,
                        help="Output filenames are relative to baseTime. Basetime format is yyyymmdd_hhmm",
                        metavar="baseTime")
    parser.add_argument("-s", action=StoreTimeAction, dest="startTime", required=False,
                        help="starting time for images in format YYYYMMDD_hhmm",
                        metavar="startTime")
    parser.add_argument("-e", action=StoreTimeAction, dest="endTime", required=False,
                        help="ending time for images in format YYYYMMDD_hhmm\n\n",
                        metavar="endTime")
    parser.add_argument("-t", action="store", dest="usrTimeName", required=False,
                        help="used to specify a user selected time range (e.g., \"Day_3\") 'usrTimeRng' overrides the start/endTime switches.",
                        metavar="usrTimeRng")

    args = parser.parse_args(args)

    return args

def error(msg):
    print("ERROR: %s\n" % msg)

def main():
    args = validateArgs()
    runIfpImage(args)

if __name__ == "__main__":
    main()
