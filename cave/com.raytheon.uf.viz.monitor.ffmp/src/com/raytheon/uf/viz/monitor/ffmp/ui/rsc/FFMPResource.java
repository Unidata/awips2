/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 *
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 *
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.viz.monitor.ffmp.ui.rsc;

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.SortedSet;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.Point;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPBasin;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPBasinData;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPBasinMetaData;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPGap;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPGuidanceBasin;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPGuidanceInterpolation;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPRecord;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPRecord.FIELDS;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPRecord.ZOOM;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPTemplates;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPUtils;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPVirtualGageBasin;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPVirtualGageBasinMetaData;
import com.raytheon.uf.common.dataplugin.ffmp.HucLevelGeometriesFactory;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.monitor.config.FFFGDataMgr;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager;
import com.raytheon.uf.common.monitor.xml.DomainXML;
import com.raytheon.uf.common.monitor.xml.ProductXML;
import com.raytheon.uf.common.monitor.xml.SourceXML;
import com.raytheon.uf.common.plugin.hpe.data.HpeLabelKey;
import com.raytheon.uf.common.plugin.hpe.request.HpeLabelDataRequest;
import com.raytheon.uf.common.plugin.hpe.request.HpeLabelDataResponse;
import com.raytheon.uf.common.status.IPerformanceStatusHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.PerformanceStatus;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.style.StyleRule;
import com.raytheon.uf.common.style.image.ImagePreferences;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.DrawableLine;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.JTSCompiler;
import com.raytheon.uf.viz.core.drawables.JTSCompiler.JTSGeometryData;
import com.raytheon.uf.viz.core.drawables.JTSCompiler.PointStyle;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IColormapShadedShapeExtension;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IColormapShadedShapeExtension.IColormapShadedShape;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.EditableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.uf.viz.core.rsc.extratext.ExtraTextResourceData;
import com.raytheon.uf.viz.core.rsc.extratext.IExtraTextGeneratingResource;
import com.raytheon.uf.viz.core.rsc.interrogation.ClassInterrogationKey;
import com.raytheon.uf.viz.core.rsc.interrogation.Interrogatable;
import com.raytheon.uf.viz.core.rsc.interrogation.InterrogateMap;
import com.raytheon.uf.viz.core.rsc.interrogation.InterrogationKey;
import com.raytheon.uf.viz.core.rsc.interrogation.Interrogator;
import com.raytheon.uf.viz.monitor.ffmp.FFMPMonitor;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.FFMPConfig;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.FfmpBasinTableDlg;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.FfmpTableConfig;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.FfmpTableConfigData;
import com.raytheon.uf.viz.monitor.ffmp.ui.listeners.FFMPAutoRefreshEvent;
import com.raytheon.uf.viz.monitor.ffmp.ui.listeners.FFMPCWAChangeEvent;
import com.raytheon.uf.viz.monitor.ffmp.ui.listeners.FFMPFieldChangeEvent;
import com.raytheon.uf.viz.monitor.ffmp.ui.listeners.FFMPHUCChangeEvent;
import com.raytheon.uf.viz.monitor.ffmp.ui.listeners.FFMPListener;
import com.raytheon.uf.viz.monitor.ffmp.ui.listeners.FFMPMaintainLayerEvent;
import com.raytheon.uf.viz.monitor.ffmp.ui.listeners.FFMPParentBasinEvent;
import com.raytheon.uf.viz.monitor.ffmp.ui.listeners.FFMPScreenCenterEvent;
import com.raytheon.uf.viz.monitor.ffmp.ui.listeners.FFMPStreamTraceEvent;
import com.raytheon.uf.viz.monitor.ffmp.ui.listeners.FFMPTimeChangeEvent;
import com.raytheon.uf.viz.monitor.ffmp.ui.listeners.FFMPWorstCaseEvent;
import com.raytheon.uf.viz.monitor.ffmp.ui.listeners.IFFMPResourceListener;
import com.raytheon.uf.viz.monitor.ffmp.ui.thread.UpdateLoadJob;
import com.raytheon.viz.ui.input.EditableManager;
import com.raytheon.viz.ui.input.InputAdapter;

import tec.uom.se.quantity.Quantities;

/**
 * Resource to display FFMP data
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date           Ticket#  Engineer   Description
 * -------------- -------- ---------- ------------------------------------------
 * 29 June, 2009  2521     dhladky    Initial creation
 * 11 Apr.  2012  14522    gzhang     Fixing invalid thread error.
 * Jul 31, 2012   14517    mpduff     Fix for blanking map on update.
 * Sep 14, 2012   1048     njensen    Code cleanup
 * Dec 07, 2012   1353     rferrel    Changes for non-blocking FFMPSplash
 *                                    dialog.
 * Jan 10, 2013   1475     dhladky    Some cleanup
 * Jan 27, 2013   1478     dhladky    Changed gap collection to a generic list
 *                                    instead of Arraylist
 * Feb 01, 2013   1569     D. Hladky  Added constants
 * Feb 10, 2013   1584     mpduff     Add performance logging.
 * Feb 19, 2013   1639     njensen    Replaced FFMPCacheRecord with FFMPRecord
 * Feb 20, 2013   1635     dhladky    Fixed multiple guidance display
 * Feb 28, 2013   1729     dhladky    Changed the way the loaders are managed
 *                                    via the status updates.
 * Mar 06, 2013   1769     dhladky    Changed threading to use count down latch.
 * Apr 09, 2013   1890     dhladky    General cleanup.
 * Apr 10, 2013   1896     bsteffen   Make FFMPResource work better with D2D
 *                                    time matcher.
 * Apr 25, 2013   1954     bsteffen   Skip extent checking for FFMP shape
 *                                    generation.
 * Apr 26, 2013   1954     bsteffen   Minor code cleanup throughout FFMP.
 * Jun 06, 2013   2075     njensen    No longer schedules load threads,
 *                                    refactored updates
 * Jun 27, 2013   2152     njensen    More thorough disposeInternal()
 * Jul 15, 2013   2184     dhladky    Remove all HUC's for storage except ALL
 * Jul 17, 2013   2197     njensen    Improved speed of getName()
 * Oct 18, 2013   16151    gzhang     Used getAverageValue() for QPF Graph.
 * Jan 21, 2014   15874    gzhang     Use getValue() for QPFSCAN independent.
 * Feb 19, 2014   2819     randerso   Removed unnecessary .clone() call
 * Mar 03, 2014   2804     mschenke   Set back up clipping pane
 * Apr 30, 2014   16148    gzhang     Filter Basin Dates for Trend and Table
 *                                    Gap.
 * May 05, 2014   3026     mpduff     Display Hpe bias source.
 * May 19, 2014   16096    gzhang     Make getBasin() protected for
 *                                    FFMPDataGenerator.L
 * Jun 24, 2016            mnash      Make FFMPResource implement Interrogatable
 * Aug 13, 2014   3492     mapeters   Updated deprecated createWireframeShape()
 *                                    calls.
 * Aug 14, 2014   3523     mapeters   Updated deprecated {@link
 *                                    DrawableString#textStyle} assignments.
 * Sep 23, 2014   3009     njensen    Overrode recycleInternal()
 * Nov 10, 2014   3026     dhladky    HPE BIAS displays.
 * Dec 16, 2014   3026     mpduff     Change location of text.
 * Feb 13, 2015   4121     mpduff     Change label caching.
 * Sep 28, 2015   4756     dhladky    Multiple guidance sources.
 * Oct 26, 2015   5056     dhladky    Simplified Guidance Interpolation.
 * Nov 05, 2015   5070     randerso   Adjust font sizes for dpi scaling
 * Feb 02, 2016   16771    arickert   Opening the FFMP dialog in initInternal
 * Sep 14, 2016   3241     bsteffen   Update deprecated JTSCompiler method calls
 * Jan 23, 2017   6032     njensen    Implement IExtraTextGeneratingResource
 * May 05, 2017   14336    lshi       FFMP VGB value differences between A1 and
 *                                    A2
 * May 15, 2017   11861    lshi       FFMP use of QPF in calculating QPE in
 *                                    Basin Table
 * Sep 15, 2017   20297    lshi       FFMP AlertViz errors when changing layers
 *                                    and opening basin trend graphs
 * Nov 28, 2017   5863     bsteffen   Change dataTimes to a NavigableSet
 * Mar 07, 2018   6581     mduff      Implemented incremental unzoom.
 * Mar 27, 2018   7029     njensen    Support changing colormap that backs
 *                                    shaded shapes
 * Apr 04, 2018   6889     njensen    Use brightness from ImagePreferences if
 *                                    present but missing in ImagingCapability
 * Jun 12, 2018   6796     mduff      Restore changes for ticket, seems to have
 *                                    been lost in a merge or restore.
 * Jul 20, 2018   6642     randerso   Code cleanup.
 * Jul 30, 2018   6720     njensen    Update for changed method names
 * Aug 07, 2018   6720     njensen    Use display name less
 * Aug 14, 2018   6720     njensen    Use simplified enums
 * Aug 24, 2018   7413     mduff      Removed table update logic.
 * Oct 18, 2018   DR 11861 mfontaine   FFMP use of QPF in Basin Table
 * Feb 28, 2019   #6952    dgilling   Do not limit data displayed in Basin Trend graph
 *                                    to what is selected on Time Duration widget.
 * Apr  1, 2019,  6801     tgurney    When zooming, zoom all panels in the
 *                                    resource's current editor
 *
 * </pre>
 *
 * @author dhladky
 */
public class FFMPResource
        extends AbstractVizResource<FFMPResourceData, MapDescriptor>
        implements IResourceDataChanged, IFFMPResourceListener, FFMPListener,
        Interrogatable, IExtraTextGeneratingResource {

    private static final String NL = "\n";

    private static final String NO_DATA = "NO DATA";

    /** Status handler */
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(FFMPResource.class);

    /** Performance log statement prefix */
    private static final String prefix = "FFMP Resource:";

    /** Performance logger */
    private final IPerformanceStatusHandler perfLog = PerformanceStatus
            .getHandler(prefix);

    /**
     * The zoom level for an aggregate view.
     */
    private static final int AGGREGATE_ZOOM = 137;

    /**
     * The zoom level for a basin view.
     */
    private static final int BASIN_ZOOM = 55;

    /**
     * Hatch pattern for stream filling
     */
    private static final byte[] STREAM_FILL = new byte[] { 0x40, 0x08, 0x40,
            0x08, 0x00, 0x01, 0x00, 0x01, 0x12, 0x40, 0x12, 0x40, 0x41, 0x04,
            0x41, 0x04, 0x00, (byte) 0x90, 0x00, (byte) 0x90, 0x08, 0x01, 0x08,
            0x01, 0x40, 0x00, 0x40, 0x00, 0x02, 0x44, 0x02, 0x44, 0x10, 0x01,
            0x10, 0x01, 0x01, 0x10, 0x01, 0x10, 0x04, 0x00, 0x04, 0x00, 0x40,
            (byte) 0x82, 0x40, (byte) 0x82, 0x11, 0x10, 0x11, 0x10, 0x00, 0x00,
            0x00, 0x00, 0x04, 0x44, 0x04, 0x44, 0x40, 0x00, 0x40, 0x00,

            0x40, 0x08, 0x40, 0x08, 0x00, 0x01, 0x00, 0x01, 0x12, 0x40, 0x12,
            0x40, 0x41, 0x04, 0x41, 0x04, 0x00, (byte) 0x90, 0x00, (byte) 0x90,
            0x08, 0x01, 0x08, 0x01, 0x40, 0x00, 0x40, 0x00, 0x02, 0x44, 0x02,
            0x44, 0x10, 0x01, 0x10, 0x01, 0x01, 0x10, 0x01, 0x10, 0x04, 0x00,
            0x04, 0x00, 0x40, (byte) 0x82, 0x40, (byte) 0x82, 0x11, 0x10, 0x11,
            0x10, 0x00, 0x00, 0x00, 0x00, 0x04, 0x44, 0x04, 0x44, 0x40, 0x00,
            0x40, 0x00, };

    /** used for the basin trace stuff */
    private FFMPRecord.CLICK_TYPE stream = FFMPRecord.CLICK_TYPE.UP_DOWN;

    /* colorMap helper */
    private FFMPColorUtils colorUtil = null;

    /* The font used */
    private IFont font = null;

    /* The center X font used */
    private IFont xfont = null;

    /* VGB stuff */
    private static int BOX_HEIGHT = 10;

    private static int BOX_WIDTH = 10;

    /* formatter */
    private DecimalFormat df = new DecimalFormat("#.##");

    /** screen re-centering from table **/
    private Coordinate center = null;

    /** select stream pfaf */
    // private Long lastStreamPfaf = null;

    private Long nextStreamPfaf = null;

    /** maintain the zoom layer in the display **/
    public FFMPRecord.ZOOM lowestCenter = FFMPRecord.ZOOM.WFO;

    /** expansion of the window **/
    private static final double EXPANSION_FACTOR = 1.0;

    /** HPE Constant */
    private static final String HPE = "HPE";

    /** BiasHPE Constant */
    private static final String BHPE = "BHPE";

    /** the stream cross hatched area **/
    private IWireframeShape streamOutlineShape = null;

    /** small basins overlay **/
    private IWireframeShape smallBasinOverlayShape = null;

    /** the stream cross hatched area **/
    private IShadedShape streamShadedShape = null;

    /** always the same vertexes, one for each CWA **/
    private final FFMPShapeContainer shadedShapes = new FFMPShapeContainer();

    /** Basin shaded shape **/
    protected Map<DataTime, FFMPDrawable> drawables = new ConcurrentHashMap<>();

    /** VGB drawables **/
    protected Map<String, PixelCoverage> vgbDrawables = new HashMap<>();

    /** used to create the wireframes for the streams **/
    private Set<Long> streamPfafIds = null;

    /** the query job **/
    private FFMPDataRetrievalJob queryJob;

    /** geom type **/
    private String geometryType;

    // time used by the resource
    private DataTime paintTime = null;

    /** mouse handler **/
    private final IInputHandler inspectAdapter = new InputAdapter() {

        @Override
        public boolean handleMouseUp(int x, int y, int mouseButton) {
            if (mouseButton == 3) {
                if (isStreamFollow()) {
                    perfLog.log("Basin Trace Draw Init");
                    traceClick(getResourceContainer().translateClick(x, y));
                    return true;
                }
            }
            return false;
        }

    };

    private FFMPRecord rateRecord = null;

    private boolean isNewRate = true;

    private FFMPRecord qpeRecord = null;

    private boolean isNewQpe = true;

    private FFMPRecord guidRecord = null;

    private boolean isNewGuid = true;

    private FFMPRecord qpfRecord = null;

    private boolean isNewQpf = true;

    private FFMPRecord virtualRecord = null;

    private boolean isNewVirtual = true;

    private FFMPMonitor monitor = null;

    /** The hour we are comparing currently **/
    public double time = 0.0;

    /** tells if you can update the screen **/
    private boolean isAutoRefresh = true;

    /** zoom in and show the basins in the parent **/
    private boolean isParent = true;

    /** when true use max value for aggregate **/
    private boolean isWorstCase = false;

    /** maintain the current layer in the display **/
    private boolean isMaintainLayer = false;

    /** link table to d2d frame **/
    private boolean isLinkToFrame = true;

    /** show basin **/
    private boolean showBasin = false;

    /** show stream trace **/
    private boolean showStream = true;

    /** show ffmp color display */
    private boolean showFfmpData = true;

    /** qpf split window */
    private boolean isSplit = false;

    /** aggregation for centering **/
    public Object centeredAggregationKey = null;

    /** aggregate Pfaf list **/
    public List<Long> centeredAggregatePfafList = null;

    /** table slider time **/
    private Date tableTime = null;

    // complete reset
    public boolean isQuery = true;

    /**
     * FFMP basin table dialog.
     */
    public FfmpBasinTableDlg basinTableDlg;

    /** Guidance Interpolation Map **/
    public Map<String, FFMPGuidanceInterpolation> interpolationMap;

    /** default source expiration **/
    public long qpeSourceExpiration = 0l;

    /** guidance source expiration **/
    public long guidSourceExpiration = 0l;

    /** QPF source expiration **/
    public long qpfSourceExpiration = 0l;

    /** is this a rate load **/
    public Boolean isRate = null;

    /** Are you zooming? **/
    public boolean isZooming = false;

    /** small basins on/off **/
    public boolean isSmallBasins = false;

    /** most recent time **/
    public Date mostRecentTime = null;

    /** first time load **/
    public boolean isFirst = true;

    private DrawableString basinLocatorString = null;

    private DrawableString hpeLabelString = null;

    private RGB basinTraceColor = null;

    private RGB basinBoundaryColor = null;

    /** ordered list of times **/
    private List<Date> timeOrderedKeys = new ArrayList<>();

    private boolean toKeysInitialized = false;

    /** force utility **/
    private FFFGForceUtil forceUtil = null;

    /** Restore Table flag */
    private boolean restoreTable = false;

    /** HPE bias source legend cache */
    private final Map<HpeLabelKey, String> hpeLegendMap = Collections
            .synchronizedMap(new HashMap<HpeLabelKey, String>());

    /** Lookup of Date to product ids displaying for that date */
    private final Map<Date, List<String>> hpeCacheLookup = Collections
            .synchronizedMap(new HashMap<Date, List<String>>());

    /** Flag denoting data as HPE */
    private boolean isHpe;

    /** The job to get the HPE bias source info */
    private final HpeSourceDataJob dataJob = new HpeSourceDataJob();

    private String colorMapName;

    /**
     * FFMP resource
     *
     * @param getResourceData
     *            ()
     * @param loadProperties
     */
    protected FFMPResource(FFMPResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties, false);
        getResourceData().addChangeListener(this);

        monitor = getResourceData().getMonitor();
        monitor.addResourceListener(this);

        if (getResourceData().tableLoad) {
            if (!isBasinToggle()) {
                setBasinToggle(true);
            }
            monitor.launchFFMPDialog(this);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void resourceChanged(ChangeType type, Object object) {
        if (type.equals(ChangeType.DATA_UPDATE)) {
            FFFGDataMgr.getUpdatedInstance();
            PluginDataObject[] pdos = (PluginDataObject[]) object;
            FFMPRecord ffmpRec = (FFMPRecord) pdos[pdos.length - 1];
            try {
                if (ffmpRec.getSourceName()
                        .equals(getResourceData().sourceName)) {
                    // an update clears everything
                    clear();

                    // go back an extra time step
                    Date previousMostRecentTime = null;
                    List<Date> tok = getTimeOrderedKeys();
                    if (tok.size() >= 2) {
                        previousMostRecentTime = tok.get(tok.size() - 2);
                    } else {
                        previousMostRecentTime = tok.get(0);
                    }
                    final Date refTime = ffmpRec.getDataTime().getRefTime();
                    updateTimeOrderedkeys(refTime);
                    if (getResourceData().tableLoad) {
                        setTableTime();
                    }

                    resourceData.populateRecord(ffmpRec);
                    statusHandler.handle(Priority.INFO,
                            "Updating : Previous: " + previousMostRecentTime
                                    + " New: "
                                    + ffmpRec.getDataTime().getRefTime());

                    if (getResourceData().tableLoad) {
                        List<String> hucsToLoad = new ArrayList<>();
                        hucsToLoad.add(FFMPRecord.ALL);
                        String currentHuc = getHuc();
                        if (!currentHuc.equals(FFMPRecord.ALL)) {
                            hucsToLoad.add(currentHuc);
                        }
                        UpdateLoadJob updateJob = new UpdateLoadJob(
                                resourceData, previousMostRecentTime, refTime,
                                hucsToLoad);
                        updateJob.addJobChangeListener(new JobChangeAdapter() {
                            @Override
                            public void done(IJobChangeEvent event) {
                                purge(refTime);
                                finishUpdate();
                                updateDialog();
                            }
                        });
                        updateJob.schedule();
                    } else {
                        finishUpdate();
                    }
                }
            } catch (VizException ve) {
                statusHandler.handle(Priority.PROBLEM, "Error updating record",
                        ve);
            }
        } else if (type.equals(ChangeType.DATA_REMOVE)) {
            if (object instanceof PluginDataObject[]) {
                PluginDataObject[] pdos = (PluginDataObject[]) object;
                for (PluginDataObject pdo : pdos) {
                    FFMPRecord ffmpRec = (FFMPRecord) pdo;
                    Date date = ffmpRec.getDataTime().getRefTime();
                    removeHpeLabels(date);
                }
            } else if (object instanceof DataTime) {
                DataTime dt = (DataTime) object;
                removeHpeLabels(dt.getRefTime());
            }
        } else if (type.equals(ChangeType.CAPABILITY)) {
            if (object instanceof ColorMapCapability) {
                ColorMapCapability cmCap = (ColorMapCapability) object;
                if (cmCap.getColorMapParameters() != null) {
                    String newName = cmCap.getColorMapParameters()
                            .getColorMapName();
                    if (!newName.equals(this.colorMapName)) {
                        this.colorMapName = newName;
                        colorUtilsChange();
                        for (FFMPDrawable drawable : drawables.values()) {
                            drawable.setDirty(true);
                        }
                    }
                }
            }
        }
    }

    /**
     * Remove the labels in the cache for the given time and product.
     *
     * @param date
     *            The time to remove
     * @param productId
     *            The product to remove
     */
    private void removeHpeLabels(Date date) {
        List<String> products = hpeCacheLookup.remove(date);
        if (products != null) {
            for (String product : products) {
                HpeLabelKey key = new HpeLabelKey(product, date);
                hpeLegendMap.remove(key);
            }
        }
    }

    /**
     * Finishes the last actions triggered by an update. Should run after the
     * data is loaded if the update triggered a data load.
     */
    private void finishUpdate() {
        resetRecords();
        if (getResourceData().tableLoad) {
            isFirst = true;
        }

        refresh();
    }

    /**
     * Resets the records to null and sets the boolean values relating to if the
     * data is new to true
     */
    private void resetRecords() {
        isNewQpe = true;
        isNewRate = true;
        isNewVirtual = true;
        isNewGuid = true;
        isNewQpf = true;

        qpeRecord = null;
        rateRecord = null;
        virtualRecord = null;
        guidRecord = null;
        qpfRecord = null;
    }

    @Override
    public void refresh() {
        if (!isAutoRefresh) {
            setQuery(true);
        }
        issueRefresh();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void hucChanged() {
        lowestCenter = FFMPRecord.ZOOM.WFO;

        if (isAutoRefresh()) {
            setQuery(true);
            dirty();
            issueRefresh();
        }
        perfLog.log("HUC Change complete");
    }

    @Override
    public ZOOM centerChanged(FFMPScreenCenterEvent fsce) {

        setCenter(fsce);

        if (isAutoRefresh()) {
            if (!isMaintainLayer() || isParent()) {
                setQuery(true);
            }

            refresh();
        }

        return lowestCenter;
    }

    @Override
    public void traceChanged(FFMPStreamTraceEvent fste) {
        if ((FFMPRecord.CLICK_TYPE) fste
                .getSource() == FFMPRecord.CLICK_TYPE.CLEAR) {
            // clear everything for UP&DOWN stream
            nextStreamPfaf = null;
            if (streamPfafIds != null) {
                streamPfafIds.clear();
            }
            if (streamShadedShape != null) {
                streamShadedShape.dispose();
            }
            if (streamOutlineShape != null) {
                streamOutlineShape.dispose();
            }
        } else {
            stream = (FFMPRecord.CLICK_TYPE) fste.getSource();
        }

        refresh();
    }

    /**
     * Gets the FFMPBasin by key
     *
     * @param key
     * @return
     * @throws VizException
     */
    protected FFMPBasin getBasin(Long key, FFMPRecord.FIELDS bfield,
            Date recentTime, boolean aggregate) throws VizException {
        FFMPBasin basin = null;
        if (aggregate) {
            basin = new FFMPBasin(key, aggregate);
        } else {
            FFMPRecord record = getRecord(bfield, recentTime);
            FFMPBasinData basinData = record.getBasinData();
            basin = basinData.getBasins().get(key);
        }

        return basin;
    }

    /**
     * returns the color used in drawing this basin
     *
     * @param key
     *            the aggr pfaf if is aggregate, otherwise the basin pfaf
     * @param aggregate
     * @return
     * @throws VizException
     */
    private RGB getColor(Long key, Date recentTime, boolean aggregate)
            throws VizException {
        double value = Double.NaN;

        if (centeredAggregationKey != null) {
            if (getHuc().equals(FFMPRecord.ALL)) {
                value = getBasinValue(key, recentTime, false);
                return getColorUtil().colorByValue(value);
            } else {
                if (getCenteredAggregatePfafs().contains(key) && isParent()) {
                    // this is for a reason
                } else {
                    if (!isMaintainLayer() && isParent()) {
                        return getColorUtil().colorByValue(value);
                    } else if (!isMaintainLayer() && !isParent()) {
                        value = getBasinValue(key, recentTime, false);
                    }
                }
            }
        }

        // get the actual basin values
        if (!aggregate) {
            value = getBasinValue(key, recentTime, aggregate);
        } else {
            // if worst case double check hash of save worst case
            if (isWorstCase() && checkWorstCase(key, recentTime)) {
                value = getWorstCaseValue(key, recentTime);
            } else {
                value = getBasinValue(key, recentTime, aggregate);
            }
        }
        return getColorUtil().colorByValue(value);
    }

    /**
     * Finds the basin value individually and collectively
     *
     * @param key
     * @return
     */
    private Float getBasinValue(Long key, Date recentTime, boolean aggregate) {
        Float value = Float.NaN;
        FIELDS field = getField();

        try {
            if (aggregate && isWorstCase()) {
                List<Long> pfafs = monitor.getTemplates(getSiteKey())
                        .getAggregatePfafs(key, getSiteKey(), getHuc());

                switch (field) {
                case DIFF: {
                    value = (float) getDiff(key, aggregate, recentTime, pfafs);
                    break;
                }
                case RATIO: {
                    value = (float) getRatio(key, aggregate, recentTime, pfafs);
                    break;
                }
                case RATE: {
                    value = getRateRecord(recentTime).getBasinData()
                            .getMaxValue(pfafs, recentTime);
                    break;
                }
                case QPF: {
                    value = getQpfRecord(recentTime).getBasinData()
                            .getAverageMaxValue(pfafs, recentTime,
                                    getQpfSourceExpiration());
                    break;
                }
                case GUIDANCE: {
                    long fips = monitor.getTemplates(getSiteKey())
                            .getCountyFipsByPfaf(pfafs.get(0));

                    value = getGuidanceRecord().getBasinData()
                            .getMaxGuidanceValue(pfafs,
                                    getGuidanceInterpolation(getFFGName()),
                                    getGuidSourceExpiration(getFFGName()),
                                    fips);
                    break;
                }
                case QPE: {
                    value = getQpeRecord().getBasinData().getAccumMaxValue(
                            pfafs, recentTime, getTableTime(),
                            getQpeSourceExpiration(),
                            getResourceData().getPrimarySourceXML().isRate());
                    break;
                }
                }
                // add the value to the worst case hash
                addWorstCase(key, recentTime, value);

            } else {
                List<Long> pfafs = null;

                if (aggregate) {
                    pfafs = monitor.getTemplates(getSiteKey())
                            .getAggregatePfafs(key, getSiteKey(), getHuc());
                }

                if (getResourceData().tableLoad) {
                    switch (field) {
                    case DIFF: {
                        value = (float) getDiff(key, aggregate, recentTime,
                                pfafs);
                        break;
                    }
                    case RATIO: {
                        value = (float) getRatio(key, aggregate, recentTime,
                                pfafs);
                        break;
                    }
                    case RATE:
                        if (aggregate) {
                            value = getRateRecord(recentTime).getBasinData()
                                    .getAverageValue(pfafs, recentTime);
                        } else {
                            value = getBasin(key, field, recentTime, aggregate)
                                    .getValue(recentTime);
                        }

                        break;
                    case QPF: {
                        if (aggregate) {
                            value = getQpfRecord(recentTime).getBasinData()
                                    .getAverageValue(pfafs, recentTime,
                                            getQpfSourceExpiration());
                        } else {
                            value = getBasin(key, field, recentTime, aggregate)
                                    .getAverageValue(recentTime,
                                            getQpfSourceExpiration());
                        }
                        break;
                    }
                    case GUIDANCE: {
                        if (aggregate) {
                            getGuidanceRecord().getBasinData()
                                    .getAverageGuidanceValue(pfafs,
                                            getGuidanceInterpolation(
                                                    getFFGName()),
                                            getGuidSourceExpiration(
                                                    getFFGName()));
                        } else {
                            value = getGuidanceValue(
                                    (FFMPGuidanceBasin) getBasin(key, field,
                                            recentTime, aggregate),
                                    recentTime, getFFGName());
                        }
                        break;
                    }
                    case QPE: {
                        if (aggregate) {
                            value = getQpeRecord().getBasinData()
                                    .getAccumAverageValue(pfafs, getTableTime(),
                                            recentTime,
                                            getQpeSourceExpiration(),
                                            getResourceData()
                                                    .getPrimarySourceXML()
                                                    .isRate());
                        } else {
                            value = getBasin(key, field, recentTime, aggregate)
                                    .getAccumValue(getTableTime(), recentTime,
                                            getQpeSourceExpiration(),
                                            getResourceData()
                                                    .getPrimarySourceXML()
                                                    .isRate());
                        }
                        break;
                    }
                    }
                } else {
                    switch (field) {
                    case QPF: {
                        value = getBasin(key, field, recentTime, aggregate)
                                .getValue(recentTime);
                        break;
                    }
                    case GUIDANCE: {
                        value = getGuidanceValue(
                                (FFMPGuidanceBasin) getBasin(key, field,
                                        recentTime, aggregate),
                                recentTime, getFFGName());
                        break;
                    }
                    }
                }
            }
        } catch (Exception e) {
            statusHandler.error("Problem getting basin value", e);
        }
        return value;
    }

    private float forceValue(List<Long> pfafs, FFMPBasin basin,
            float unforcedValue) {
        float value = unforcedValue;
        String ffgType = getFFGName();

        if (forceUtil == null) {
            forceUtil = new FFFGForceUtil(this, getFFGName());
        }

        forceUtil.setSliderTime(this.getTime());

        if (pfafs != null) {
            ForceUtilResult forceResult = forceUtil.calculateForcings(pfafs,
                    monitor.getTemplates(getSiteKey()), basin);

            List<Long> forcedPfafs = forceResult.getForcedPfafList();
            List<Long> pfafList = forceResult.getPfafList();
            boolean forced = forceResult.isForced();
            if (!forcedPfafs.isEmpty() && forced) {
                // Recalculate the guidance using the forced value(s)
                value = guidRecord.getBasinData().getAverageGuidanceValue(
                        pfafList, this.getGuidanceInterpolation(ffgType),
                        new Float(value), forcedPfafs,
                        getGuidSourceExpiration(ffgType));
            } else if (!forcedPfafs.isEmpty()) {
                value = guidRecord.getBasinData().getAverageGuidanceValue(
                        pfafList, this.getGuidanceInterpolation(ffgType),
                        Float.NaN, forcedPfafs,
                        getGuidSourceExpiration(ffgType));
            }
        }

        return value;
    }

    @Override
    public String getName() {
        StringBuilder prefix = new StringBuilder();
        try {
            FIELDS sfield = getField();
            if (sfield == FFMPRecord.FIELDS.RATIO
                    || sfield == FFMPRecord.FIELDS.DIFF) {
                sfield = FFMPRecord.FIELDS.QPE;
            }

            prefix.append("ffmp");
            prefix.append(" ");
            prefix.append(resourceData.siteKey);
            prefix.append(" ");
            if (getResourceData().tableLoad) {
                prefix.append("Table Display ");
            } else {
                prefix.append("Image ");
                SourceXML source = monitor.getSourceConfig()
                        .getSource(getResourceData().sourceName);
                if (source.isGuidance()) {
                    prefix.append(source.getDisplayName()).append(" ")
                            .append(source.getDurationHour()).append(" HR");
                } else {
                    prefix.append(source.getDisplayName());
                }
            }
        } catch (Exception e) {
            /*
             * TODO: determine what exception is actually being handled and
             * possibly fix the cause. Guessing it's a NullPointerException.
             */
            statusHandler.warn("Error getting resource name", e);
            return "No Data Available";
        }

        return prefix.toString();
    }

    /**
     * Returns the ALL huc if worst case, otherwise the huc
     *
     * @return
     */
    private String getHucIfWorstCase() {
        String huc = null;
        if (isWorstCase()) {
            huc = FFMPRecord.ALL;
        } else {
            huc = getHuc();
        }
        return huc;
    }

    /**
     * Gets the record currently used
     *
     * @param recentTime
     *
     * @return FFMPCacheRecord
     */
    public FFMPRecord getRateRecord(Date recentTime) {

        if (rateRecord == null && isNewRate) {
            try {
                rateRecord = monitor.getRateRecord(getProduct(), getSiteKey(),
                        getDataKey(), getPrimarySource(), recentTime, false);
                isNewRate = false;
            } catch (Exception e) {
                statusHandler.error("Error retrieving the current rate record",
                        e);
            }
        }
        return rateRecord;
    }

    /**
     * Gets the record currently used
     *
     * @return FFMPCacheRecord
     */
    public FFMPRecord getQpeRecord() {
        try {
            if (qpeRecord == null && getTableTime() != null && isNewQpe) {
                qpeRecord = monitor.getQPERecord(getProduct(), getSiteKey(),
                        getDataKey(), getPrimarySource(), getTableTime(),
                        false);
                isNewQpe = false;
            }
        } catch (Exception e) {
            statusHandler.error("Error retrieving the current QPE record", e);
        }

        return qpeRecord;
    }

    /**
     * Gets the record currently used
     *
     * @return FFMPCacheRecord
     */
    public FFMPRecord getGuidanceRecord() {
        try {
            if (guidRecord == null || isNewGuid) {
                Date date = null;
                String sourceName = getResourceData().sourceName;
                // Stand alone displays
                boolean isStandAlone = false;
                if (getResourceData().tableLoad) {
                    date = getTableTime();
                } else {
                    if (paintTime != null) {
                        date = paintTime.getRefTime();
                    }
                    isStandAlone = true;
                }

                guidRecord = monitor.getGuidanceRecord(getProduct(),
                        getSiteKey(), sourceName, date, isStandAlone);
                isNewGuid = false;
            }

        } catch (Exception e) {
            statusHandler.error("Error retrieving the current guid record", e);
        }

        return guidRecord;
    }

    /**
     * Gets the record currently used
     *
     * @return FFMPReFFMPCacheRecordcord
     */
    public FFMPRecord getQpfRecord(Date recentTime) {
        try {
            if (qpfRecord == null && isNewQpf) {
                Date date = null;
                // Stand alone displays
                if (getResourceData().tableLoad) {
                    date = recentTime;
                } else {
                    if (paintTime != null) {
                        date = paintTime.getRefTime();
                    }
                }

                qpfRecord = monitor.getQPFRecord(getProduct(), getSiteKey(),
                        getDataKey(), getPrimarySource(), date, false);
                isNewQpf = false;
            }
        } catch (Exception e) {
            statusHandler.error("Error retrieving the current QPF record", e);
        }

        return qpfRecord;
    }

    /**
     * Gets the record currently used
     *
     * @return FFMPCacheRecord
     */
    public FFMPRecord getVirtualRecord() {
        try {
            if (virtualRecord == null && isNewVirtual) {
                virtualRecord = monitor.getVirtualRecord(getProduct(),
                        getSiteKey(), getDataKey(), getPrimarySource(),
                        getTableTime(), false);
                isNewVirtual = false;
            }

        } catch (Exception e) {
            statusHandler.error("Error retrieving the current virtual record",
                    e);
        }

        return virtualRecord;
    }

    /**
     * General get record call
     *
     * @param pfield
     * @param recentTime
     *
     * @return FFMPCacheRecord
     */
    public FFMPRecord getRecord(FIELDS pfield, Date recentTime) {
        if (pfield == FIELDS.GUIDANCE) {
            return getGuidanceRecord();
        } else if (pfield == FIELDS.RATIO) {
            return getGuidanceRecord();
        } else if (pfield == FIELDS.DIFF) {
            return getGuidanceRecord();
        } else if (pfield == FIELDS.QPE) {
            return getQpeRecord();
        } else if (pfield == FIELDS.QPF) {
            return getQpfRecord(recentTime);
        } else if (pfield == FIELDS.RATE) {
            return getRateRecord(recentTime);
        }

        return null;
    }

    @Override
    protected void disposeInternal() {
        if (this.getName().indexOf("Table Display") > -1) {
            if (basinTableDlg != null) {
                closeDialog();
            }

            HucLevelGeometriesFactory.getInstance().clear();

            if (monitor != null) {
                monitor.forceKillFFMPSplash();
            }
        }

        resetRecords();

        if (monitor.getResourceListenerList().size() == 1) {
            // free up the monitor which holds most of the memory
            FFMPMonitor tempMonitor = monitor;
            monitor = null;
            tempMonitor.nullifyMonitor();
        } else {
            monitor.removeResourceListener(this);
        }

        recycleInternal();
    }

    /**
     * Removes the mouse adapter and disposes of all the graphics objects.
     */
    @Override
    protected void recycleInternal() {
        IDisplayPaneContainer container = getResourceContainer();
        if (container != null) {
            container.unregisterMouseHandler(inspectAdapter);
        }

        if (font != null) {
            font.dispose();
            font = null;
        }

        if (xfont != null) {
            xfont.dispose();
            xfont = null;
        }

        // dispose of shapes
        if (smallBasinOverlayShape != null) {
            smallBasinOverlayShape.dispose();
            smallBasinOverlayShape = null;
        }
        if (streamShadedShape != null) {
            streamShadedShape.dispose();
            streamShadedShape = null;
        }
        if (streamOutlineShape != null) {
            streamOutlineShape.dispose();
            streamOutlineShape = null;
        }
        shadedShapes.dispose();

        // clear takes care of the drawables
        clear();

        for (PixelCoverage px : vgbDrawables.values()) {
            px.dispose();
        }
        vgbDrawables.clear();

    }

    /**
     * DR 14522 fixing: enclosing font setting into GUI thread to avoid invalid
     * thread access.
     */
    @Override
    protected void initInternal(final IGraphicsTarget target)
            throws VizException {
        EditableManager.makeEditable(this,
                getCapability(EditableCapability.class).isEditable());
        IDisplayPaneContainer container = getResourceContainer();
        if (container != null) {
            container.registerMouseHandler(inspectAdapter);
        }

        try {

            if (getTimeOrderedKeys() != null
                    && !getTimeOrderedKeys().isEmpty()) {
                // change when updated
                getResourceData().addChangeListener(this);
                queryJob = new FFMPDataRetrievalJob();
                df = new DecimalFormat();
                df.setMinimumIntegerDigits(1);
                df.setMaximumFractionDigits(2);
                df.setMinimumFractionDigits(1);
                df.setDecimalSeparatorAlwaysShown(true);
            }

        } catch (Exception ex) {
            statusHandler.handle(Priority.PROBLEM, "Error opening FFMP", ex);
        }

        // DR 14522: use Display.getDefault().asyncExec() for GUI thread.
        Display.getDefault().asyncExec(new Runnable() {

            @Override
            public void run() {

                if (font == null) {
                    font = target.initializeFont("Dialog", 9, null);
                }

                font.setMagnification(
                        getCapability(MagnificationCapability.class)
                                .getMagnification().floatValue());

                if (xfont == null) {
                    IFont.Style[] styles = new IFont.Style[] {
                            IFont.Style.BOLD };
                    xfont = target.initializeFont("Monospace", 10, styles);
                }

                xfont.setMagnification(
                        getCapability(MagnificationCapability.class)
                                .getMagnification().floatValue());

                basinLocatorString = new DrawableString("X",
                        new RGB(255, 255, 255));
                basinLocatorString.font = xfont;
                basinLocatorString.horizontalAlignment = HorizontalAlignment.CENTER;
                basinLocatorString.verticallAlignment = VerticalAlignment.MIDDLE;
                basinLocatorString.addTextStyle(TextStyle.BLANKED);

                hpeLabelString = new DrawableString("",
                        getCapability(ColorableCapability.class).getColor());
                hpeLabelString.font = font;
                hpeLabelString.horizontalAlignment = HorizontalAlignment.CENTER;
                hpeLabelString.verticallAlignment = VerticalAlignment.TOP;

            }
        });

        // Set flag for HPE data
        isHpe = resourceData.dataKey.equalsIgnoreCase(HPE)
                || resourceData.dataKey.equalsIgnoreCase(BHPE);

        ExtraTextResourceData.addExtraTextResource(descriptor);
    }

    /**
     * @param screenExtent
     * @return
     */
    protected PixelExtent getExpandedExtent(PixelExtent screenExtent) {
        PixelExtent expandedExtent = screenExtent.clone();
        expandedExtent.getEnvelope().expandBy(
                expandedExtent.getWidth() * EXPANSION_FACTOR,
                expandedExtent.getHeight() * EXPANSION_FACTOR);
        return expandedExtent;
    }

    @Override
    protected void paintInternal(IGraphicsTarget aTarget,
            PaintProperties paintProps) throws VizException {
        if (getTimeOrderedKeys() == null || getTimeOrderedKeys().isEmpty()
                || getDomains() == null) {
            return;
        }

        aTarget.clearClippingPlane();
        try {

            paintTime = paintProps.getDataTime();
            paintProps.setAlpha(
                    getCapability(ImagingCapability.class).getAlpha());

            FFMPDrawable drawable = null;

            if (paintTime != null) {
                if (!drawables.containsKey(paintTime)) {

                    drawable = new FFMPDrawable(getDomains());
                    drawables.put(paintTime, drawable);
                } else {
                    // we found it!
                    drawable = drawables.get(paintTime);

                    if (!paintTime.equals(drawable.getTime())) {
                        drawable.setDirty(true);
                    }

                    // auto refresh state
                    if (isQuery) {
                        drawable.setDirty(true);
                    }
                }

                if (getResourceData().tableLoad && !paintTime.getRefTime()
                        .equals(getMostRecentTime())) {
                    setMostRecentTime(paintTime.getRefTime());
                    setTableTime();
                    // if (isLinkToFrame && loader != null && loader.loadType !=
                    // LOADER_TYPE.GENERAL) {
                    if (isLinkToFrame) {
                        updateDialog();
                    }
                }
            } else {
                getResourceData().getMonitor().forceKillFFMPSplash();
            }

            if (drawable != null && drawable.isDirty()) {
                /*
                 * only need to do the query if extent or colormap changed,
                 * pfafs may be fine
                 */
                if (!isFirst || queryJob.getState() == Job.NONE) {
                    queryJob.request(aTarget, drawable, paintTime);
                }
            }

            if (drawable != null && isFfmpDataToggle()) {
                IColormapShadedShapeExtension ext = aTarget
                        .getExtension(IColormapShadedShapeExtension.class);
                ImagingCapability imageCap = getCapability(
                        ImagingCapability.class);
                float brightness = imageCap.getBrightness();
                float alpha = imageCap.getAlpha();
                for (DomainXML domain : getDomains()) {
                    String cwa = domain.getCwa();
                    IColormapShadedShape shape = shadedShapes
                            .getDrawableShape(cwa, drawable.getShadedHuc());
                    Map<Object, RGB> colorMap = drawable.getColorMap(cwa);
                    if (shape != null && colorMap != null) {
                        ext.drawColormapShadedShape(shape, colorMap, alpha,
                                brightness);
                    }
                }
            }

            boolean isAllHuc = getHuc().equals(FFMPRecord.ALL);
            if (getResourceData().tableLoad) {

                int mapWidth = getDescriptor().getMapWidth() / 1000;
                double zoom = getDescriptor().getRenderableDisplay().getZoom();

                // determine whether or not to draw the small guys
                if (mapWidth * zoom > 250.0) {
                    if (isSmallBasins) {
                        isSmallBasins = false;
                        refresh();
                    }

                } else if (mapWidth * zoom < 250.0) {
                    if (!isSmallBasins) {
                        isSmallBasins = true;
                        if (smallBasinOverlayShape == null) {
                            drawable.setDirty(true);
                        } else {
                            refresh();
                        }
                    }
                }

                if (isSmallBasins && this.isBasinToggle()) {
                    OutlineCapability lineCap = getCapability(
                            OutlineCapability.class);
                    if (smallBasinOverlayShape != null
                            && smallBasinOverlayShape.isDrawable()) {

                        if (basinBoundaryColor == null) {
                            basinBoundaryColor = getCapability(
                                    ColorableCapability.class).getColor();
                        }

                        aTarget.drawWireframeShape(smallBasinOverlayShape,
                                basinBoundaryColor, lineCap.getOutlineWidth(),
                                lineCap.getLineStyle());
                    } else if (smallBasinOverlayShape == null
                            && lineCap.isOutlineOn()) {
                        issueRefresh();
                    }
                }
            }
            // re-centered ?
            if (centeredAggregationKey != null) {
                vgbDrawables.clear();
                // create pixelCoverages for the VGB's
                if (isAllHuc) {
                    for (DomainXML domain : getDomains()) {
                        for (Long pfaf : monitor.getTemplates(getSiteKey())
                                .getMap(getSiteKey(), domain.getCwa(), getHuc())
                                .keySet()) {
                            List<FFMPVirtualGageBasinMetaData> fvgmdList = monitor
                                    .getTemplates(getSiteKey())
                                    .getVirtualGageBasinMetaData(getSiteKey(),
                                            domain.getCwa(), pfaf);
                            if (fvgmdList != null) {
                                for (FFMPVirtualGageBasinMetaData fvgmd : fvgmdList) {
                                    vgbDrawables.put(fvgmd.getLid(),
                                            getPixelCoverage(
                                                    fvgmd.getCoordinate(),
                                                    paintProps));
                                }
                            }
                        }
                    }
                } else {
                    if (lowestCenter == FFMPRecord.ZOOM.AGGREGATE) {
                        for (Long pfaf : monitor.getTemplates(getSiteKey())
                                .getAllAggregatePfafs(centeredAggregationKey,
                                        getHuc())) {
                            List<FFMPVirtualGageBasinMetaData> fvgmdList = monitor
                                    .getTemplates(getSiteKey())
                                    .getVirtualGageBasinMetaData(getSiteKey(),
                                            null, pfaf);
                            if (fvgmdList != null) {
                                for (FFMPVirtualGageBasinMetaData fvgmd : fvgmdList) {
                                    vgbDrawables.put(fvgmd.getLid(),
                                            getPixelCoverage(
                                                    fvgmd.getCoordinate(),
                                                    paintProps));
                                }
                            }
                        }
                    } else {
                        for (DomainXML domain : getDomains()) {
                            for (Entry<String, FFMPVirtualGageBasinMetaData> entry : monitor
                                    .getTemplates(getSiteKey())
                                    .getVirtualGageBasins(getSiteKey(),
                                            domain.getCwa())
                                    .entrySet()) {
                                if (entry.getValue() != null) {
                                    vgbDrawables.put(entry.getKey(),
                                            getPixelCoverage(
                                                    entry.getValue()
                                                            .getCoordinate(),
                                                    paintProps));
                                }
                            }
                        }
                    }
                }

                paintCenter(aTarget, paintProps);
                paintVGBs(aTarget, paintProps);
            }

            // draw or clear the colorMap
            if (!isFfmpDataToggle()) {
                // clear if ffmpDataToggle is false
                getCapability(ColorMapCapability.class)
                        .setColorMapParameters(null);
            } else if (getColorUtil().getColorMapParameters() != null) {
                // restore if null
                getCapability(ColorMapCapability.class).setColorMapParameters(
                        getColorUtil().getColorMapParameters());
            }

            // draw stream trace?
            if (isShowStream() && isStreamFollow()) {
                paintUpAndDownStream(aTarget, paintProps);
            }

            // draw hpe strings if HPE
            if (isHpe) {
                // Paint the HPE bias source text if HPE
                String text = getHpeText(paintTime.getRefTime());

                if (text != null && text.trim().length() > 0) {
                    double[] pixel = paintProps.getView().getDisplayCoords(
                            new double[] { 110, 120 }, aTarget);
                    hpeLabelString.setText(text,
                            getCapability(ColorableCapability.class)
                                    .getColor());
                    hpeLabelString.setCoordinates(pixel[0], pixel[1]);
                    aTarget.drawStrings(hpeLabelString);

                }
            }

            // always reset
            isQuery = false;
        } finally {
            aTarget.setupClippingPlane(paintProps.getClippingPane());
        }
    }

    /**
     * paint X at center of basin chosen
     *
     * @param target
     * @param paintProps
     * @throws VizException
     */
    private void paintCenter(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
        double[] world = this.descriptor
                .worldToPixel(new double[] { center.x, center.y });
        basinLocatorString.setCoordinates(world[0], world[1]);
        target.drawStrings(basinLocatorString);
    }

    /**
     * Paints the up and down stream basins, based on what is chosen the
     * traceGeometries hash is critical here
     *
     * @param target
     * @param paintProps
     * @throws VizException
     */
    private void paintUpAndDownStream(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {

        if (basinTraceColor == null) {
            basinTraceColor = getCapability(ColorableCapability.class)
                    .getColor();
        }

        if (isShowStream() && streamShadedShape != null
                && streamShadedShape.isDrawable()) {
            target.drawShadedShape(streamShadedShape, paintProps.getAlpha());
        }
        if (isShowStream() && streamOutlineShape != null
                && streamOutlineShape.isDrawable()
                && getCapability(OutlineCapability.class).isOutlineOn()) {
            target.drawWireframeShape(streamOutlineShape, basinTraceColor,
                    getCapability(OutlineCapability.class).getOutlineWidth(),
                    getCapability(OutlineCapability.class).getLineStyle());
        } else if (streamOutlineShape == null
                && getCapability(OutlineCapability.class).isOutlineOn()) {
            target.setNeedsRefresh(true);
        }
    }

    @Override
    public void project(CoordinateReferenceSystem mapData) throws VizException {

        if (shadedShapes != null) {
            shadedShapes.dispose();
        }

        if (streamShadedShape != null) {
            streamShadedShape.dispose();
            streamShadedShape = null;
        }
        if (streamOutlineShape != null) {
            streamOutlineShape.dispose();
            streamOutlineShape = null;
        }

        if (smallBasinOverlayShape != null) {
            smallBasinOverlayShape.dispose();
            smallBasinOverlayShape = null;
        }

        setQuery(true);
        refresh();
    }

    protected String getGeometryType() {
        if (geometryType == null) {
            try {
                int p = getResourceData().getTables()[0].indexOf('.');
                String schema = getResourceData().getTables()[0].substring(0,
                        p);
                String table = getResourceData().getTables()[0]
                        .substring(p + 1);
                StringBuilder query = new StringBuilder(
                        "SELECT type FROM geometry_columns WHERE f_table_schema='");
                query.append(schema);
                query.append("' AND f_table_name='");
                query.append(table);
                query.append("' LIMIT 1;");
                List<Object[]> results = DirectDbQuery.executeQuery(
                        query.toString(), "maps", QueryLanguage.SQL);

                geometryType = (String) results.get(0)[0];
            } catch (Throwable e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error querying geometry type", e);
            }
        }

        return geometryType;
    }

    /**
     * Get color wrapper class
     *
     * @return
     * @throws VizException
     */
    private FFMPColorUtils getColorUtil() throws VizException {
        if (colorUtil == null) {
            boolean tableLoad = getResourceData().tableLoad;
            String ffg = getFFGName();
            if (!tableLoad) {
                SourceXML source = FFMPSourceConfigurationManager.getInstance()
                        .getSource(ffg);
                ffg = source.getSourceFamily();
            }

            colorUtil = new FFMPColorUtils(getField(), getTime(), ffg,
                    tableLoad, this.colorMapName);
            StyleRule sr = colorUtil.getStyleRule();

            float brightness = 0.8f;
            ImagingCapability imgCap = getCapability(ImagingCapability.class);
            if (imgCap.isBrightnessSet()) {
                brightness = imgCap.getBrightness();
            } else if (sr.getPreferences() instanceof ImagePreferences) {
                ImagePreferences imgPrefs = (ImagePreferences) sr
                        .getPreferences();
                if (imgPrefs.getBrightness() != null) {
                    brightness = imgPrefs.getBrightness();
                }
            }
            imgCap.setBrightness(brightness);
        }
        return colorUtil;
    }

    /**
     * Inspect may frequently be implemented by calling interrogate, and
     * formatting the result as a string.
     *
     * @param coord
     *            the coordinate
     *
     * @return a string that represents the result of inspecting that point
     *
     * @throws VizException
     */
    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        // No inspection by default
        StringBuilder buf = new StringBuilder();
        Long pfaf = null;
        boolean aggregate = false;
        try {
            FFMPBasinMetaData metaBasin = monitor.getTemplates(getSiteKey())
                    .findBasinByLatLon(getSiteKey(), coord.asLatLon());
            if (metaBasin != null) {
                if (getHuc().equals(FFMPRecord.ALL)
                        || centeredAggregationKey != null) {
                    pfaf = metaBasin.getPfaf();
                    if (isMaintainLayer) {
                        pfaf = monitor.getTemplates(getSiteKey())
                                .findAggregatedPfaf(pfaf, getSiteKey(),
                                        getHuc());
                        aggregate = true;
                    }
                } else {
                    pfaf = monitor.getTemplates(getSiteKey())
                            .findAggregatedPfaf(metaBasin.getPfaf(),
                                    getSiteKey(), getHuc());
                    if (pfaf == null) {
                        pfaf = metaBasin.getPfaf();
                    } else {
                        aggregate = true;
                    }
                }
                if (pfaf != null) {
                    if (!"COUNTY".equals(getHuc())
                            || centeredAggregationKey != null) {
                        buf.append("Pfaf ID:  ").append(pfaf).append(NL);
                        buf.append("Basin Name:  ")
                                .append(metaBasin.getStreamName()).append(NL);
                        buf.append("Root River:  ")
                                .append(metaBasin.getHucName()).append(NL);

                    }
                    buf.append("County:  ").append(metaBasin.getState())
                            .append(", ");
                    buf.append(metaBasin.getCounty()).append(NL);
                    String valst = null;
                    Float val = getBasinValue(pfaf, getPaintTime().getRefTime(),
                            aggregate);
                    if (val.isNaN() || val == FFMPUtils.MISSING) {
                        valst = NO_DATA;
                    } else {
                        valst = df.format(getBasinValue(pfaf,
                                getPaintTime().getRefTime(), aggregate));
                    }

                    if (!NO_DATA.equals(valst)) {
                        buf.append(getField().getFieldName()).append(":  ");
                        buf.append(valst).append(" ")
                                .append(FFMPRecord.getUnitType(getField()));
                    } else {
                        buf.append(getField().getFieldName()).append(":  ")
                                .append(valst);
                    }
                }
            }
            // For DR 10704 I am turning off sampling for VGBs. If we are
            // directed to turn it back on for some reason then we can just
            // uncomment this code
            // if (centeredAggregationKey != null) {
            // for (String lid : vgbDrawables.keySet()) {
            // if (contains(vgbDrawables.get(lid), coord.asLatLon())) {
            // FFMPVirtualGageBasinMetaData vgbmd = monitor
            // .getTemplates(getSiteKey())
            // .getVirtualGageBasinMetaData(getSiteKey(), lid);
            // Double val = (double) getVGBValue(vgbmd.getLookupId(),
            // paintTime.getRefTime());
            // buf.append("\n");
            // buf.append("Virtual Gage Basin: " + lid + "\n");
            // buf.append("Name: " + vgbmd.getName() + "\n");
            // String valst = null;
            // if (val.isNaN() || (val == FFMPUtils.MISSING)) {
            // valst = getField().getFieldName() + ": NO DATA";
            // } else {
            // valst = getField().getFieldName() + ": "
            // + df.format(val) + " "
            // + FFMPRecord.getUnitType(getField());
            // }
            //
            // buf.append(valst);
            // }
            // }
            // }
        } catch (TransformException | FactoryException e) {
            statusHandler.debug("Error in FFMP inspect.", e);
            statusHandler.warn(
                    "An error occured while inspecting the FFMP data.", e);
        }
        return buf.toString();
    }

    /**
     * is it following the stream capable
     *
     * @return Stream follow
     */
    public boolean isStreamFollow() {
        return getCapability(EditableCapability.class).isEditable();
    }

    /**
     * Sets the screen center from the table
     *
     * @param center
     */
    private void setCenter(FFMPScreenCenterEvent event) {
        boolean isUpdateDialog = true;

        int mapWidth = getDescriptor().getMapWidth() / 1000;
        FFMPTemplates templates = monitor.getTemplates(getSiteKey());
        String huc = getHuc();
        boolean isAllHuc = huc.equals(FFMPRecord.ALL);

        Object pfaf = event.getSource();
        if (centeredAggregationKey == null) {
            centeredAggregationKey = pfaf;
            if (!isAllHuc) {
                center = templates.findAggregationCenter(
                        (Long) centeredAggregationKey, getSiteKey(), huc);

            } else {
                center = getCenter(pfaf);
                isUpdateDialog = false;
            }

            getRenderableDisplays().forEach(d -> d.getExtent().reset());
            float zoomLevel = 0.0f;

            if (!isAllHuc) {
                if (lowestCenter == FFMPRecord.ZOOM.WFO) {
                    clearAllHuc();
                }
                lowestCenter = FFMPRecord.ZOOM.AGGREGATE;
                zoomLevel = (float) AGGREGATE_ZOOM / mapWidth;
            } else {
                lowestCenter = FFMPRecord.ZOOM.BASIN;
                zoomLevel = (float) BASIN_ZOOM / mapWidth;
                isUpdateDialog = false;
            }

            zoom(zoomLevel);

        } else if (!centeredAggregationKey.equals(pfaf)) {
            centeredAggregationKey = pfaf;
            if (lowestCenter == FFMPRecord.ZOOM.WFO) {
                if (!isAllHuc) {
                    center = templates.findAggregationCenter(
                            (Long) centeredAggregationKey, getSiteKey(),
                            getHuc());
                } else {
                    center = getCenter(pfaf);
                    isUpdateDialog = false;
                }

                getRenderableDisplays().forEach(d -> d.getExtent().reset());
                float zoomLevel = 0.0f;

                if (!isAllHuc) {
                    clearTables();
                    lowestCenter = FFMPRecord.ZOOM.AGGREGATE;
                    zoomLevel = (float) AGGREGATE_ZOOM / mapWidth;
                } else {
                    lowestCenter = FFMPRecord.ZOOM.BASIN;
                    zoomLevel = (float) BASIN_ZOOM / mapWidth;
                    isUpdateDialog = false;
                }

                zoom(zoomLevel);

            } else {
                center = getCenter(pfaf);
                if (lowestCenter != FFMPRecord.ZOOM.BASIN) {
                    getRenderableDisplays().forEach(d -> d.getExtent().reset());
                    float zoomLevel = (float) BASIN_ZOOM / mapWidth;
                    zoom(zoomLevel);
                    lowestCenter = FFMPRecord.ZOOM.BASIN;
                }
            }

        } else {
            centeredAggregationKey = pfaf;
            center = getCenter(pfaf);
            isUpdateDialog = false;
            lowestCenter = FFMPRecord.ZOOM.BASIN;
        }

        if (getResourceData().tableLoad) {

            if (isUpdateDialog) {
                updateDialog();
            }

            // stops the annoying wait cursor every time you re-center
            if (isAllHuc || lowestCenter == FFMPRecord.ZOOM.BASIN) {
                basinTableDlg.getShell().setCursor(null);
            }
        }

        // reset the screen as if it were a pan
        if (center != null) {
            getRenderableDisplays().forEach(
                    d -> d.recenter(new double[] { center.x, center.y }));
        }
    }

    /**
     * Zoom the screen
     *
     * @param zoomFactor
     */
    public void zoom(float zoomFactor) {
        getRenderableDisplays().forEach(d -> d.zoom(zoomFactor));
    }

    /**
     * @return list of all renderable displays in this resource's container
     */
    private List<IRenderableDisplay> getRenderableDisplays() {
        return Arrays.stream(getResourceContainer().getDisplayPanes())
                .map(pane -> pane.getRenderableDisplay())
                .collect(Collectors.toList());
    }

    /**
     *
     * @param latLon
     */
    protected void traceClick(Coordinate latLon) {
        FFMPBasinMetaData basin = monitor.getTemplates(getSiteKey())
                .findBasinByLatLon(getSiteKey(), latLon);
        if (basin != null) {
            Long newPfaf = basin.getPfaf();

            if (stream.equals(FFMPRecord.CLICK_TYPE.TREND)) {
                monitor.basinTrend(newPfaf);
            } else {
                // only re-draw the screen for UP/DOWN
                nextStreamPfaf = newPfaf;
                dirty();
            }
        } else {
            Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getShell();
            MessageBox mb = new MessageBox(shell,
                    SWT.ICON_INFORMATION | SWT.OK);
            mb.setText("FFMP:  Invalid Basin Selection");
            mb.setMessage(
                    "The clicked basin is not in the list of FFMP basins.  Re-select one!");
            mb.open();

        }

        refresh();
    }

    /**
     * Gets the diff
     *
     * @param key
     * @param aggregate
     * @return
     */
    private double getDiff(Long key, boolean aggregate, Date recentTime,
            List<Long> pfafs) {

        float qpe = 0.0f;
        float guid = 0.0f;
        float diff = Float.NaN;
        String ffgType = getFFGName();

        FFMPRecord qpfRecord = this.getQpfRecord(getPaintTime().getRefTime());
        FFMPBasinData qpfBasin = null;
        if (qpfRecord != null) {
            qpfBasin = qpfRecord.getBasinData();
        }

        try {
            if (aggregate) {
                if (isWorstCase()) {
                    List<Float> qpes = null;
                    List<Float> guids = null;

                    if (getQpeRecord() != null && getGuidanceRecord() != null) {
                        qpes = getQpeRecord().getBasinData().getAccumValues(
                                pfafs, getTableTime(), recentTime,
                                getQpeSourceExpiration(), isRate());

                        guids = getGuidanceRecord().getBasinData()
                                .getGuidanceValues(pfafs,
                                        getGuidanceInterpolation(ffgType),
                                        getGuidSourceExpiration(ffgType));
                    }

                    if (qpes != null && guids != null) {
                        diff = FFMPUtils.getMaxDiffValue(qpes, guids);
                    }
                } else {
                    if (getQpeRecord() != null && getGuidanceRecord() != null) {
                        qpe = getQpeRecord().getBasinData()
                                .getAccumAverageValue(pfafs, getTableTime(),
                                        recentTime, getQpeSourceExpiration(),
                                        getResourceData().getPrimarySourceXML()
                                                .isRate());

                        guid = getGuidanceRecord().getBasinData()
                                .getAverageGuidanceValue(pfafs,
                                        getGuidanceInterpolation(ffgType),
                                        getGuidSourceExpiration(ffgType));

                        diff = FFMPUtils.getDiffValue(qpe, guid);
                    }
                }
            } else {
                if (getQpeRecord() != null && getGuidanceRecord() != null) {
                    qpe = getQpeRecord().getBasinData().get(key).getAccumValue(
                            getTableTime(), recentTime,
                            getQpeSourceExpiration(), isRate());

                    guid = getGuidanceValue(
                            (FFMPGuidanceBasin) getGuidanceRecord()
                                    .getBasinData().get(key),
                            recentTime, ffgType);
                    guid = forceValue(pfafs,
                            getBasin(key, getField(), recentTime, aggregate),
                            guid);

                    diff = FFMPUtils.getDiffValue(qpe, guid);
                }
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR, "Error caculating Diff", e);
        }
        return diff;
    }

    /**
     * Gets the ratio
     *
     * @param qpe
     * @param guid
     * @return
     */
    private double getRatio(Long key, boolean aggregate, Date recentTime,
            List<Long> pfafs) {

        float qpe = 0.0f;
        float guid = 0.0f;
        float ratio = Float.NaN;
        String ffgType = getFFGName();

        try {
            if (aggregate) {
                if (isWorstCase()) {
                    List<Float> qpes = null;
                    List<Float> guids = null;
                    if (getQpeRecord() != null) {
                        qpes = getQpeRecord().getBasinData().getAccumValues(
                                pfafs, getTableTime(), recentTime,
                                getQpeSourceExpiration(), isRate());
                    }
                    if (getGuidanceRecord() != null) {
                        guids = getGuidanceRecord().getBasinData()
                                .getGuidanceValues(pfafs,
                                        getGuidanceInterpolation(ffgType),
                                        getGuidSourceExpiration(ffgType));
                    }
                    if (qpes != null && guids != null) {
                        ratio = FFMPUtils.getMaxRatioValue(qpes, guids);
                    }
                } else {
                    if (getQpeRecord() != null && getGuidanceRecord() != null) {
                        qpe = getQpeRecord().getBasinData()
                                .getAccumAverageValue(pfafs, getTableTime(),
                                        recentTime, getQpeSourceExpiration(),
                                        getResourceData().getPrimarySourceXML()
                                                .isRate());

                        guid = getGuidanceRecord().getBasinData()
                                .getAverageGuidanceValue(pfafs,
                                        getGuidanceInterpolation(ffgType),
                                        getGuidSourceExpiration(ffgType));

                        ratio = FFMPUtils.getRatioValue(qpe, guid);
                    }
                }
            } else {
                if (getQpeRecord() != null && getGuidanceRecord() != null) {
                    qpe = getQpeRecord().getBasinData().get(key).getAccumValue(
                            getTableTime(), recentTime,
                            getQpeSourceExpiration(), isRate());
                    guid = getGuidanceValue(
                            (FFMPGuidanceBasin) getGuidanceRecord()
                                    .getBasinData().get(key),
                            recentTime, ffgType);

                    ratio = FFMPUtils.getRatioValue(qpe, guid);
                }
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR, "Error caculating Ratio", e);
        }
        return ratio;
    }

    /**
     * close our dialog(s)
     */
    @Override
    public void closeDialog() {
        monitor.closeDialog(this);
    }

    /**
     * set the query runner
     */
    @Override
    public void setQuery(boolean isQuery) {
        this.isQuery = isQuery;
    }

    /**
     * clear them
     */
    @Override
    public void clear() {
        if (drawables != null) {
            for (Entry<DataTime, FFMPDrawable> entry : drawables.entrySet()) {
                entry.getValue().dispose();
            }

            drawables.clear();
        }
    }

    /**
     * Set them all for re-rendering
     */
    @Override
    public void dirty() {
        if (drawables != null) {
            for (Entry<DataTime, FFMPDrawable> entry : drawables.entrySet()) {
                entry.getValue().setDirty(true);
            }
        }

        if (queryJob != null) {
            queryJob.cancel();
        }
    }

    /**
     * clear the tables in the drawables
     */
    public void clearTables() {
        if (drawables != null) {
            for (Entry<DataTime, FFMPDrawable> entry : drawables.entrySet()) {
                entry.getValue().clearTables();
            }
        }
    }

    public void clearAllHuc() {
        if (drawables != null) {
            for (Entry<DataTime, FFMPDrawable> entry : drawables.entrySet()) {
                entry.getValue().removeTable(FFMPRecord.ALL);
            }
        }
    }

    @Override
    public void domainChanged() {
        for (Entry<DataTime, FFMPDrawable> entry : drawables.entrySet()) {
            entry.getValue().setValidDomains(getDomains());
        }
        setQuery(true);
        dirty();
        refresh();
    }

    /**
     * Allows the dialog to override CAVE
     *
     * @param paintTime
     */
    @Override
    public void setPaintTime(DataTime paintTime) {
        this.paintTime = paintTime;
    }

    /**
     * Draw the square and whatever else it needs
     *
     * @param pc
     * @param target
     * @param paintProps
     * @throws VizException
     */
    private void drawSquare(PixelCoverage pc, IGraphicsTarget target)
            throws VizException {
        DrawableLine line = new DrawableLine();
        line.lineStyle = getCapability(OutlineCapability.class).getLineStyle();
        line.width = getCapability(OutlineCapability.class).getOutlineWidth();
        line.basics.color = getCapability(ColorableCapability.class).getColor();
        line.addPoint(pc.getLl().x, pc.getLl().y);
        line.addPoint(pc.getUl().x, pc.getUl().y);
        line.addPoint(pc.getUr().x, pc.getUr().y);
        line.addPoint(pc.getLr().x, pc.getLr().y);
        line.addPoint(pc.getLl().x, pc.getLl().y);
        target.drawLine(line);
    }

    /**
     * gets the pixel coverage for this drawable
     *
     * @return
     */
    private PixelCoverage getPixelCoverage(Coordinate loc,
            PaintProperties myPaintProps) {

        double wscale = getScaleWidth(myPaintProps);
        double hscale = getScaleHeight(myPaintProps);
        double[] center = descriptor
                .worldToPixel(new double[] { loc.x, loc.y });

        Coordinate ul = new Coordinate(center[0] - wscale, center[1] - hscale);
        Coordinate ur = new Coordinate(center[0] + wscale, center[1] - hscale);
        Coordinate lr = new Coordinate(center[0] + wscale, center[1] + hscale);
        Coordinate ll = new Coordinate(center[0] - wscale, center[1] + hscale);

        return new PixelCoverage(ul, ur, lr, ll);
    }

    /**
     * Set the width scalar
     *
     * @param props
     * @return
     */
    private double getScaleWidth(PaintProperties myPaintProps) {
        double screenToWorldWidthRatio = myPaintProps.getCanvasBounds().width
                / myPaintProps.getView().getExtent().getWidth();

        return BOX_WIDTH / 3.0 / screenToWorldWidthRatio;
    }

    /**
     * Set the height scalar
     *
     * @param props
     * @return
     */
    private double getScaleHeight(PaintProperties myPaintProps) {
        double screenToWorldHeightRatio = myPaintProps.getCanvasBounds().height
                / myPaintProps.getView().getExtent().getHeight();

        return BOX_HEIGHT / 3.0 / screenToWorldHeightRatio;
    }

    /**
     * Draws the VGB's
     *
     * @param aTarget
     * @param paintProps
     * @throws VizException
     */
    private void paintVGBs(IGraphicsTarget aTarget, PaintProperties paintProps)
            throws VizException {

        for (PixelCoverage pc : vgbDrawables.values()) {
            drawSquare(pc, aTarget);
        }
    }

    private Coordinate getCenter(Object key) {
        FFMPTemplates templates = monitor.getTemplates(getSiteKey());
        Coordinate centerCoord = null;
        if (key instanceof Long) {
            Long pfaf = (Long) key;
            HucLevelGeometriesFactory hucGeomFactory = HucLevelGeometriesFactory
                    .getInstance();
            for (DomainXML domain : getDomains()) {
                try {
                    Map<Long, Geometry> map = hucGeomFactory.getGeometries(
                            templates, getSiteKey(), domain.getCwa(),
                            FFMPRecord.ALL);

                    if (map.containsKey(pfaf)) {
                        centerCoord = map.get(pfaf).getCentroid()
                                .getCoordinate();
                        break;
                    }
                } catch (Exception e) {
                    statusHandler.error("Error retrieving geometries", e);
                }
            }

        } else {
            centerCoord = monitor.getTemplates(getSiteKey())
                    .getVirtualGageBasinMetaData(getSiteKey(), (String) key)
                    .getCoordinate();

        }

        return centerCoord;
    }

    /**
     * recurse over up stream basins
     *
     * @param basinIds
     */
    private void getUpStreamBasins(List<Long> basinIds) {

        for (Long pfaf : basinIds) {
            streamPfafIds.add(pfaf);
            List<Long> newBasinIds = monitor.getTemplates(getSiteKey())
                    .getUpStreamBasins(getSiteKey(), pfaf);

            if (newBasinIds != null && !newBasinIds.isEmpty()) {
                getUpStreamBasins(newBasinIds);
            }
        }
    }

    /**
     * Down stream recursively
     *
     * @param pfafs
     */
    private void getDownStreamBasins(Long pfaf) {

        streamPfafIds.add(pfaf);

        Long newBasinId = monitor.getTemplates(getSiteKey())
                .getDownStreamBasins(getSiteKey(), pfaf);

        if (newBasinId != null) {
            getDownStreamBasins(newBasinId);
        }
    }

    @Override
    public void colorUtilsChange() {
        // clear and update the color maps
        colorUtil = null;
    }

    @Override
    public DataTime getPaintTime() {
        return paintTime;
    }

    /**
     * Add a value to worst case hash
     *
     * @param aggPfaf
     * @param value
     */
    private void addWorstCase(Long aggPfaf, Date recentTime, Float value) {
        FFMPDrawable drawable = drawables.get(new DataTime(recentTime));
        if (drawable != null && drawable.worstCaseHash != null) {
            drawable.worstCaseHash.put(aggPfaf, value);
        }
    }

    /**
     * Clear the worst case hash
     */
    @Override
    public void clearWorstCase() {
        for (Entry<DataTime, FFMPDrawable> entry : drawables.entrySet()) {
            entry.getValue().worstCaseHash.clear();
        }
    }

    /**
     * Get a value from the worst case hash
     *
     * @param aggPfaf
     * @return
     */
    private double getWorstCaseValue(Long aggPfaf, Date recentTime) {
        if (drawables.get(new DataTime(recentTime)) != null) {
            return drawables.get(new DataTime(recentTime)).worstCaseHash
                    .get(aggPfaf);
        } else {
            return 0.0;
        }
    }

    /**
     * check to see if aggregate exists in hash
     *
     * @param aggPfaf
     * @return
     */
    private boolean checkWorstCase(Long aggPfaf, Date recentTime) {
        if (drawables.get(new DataTime(recentTime)) != null) {
            return drawables.get(new DataTime(recentTime)).worstCaseHash
                    .containsKey(aggPfaf);
        } else {
            return false;
        }
    }

    private class FFMPDataRetrievalJob extends Job {
        private static final int QUEUE_LIMIT = 1;

        private final HucLevelGeometriesFactory hucGeomFactory = HucLevelGeometriesFactory
                .getInstance();

        private final ArrayBlockingQueue<Request> requestQueue = new ArrayBlockingQueue<>(
                QUEUE_LIMIT);

        public FFMPDataRetrievalJob() {
            super("Retrieving FFMP map information...");
        }

        private class Request {

            private IGraphicsTarget target;

            private FFMPDrawable drawable;

            private DataTime time;

            Request(IGraphicsTarget target, FFMPDrawable drawable,
                    DataTime time) {
                this.target = target;
                this.drawable = drawable;
                this.time = time;
            }
        }

        public void request(IGraphicsTarget target, FFMPDrawable drawable,
                DataTime time) {
            if (drawable != null) {
                if (requestQueue.size() == QUEUE_LIMIT) {
                    requestQueue.poll();
                }

                Request req = new Request(target, drawable, time);
                requestQueue.add(req);
                this.schedule();
            }
        }

        @SuppressWarnings({ "unchecked" })
        @Override
        protected IStatus run(IProgressMonitor progMonitor) {
            VizApp.runSync(new Runnable() {

                @Override
                public void run() {
                    Shell fshell = PlatformUI.getWorkbench()
                            .getActiveWorkbenchWindow().getShell();
                    fshell.setCursor(fshell.getDisplay()
                            .getSystemCursor(SWT.CURSOR_WAIT));
                }

            });
            String siteKey = getSiteKey();
            Request req = requestQueue.poll();
            while (req != null) {
                try {
                    // long t0 = System.currentTimeMillis();
                    FFMPDrawable drawable = req.drawable;
                    FFMPTemplates templates = monitor.getTemplates(siteKey);

                    String phuc = getHuc();
                    boolean isAllPhuc = phuc.equals(FFMPRecord.ALL);
                    FIELDS field = getField();

                    if (getResourceData().tableLoad) {
                        if (!getFFGName().equals(drawable.getGuidType())) {
                            drawable.disposeImage();
                        }
                    }

                    boolean globalRegen = !(phuc.equals(drawable.getHuc())
                            && field.equals(drawable.getField())
                            && req.time.equals(drawable.getTime())
                            && (centeredAggregationKey == null
                                    || !centeredAggregationKey.equals(
                                            drawable.getCenterAggrKey()))
                            && !drawable.isDirty()
                            && isParent() == drawable.isParent()
                            && isMaintainLayer() == drawable.isMaintainLayer()
                            && isWorstCase() == drawable.isWorstCase());

                    if (globalRegen) {
                        resetRecords();
                    }
                    for (DomainXML domain : getDomains()) {
                        String cwa = domain.getCwa();

                        // Added isDirty call to if check to force a repaint of
                        // the
                        // the basin when the color map changes.
                        if (globalRegen || drawable.genCwa(cwa)) {
                            Set<Long> cwaPfafs = templates
                                    .getMap(getSiteKey(), cwa, phuc).keySet();
                            Set<Long> pfafsToProcess = null;
                            Long centeredAggr = null;

                            if (centeredAggregationKey == null) {
                                // no center aggr selected
                                pfafsToProcess = cwaPfafs;
                            } else {
                                // center selected, determine center key
                                if (!isAllPhuc) {
                                    if (centeredAggregationKey instanceof String) {
                                        if (lowestCenter != ZOOM.BASIN) {

                                            centeredAggr = templates
                                                    .findAggregatedVGB(
                                                            (String) centeredAggregationKey,
                                                            siteKey, phuc);
                                        } else {
                                            centeredAggr = (Long) drawable
                                                    .getCenterAggrKey();
                                            // this is a fall back for VGB's
                                            if (centeredAggr == null) {
                                                centeredAggr = templates
                                                        .findAggregatedVGB(
                                                                (String) centeredAggregationKey,
                                                                siteKey, phuc);
                                            }
                                        }

                                    } else {
                                        if (lowestCenter != ZOOM.BASIN) {
                                            centeredAggr = (Long) centeredAggregationKey;
                                        } else {
                                            centeredAggr = (Long) drawable
                                                    .getCenterAggrKey();
                                            if (centeredAggr == null) {
                                                centeredAggr = templates
                                                        .getAggregatedPfaf(
                                                                (Long) centeredAggregationKey,
                                                                siteKey, phuc);
                                            }
                                        }
                                    }

                                    if (isParent() || !isParent()
                                            && !isMaintainLayer()) {

                                        if (cwaPfafs.contains(centeredAggr)) {
                                            pfafsToProcess = new HashSet<>();
                                            pfafsToProcess.add(centeredAggr);
                                        } else {
                                            drawable.disposeCwa(cwa);
                                        }

                                    } else {
                                        pfafsToProcess = cwaPfafs;
                                    }
                                } else {
                                    pfafsToProcess = cwaPfafs;
                                }
                            }

                            // long t1 = System.currentTimeMillis();

                            if (pfafsToProcess != null
                                    && !pfafsToProcess.isEmpty()) {

                                Map<Object, RGB> colorMap = new HashMap<>(
                                        pfafsToProcess.size(), 1);
                                String shadedHuc = null;

                                if (!isAllPhuc) {
                                    Map<Long, Geometry> geomMap = hucGeomFactory
                                            .getGeometries(templates, siteKey,
                                                    cwa, phuc);

                                    for (Long pfaf : pfafsToProcess) {

                                        if (!isMaintainLayer() && isParent()
                                                && pfaf.equals(centeredAggr)) {
                                            // add centered aggr to shape
                                            Collection<Long> allPfafs = null;

                                            if (isParent()) {
                                                allPfafs = templates
                                                        .getMap(siteKey, cwa,
                                                                FFMPRecord.ALL)
                                                        .keySet();
                                            } else {
                                                allPfafs = (List<Long>) templates
                                                        .getMap(siteKey, cwa,
                                                                phuc)
                                                        .get(centeredAggr);
                                            }

                                            if (allPfafs != null) {
                                                Map<Long, Geometry> allGeomMap = hucGeomFactory
                                                        .getGeometries(
                                                                templates,
                                                                siteKey, cwa,
                                                                FFMPRecord.ALL);
                                                IColormapShadedShape shape = shadedShapes
                                                        .getShape(cwa,
                                                                FFMPRecord.ALL,
                                                                req.target,
                                                                descriptor);
                                                shadedHuc = FFMPRecord.ALL;

                                                for (Long allPfaf : allPfafs) {

                                                    generateShapes(templates,
                                                            FFMPRecord.ALL,
                                                            allPfaf, allGeomMap,
                                                            req, shape,
                                                            colorMap);
                                                }
                                            }
                                        } else if (!isMaintainLayer()
                                                && !isParent()
                                                && !FFMPRecord.ALL.equals(phuc)
                                                && pfaf.equals(centeredAggr)) {

                                            Collection<Long> allPfafs = templates
                                                    .getMap(siteKey, cwa,
                                                            FFMPRecord.ALL)
                                                    .keySet();

                                            if (allPfafs != null) {
                                                Map<Long, Geometry> allGeomMap = hucGeomFactory
                                                        .getGeometries(
                                                                templates,
                                                                siteKey, cwa,
                                                                FFMPRecord.ALL);

                                                IColormapShadedShape shape = shadedShapes
                                                        .getShape(cwa,
                                                                FFMPRecord.ALL,
                                                                req.target,
                                                                descriptor);

                                                shadedHuc = FFMPRecord.ALL;

                                                for (Long allPfaf : allPfafs) {

                                                    generateShapes(templates,
                                                            FFMPRecord.ALL,
                                                            allPfaf, allGeomMap,
                                                            req, shape,
                                                            colorMap);
                                                }
                                            }
                                            // the case with aggregations in
                                            // mind
                                        } else {

                                            IColormapShadedShape shape = shadedShapes
                                                    .getShape(cwa, phuc,
                                                            req.target,
                                                            descriptor);

                                            shadedHuc = phuc;

                                            generateShapes(templates, phuc,
                                                    pfaf, geomMap, req, shape,
                                                    colorMap);
                                        }
                                    }
                                } else {
                                    Map<Long, Geometry> allGeomMap = hucGeomFactory
                                            .getGeometries(templates, siteKey,
                                                    cwa, FFMPRecord.ALL);
                                    IColormapShadedShape shape = shadedShapes
                                            .getShape(cwa, FFMPRecord.ALL,
                                                    req.target, descriptor);

                                    shadedHuc = FFMPRecord.ALL;
                                    for (Long allPfaf : pfafsToProcess) {
                                        generateShapes(templates,
                                                FFMPRecord.ALL, allPfaf,
                                                allGeomMap, req, shape,
                                                colorMap);
                                    }
                                }

                                drawable.setColorMap(cwa, colorMap);
                                drawable.setShadedHuc(shadedHuc);
                                req.target.setNeedsRefresh(true);
                            }
                        }
                    }

                    if (restoreTable) {
                        restoreTable = false;
                    }

                    drawable.setTime(req.time);
                    if (lowestCenter != ZOOM.BASIN) {
                        drawable.setCenterAggrKey(centeredAggregationKey);
                    }
                    drawable.setField(field);
                    drawable.setHuc(phuc);
                    drawable.setMaintainLayer(isMaintainLayer());
                    drawable.setParent(isParent());
                    drawable.setWorstCase(isWorstCase());

                    if (getResourceData().tableLoad) {
                        drawable.setGuidType(getFFGName());
                    }

                    drawable.setDirty(false);

                    if (isStreamFollow()) {
                        generateStreamShapes(templates, req);
                    } else {
                        streamPfafIds = null;
                        if (streamShadedShape != null) {
                            streamShadedShape.dispose();
                        }
                        if (streamOutlineShape != null) {
                            streamOutlineShape.dispose();
                        }
                    }

                    if (isSmallBasins) {
                        if (smallBasinOverlayShape == null) {
                            generateSmallBasins(templates, req);
                        }
                    }

                } catch (Exception e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error processing map query request", e);
                }

                req = requestQueue.poll();

            }
            VizApp.runSync(new Runnable() {

                @Override
                public void run() {
                    Shell fshell = PlatformUI.getWorkbench()
                            .getActiveWorkbenchWindow().getShell();
                    fshell.setCursor(null);

                    // check whether or not the dialog needs to be dumped
                    if (monitor != null) {
                        monitor.splashDispose(getResource());

                        if (getResourceData().tableLoad && isFirst) {
                            isFirst = false;
                            updateDialog();
                        }
                    }
                }

            });

            return Status.OK_STATUS;
        }

        private void generateShapes(FFMPTemplates templates, String huc,
                Long pfaf, Map<Long, Geometry> geomMap, Request req,
                IColormapShadedShape shape, Map<Object, RGB> colorMap) {

            // my logic
            Geometry g = geomMap.get(pfaf);

            if (g == null) {
                return;
            }

            if (!(g instanceof Point)) {
                RGB color = null;

                // find the color
                try {

                    color = getColor(pfaf, req.time.getRefTime(),
                            !FFMPRecord.ALL.equals(huc));

                    if (color != null) {
                        if (!shape.getColorKeys().contains(pfaf)) {
                            shape.addGeometry(g, pfaf);
                        }

                        colorMap.put(pfaf, color);
                    }

                } catch (Exception e) {
                    e.printStackTrace();
                    statusHandler.handle(Priority.PROBLEM,
                            "Error reprojecting map outline", e);
                }
            }
        }

        private void generateStreamShapes(FFMPTemplates templates,
                Request req) {
            // run stream queries
            if (streamPfafIds != null) {
                streamPfafIds.clear();
            } else {
                streamPfafIds = new HashSet<>();
            }

            IWireframeShape localWireframeShape = null;
            IShadedShape localShadedShape = null;

            // make new one's
            if (nextStreamPfaf != null) {
                if (FFMPRecord.CLICK_TYPE.UP.equals(stream)
                        || FFMPRecord.CLICK_TYPE.UP_DOWN.equals(stream)) {

                    List<Long> upstreamBasins = monitor
                            .getTemplates(getSiteKey())
                            .getUpStreamBasins(getSiteKey(), nextStreamPfaf);
                    getUpStreamBasins(upstreamBasins);
                }
                if (FFMPRecord.CLICK_TYPE.DOWN.equals(stream)
                        || FFMPRecord.CLICK_TYPE.UP_DOWN.equals(stream)) {
                    Long downStreamBasin = monitor.getTemplates(getSiteKey())
                            .getDownStreamBasins(getSiteKey(), nextStreamPfaf);
                    getDownStreamBasins(downStreamBasin);

                }

                // lookup geometires for the pfafs and compile
                // shapes
                streamPfafIds.add(nextStreamPfaf);

                // create the frames/shaded shapes here
                localWireframeShape = req.target.createWireframeShape(false,
                        descriptor);
                localShadedShape = req.target.createShadedShape(false,
                        descriptor.getGridGeometry());

                JTSCompiler jtsCompiler2 = new JTSCompiler(localShadedShape,
                        localWireframeShape, descriptor);

                if (basinTraceColor == null) {
                    basinTraceColor = getCapability(ColorableCapability.class)
                            .getColor();
                }
                JTSGeometryData jtsData2 = jtsCompiler2.createGeometryData();
                jtsData2.setPointStyle(PointStyle.CROSS);
                jtsData2.setGeometryColor(basinTraceColor);

                // read in geometries
                try {
                    for (DomainXML domains : templates.getDomains()) {
                        String cwa = domains.getCwa();
                        Map<Long, Geometry> geomMap = hucGeomFactory
                                .getGeometries(templates, getSiteKey(), cwa,
                                        FFMPRecord.ALL);

                        for (Long pfaf : streamPfafIds) {
                            // TODO: streamPfafIds should be ordered by
                            // domain already...
                            Geometry g = null;
                            try {
                                g = geomMap.get(pfaf);
                                if (g != null) {
                                    jtsCompiler2.handle(g, jtsData2);

                                }
                            } catch (Exception e) {
                                // just a missing geometry, no biggie
                                statusHandler.info(
                                        "Error compiling outline for " + g, e);
                            }
                        }
                    }
                } catch (Exception e) {
                    statusHandler.error("Error retrieving geometries", e);
                }

                if (localWireframeShape != null) {
                    localWireframeShape.compile();
                    if (streamOutlineShape != null) {
                        streamOutlineShape.dispose();
                    }
                }

                if (localShadedShape != null) {
                    localShadedShape.compile();
                    localShadedShape.setFillPattern(STREAM_FILL);
                    if (streamShadedShape != null) {
                        streamShadedShape.dispose();
                    }
                }

                streamOutlineShape = localWireframeShape;
                streamShadedShape = localShadedShape;
                perfLog.log("Basin Trace Shapes Complete");
            }
        }

        private void generateSmallBasins(FFMPTemplates templates, Request req) {

            IWireframeShape basinShape = null;
            // create the frames/shaded shapes here
            try {
                // read in geometries
                basinShape = req.target.createWireframeShape(false, descriptor);
                JTSCompiler jtsCompiler3 = new JTSCompiler(null, basinShape,
                        descriptor);
                RGB color = getCapability(ColorableCapability.class).getColor();
                JTSGeometryData jtsData3 = jtsCompiler3.createGeometryData();
                jtsData3.setPointStyle(PointStyle.CROSS);
                jtsData3.setGeometryColor(color);

                for (DomainXML domains : templates.getDomains()) {
                    String cwa = domains.getCwa();
                    Map<Long, Geometry> geomMap = hucGeomFactory.getGeometries(
                            templates, getSiteKey(), cwa, FFMPRecord.ALL);

                    if (geomMap != null) {
                        for (Geometry g : geomMap.values()) {
                            if (g != null) {
                                jtsCompiler3.handle(g, jtsData3);
                            }
                        }
                    }
                }
            } catch (Exception e) {
                statusHandler.error("Error generating small basins outlines",
                        e);
            }

            if (basinShape != null) {
                basinShape.compile();
            }

            if (smallBasinOverlayShape != null) {
                smallBasinOverlayShape.dispose();
            }

            smallBasinOverlayShape = basinShape;
        }

        @Override
        protected void canceling() {
            super.canceling();
        }
    }

    @Override
    public void cwaChanged(FFMPCWAChangeEvent fcce) {

        @SuppressWarnings("unchecked")
        List<String> domainNames = (List<String>) fcce.getSource();
        getDomains().clear();
        clearTables();

        for (String domainName : domainNames) {
            getDomains().add(monitor.getRunConfig().getDomain(domainName));
        }

        if (isAutoRefresh) {
            domainChanged();
        }

        updateDialog();
    }

    @Override
    public void hucChanged(FFMPHUCChangeEvent fhce) {
        ITimer timer = TimeUtil.getTimer();
        timer.start();
        setHuc((String) fhce.getSource());
        centeredAggregationKey = null;
        centeredAggregatePfafList = null;

        if (isAutoRefresh) {
            if (basinTableDlg != null) {
                // Gets rid of the aggregate name if it is zoomed into one
                basinTableDlg.blankGroupLabel();
            }
            clearTables();
            hucChanged();
            refresh();
        }

        updateDialog();
        timer.stop();
        perfLog.logDuration("HUC Update complete", timer.getElapsedTime());
    }

    @Override
    public void timeChanged(FFMPTimeChangeEvent fhce,
            FFMPRecord.FIELDS fieldArg) throws VizException {

        FFMPTime ffmpTime = (FFMPTime) fhce.getSource();

        if (ffmpTime.getTime() != time || isSplit != ffmpTime.isSplit()) {

            isSplit = ffmpTime.isSplit();
            setTime(ffmpTime.getTime());
            setTableTime();
            if (interpolationMap != null) {
                interpolationMap.clear();
            }

            if (getTime() == 0.00) {
                getResourceData().field = FFMPRecord.FIELDS.RATE;
            } else if (getTime() > 0.00) {
                if (getField().equals(FFMPRecord.FIELDS.RATE)) {
                    getResourceData().field = fieldArg;
                }
            }

            if (isAutoRefresh) {
                clearWorstCase();
                colorUtilsChange();
                dirty();
                clearTables();
                refresh();
            }

            updateDialog();
        }
    }

    @Override
    public void fieldChanged(FFMPFieldChangeEvent ffce) throws VizException {
        FIELDS origField = getResourceData().field;
        getResourceData().field = (FIELDS) ffce.getSource();

        if (isAutoRefresh) {
            clearWorstCase();
            colorUtilsChange();
            setQuery(true);
            dirty();
            refresh();
        }

        if (getResourceData().field.equals(FFMPRecord.FIELDS.RATE)) {
            try {
                setTime(0.00);
                setTableTime();
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error changing the resource time", e);
            }

            updateDialog();

        } else if (getResourceData().field.equals(FFMPRecord.FIELDS.QPE)
                && origField.equals(FFMPRecord.FIELDS.RATE)) {

            updateDialog();
        }
    }

    @Override
    public void isWorstCaseChanged(FFMPWorstCaseEvent fwce) {
        isWorstCase = (Boolean) fwce.getSource();

        if (isAutoRefresh) {
            clearWorstCase();
            setQuery(true);
            refresh();
            dirty();
            clearTables();
        }

        updateDialog();

    }

    @Override
    public void isAutoRefresh(FFMPAutoRefreshEvent fare) {
        isAutoRefresh = (Boolean) fare.getSource();

        if (isAutoRefresh) {
            setQuery(false);
        } else {
            setQuery(true);
            dirty();
        }

        refresh();

        updateDialog();
    }

    @Override
    public void isMaintainLayer(FFMPMaintainLayerEvent fmle) {
        isMaintainLayer = (Boolean) fmle.getSource();

        if (isAutoRefresh) {
            setQuery(true);
            refresh();
            dirty();
        }

    }

    @Override
    public void isParent(FFMPParentBasinEvent fpbe) {
        isParent = (Boolean) fpbe.getSource();

        if (isAutoRefresh) {
            setQuery(true);
            refresh();
            dirty();
        }

        updateDialog();

    }

    @Override
    public void restoreTable() {
        centeredAggregationKey = null;
        centeredAggregatePfafList = null;
        restoreTable = true;

        lowestCenter = FFMPRecord.ZOOM.WFO;
        getRenderableDisplays().forEach(d -> d.getExtent().reset());
        zoom(1.0f);

        if (isAutoRefresh) {
            setQuery(true);
            dirty();
            refresh();
        }

        updateDialog();

    }

    @Override
    public void setLinkToFrame(boolean isLinkToFrame) {
        this.isLinkToFrame = isLinkToFrame;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public FFMPGraphData getGraphData(String pfafString) throws VizException {
        ITimer timer = TimeUtil.getTimer();
        timer.start();
        FfmpTableConfig tableConfig = FfmpTableConfig.getInstance();
        String ffgGraphType = tableConfig.getTableConfigData(getSiteKey())
                .getFfgGraphType();
        Long basinPfaf = null;
        Long dataId = null;
        FFMPVirtualGageBasinMetaData fvgbmd = null;
        FFMPBasin basin = null;

        try {
            basinPfaf = Long.parseLong(pfafString);
            dataId = basinPfaf;
        } catch (NumberFormatException nfe) {
            // can't parse a string for VGB
            fvgbmd = monitor.getTemplates(getSiteKey())
                    .getVirtualGageBasinMetaData(getSiteKey(), pfafString);
            basinPfaf = fvgbmd.getParentPfaf();
            dataId = fvgbmd.getLookupId();
        }

        FFMPBasinMetaData mBasin = monitor.getTemplates(getSiteKey())
                .getBasin(getSiteKey(), basinPfaf);
        FFMPGraphData fgd = null;
        // VGB
        if (fvgbmd != null) {
            fgd = new FFMPGraphData(pfafString, fvgbmd.getCounty(),
                    mBasin.getState(), fvgbmd.getName(), getTableTime(), basin);
        } else {
            fgd = new FFMPGraphData(pfafString, mBasin.getCounty(),
                    mBasin.getState(), mBasin.getStreamName(), getTableTime(),
                    basin);
        }

        FFMPBasin rateBasin = null;
        FFMPBasin qpeBasin = null;
        FFMPBasin qpfBasin = null;
        FFMPGuidanceBasin guidBasin = null;
        FFMPVirtualGageBasin virtualBasin = null;
        boolean qpe = false;
        boolean guid = false;
        Date oldestRefTime = getOldestTime();
        Date mostRecentRefTime = getPaintTime().getRefTime();
        Date minUriTime = getTimeOrderedKeys().get(0);

        // grabs the basins we need
        try {
            rateBasin = monitor.getGraphRateBasin(getProduct(), getSiteKey(),
                    getDataKey(),
                    fvgbmd == null ? getProduct().getRate()
                            : getProduct().getVirtual(),
                    oldestRefTime, FFMPRecord.ALL, dataId);

            List<Double> rateTimes = new ArrayList<>();

            if (rateBasin != null) {
                for (Date date : rateBasin.getValues().keySet()) {
                    if (date.before(minUriTime)
                            || date.after(mostRecentRefTime)) {
                        continue;
                    }

                    double dtime = FFMPGuiUtils.getTimeDiff(mostRecentRefTime,
                            date);
                    fgd.setRate(dtime, (double) rateBasin.getValue(date));
                    rateTimes.add(dtime);
                }
            }

            fgd.setRateTimes(rateTimes);

        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "FFMPMonitor: getGraphData(): missing RATE dataset.", e);
        }
        try {
            qpeBasin = monitor.getGraphQPEBasin(getProduct(), getSiteKey(),
                    getDataKey(),
                    fvgbmd == null ? getProduct().getQpe()
                            : getProduct().getVirtual(),
                    oldestRefTime, FFMPRecord.ALL, dataId);

            List<Double> qpeTimes = new ArrayList<>();

            if (qpeBasin != null) {
                for (Date date : qpeBasin.getValues().keySet()) {
                    if (date.before(minUriTime)
                            || date.after(mostRecentRefTime)) {
                        continue;
                    }

                    double dtime = FFMPGuiUtils.getTimeDiff(mostRecentRefTime,
                            date);
                    double currVal = qpeBasin.getAccumValue(date,
                            mostRecentRefTime, getQpeSourceExpiration(),
                            isRate());
                    fgd.setQpe(dtime, currVal);
                    qpeTimes.add(dtime);

                }
            }

            fgd.setQpeTimes(qpeTimes);
            qpe = true;

        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "FFMPMonitor: getGraphData(): missing QPE dataset.", e);
        }

        try {

            qpfBasin = monitor.getGraphQPFBasin(getProduct(), getSiteKey(),
                    getDataKey(), oldestRefTime, FFMPRecord.ALL, basinPfaf);

            if (qpfBasin != null) {
                Float qpfFloat = qpfBasin.getAverageValue(
                        monitor.getQpfWindow().getAfterTime(),
                        monitor.getQpfWindow().getBeforeTime());

                fgd.setQpfValue(qpfFloat);

                List<Double> qpfTimes = new ArrayList<>();
                for (Date date : qpfBasin.getValues().keySet()) {

                    double dtime = FFMPGuiUtils.getTimeDiff(mostRecentRefTime,
                            date);

                    fgd.setQpf(dtime, (double) qpfBasin.getValue(date));
                    qpfTimes.add(dtime);
                }

                fgd.setQpfTimes(qpfTimes);
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "FFMPMonitor: getGraphData(): missing QPF dataset.", e);
        }

        FFMPGuidanceInterpolation guidanceInterpolator = new FFMPGuidanceInterpolation(
                monitor.getSourceConfig(), getProduct(),
                monitor.getRunConfig().getRunner(getResourceData().wfo)
                        .getProduct(getSiteKey()),
                getPrimarySource(), ffgGraphType, getSiteKey());

        try {
            guidBasin = (FFMPGuidanceBasin) monitor.getGraphGuidanceBasin(
                    getProduct(), ffgGraphType, getSiteKey(), getDataKey(),
                    oldestRefTime, FFMPRecord.ALL, basinPfaf);
            List<Double> guidTimes = new ArrayList<>();
            for (SourceXML ffgSource : getProduct()
                    .getGuidanceSourcesByDisplayName(ffgGraphType)) {
                if (guidBasin.getValue(ffgSource.getSourceName(),
                        guidanceInterpolator,
                        getGuidSourceExpiration(ffgGraphType)) != null) {

                    double time = FFMPGuiUtils.getTimeDiff(mostRecentRefTime,
                            FFMPGuiUtils.getHourDisplacement(mostRecentRefTime,
                                    ffgSource.getDurationHour()));
                    fgd.setGuid(time,
                            (double) guidBasin.getValue(
                                    ffgSource.getSourceName(),
                                    guidanceInterpolator,
                                    getGuidSourceExpiration(ffgGraphType)));
                    guidTimes.add(time);
                }
            }

            fgd.setGuidanceTimes(guidTimes);
            guid = true;
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "FFMPMonitor: getGraphData(): missing GUIDANCE dataset: "
                            + ffgGraphType,
                    e);
        }

        if (fvgbmd != null) {
            try {
                // VGB's use a different timing sequenceFFMPResource
                String lid = fvgbmd.getLid();

                virtualBasin = monitor.getVirtualGageBasinData(dataId, lid,
                        mostRecentRefTime);

                if (virtualBasin != null) {
                    List<Double> virtualTimes = new ArrayList<>();
                    // Date refTime = null;
                    for (Date date : virtualBasin.getValues().keySet()) {
                        double dtime = FFMPGuiUtils
                                .getTimeDiff(mostRecentRefTime, date);
                        Double dval = new Double(virtualBasin.getValue(date));
                        fgd.setVirtual(dtime, dval);
                        virtualTimes.add(dtime);
                    }
                    fgd.setVirtualTimes(virtualTimes);
                }
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "FFMPMonitor: getGraphData(): missing VIRTUAL dataset.",
                        e);
            }
        }

        if (qpe) {
            for (Double fgdQpeTime : fgd.getQpeTimes()) {

                try {

                    Float guidancev = 0.0f;
                    Float qpev = fgd.getQpe(fgdQpeTime).floatValue();

                    // Initialize to NaN. If there is no guidance then we want
                    // NaN as the default value as it will be "missing".
                    double diff = Double.NaN;
                    double ratio = Double.NaN;

                    if (guid) {

                        guidanceInterpolator
                                .setInterpolationSources(fgdQpeTime);

                        if (guidanceInterpolator.isInterpolate()) {
                            guidancev = guidBasin.getInterpolatedValue(
                                    guidanceInterpolator,
                                    getGuidSourceExpiration(ffgGraphType));
                        } else {
                            if (guidanceInterpolator.getSource1() != null) {
                                guidancev = guidBasin.getValue(
                                        guidanceInterpolator.getSource1(),
                                        guidanceInterpolator,
                                        getGuidSourceExpiration(ffgGraphType));
                            }

                        }

                        Double qpf = fgd.getQpf(fgdQpeTime);
                        if (qpf != null) {
                            diff = FFMPUtils.getDiffValue(qpev,
                                    guidancev);
                            ratio = FFMPUtils.getRatioValue(qpev,
                                    guidancev);
                        }
                    }

                    fgd.setRatio(fgdQpeTime, ratio);
                    fgd.setDiff(fgdQpeTime, diff);

                } catch (Exception e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "FFMP Can't retrieve graphing data", e);
                }
            }
        }
        timer.stop();
        perfLog.logDuration("Graph Data processing", timer.getElapsedTime());

        return fgd;
    }

    /**
     * @return showStream
     */
    public boolean isShowStream() {
        return showStream;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setBasinToggle(boolean val) {
        showBasin = val;
        refresh();
    }

    public boolean isBasinToggle() {
        return showBasin;
    }

    /**
     * @return the showFfmpData
     */
    public boolean isFfmpDataToggle() {
        return showFfmpData;
    }

    /**
     * @param showFfmpData
     *            the showFfmpData to set
     */
    public void setFfmpDataToggle(boolean showFfmpData) {
        this.showFfmpData = showFfmpData;
        refresh();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setStreamToggle(boolean val) {
        showStream = val;
        refresh();
    }

    public boolean isStreamToggle() {
        return showStream;
    }

    /**
     * Sets the time
     *
     * @param time
     */
    private void setTime(double time) {
        this.time = time;
    }

    /** gets the double time **/
    public Double getTime() {
        return time;
    }

    /**
     * gets the table accumulation time
     *
     * @return
     */
    public Date getTableTime() {
        return tableTime;
    }

    public void setTableTime(Date tableTime) {
        this.tableTime = tableTime;
    }

    /**
     * Sets the time used for accumulation drawn from the slider time
     */
    private void setTableTime() {
        if (tableTime == null) {
            tableTime = new Date();
        }

        synchronized (this) {
            Date recentTime = getMostRecentTime();
            long time = new Double(
                    recentTime.getTime() - TimeUtil.MILLIS_PER_HOUR * getTime())
                            .longValue();
            Date date = new Date();
            date.setTime(time);
            this.tableTime = date;
        }
    }

    /**
     * sets the huc
     *
     * @param huc
     */
    public void setHuc(String huc) {
        getResourceData().huc = huc;
    }

    /**
     * Gets the huc
     *
     * @return huc
     */
    public String getHuc() {
        return getResourceData().huc;
    }

    /**
     * get primary source site key
     *
     * @return site key
     */
    public String getSiteKey() {
        return getResourceData().siteKey;
    }

    /**
     * get primary source data key
     *
     * @return data key
     */
    public String getDataKey() {
        return getResourceData().dataKey;
    }

    /**
     * Get the primary source
     *
     * @return primary source
     */
    public String getPrimarySource() {
        return getResourceData().sourceName;
    }

    /**
     * gets the reverse of the later
     *
     * @param sourceName
     * @return enum field
     */
    public FIELDS getField(String sourceName) {
        // TODO Throws error here if product is defined, but not the source in
        // FFMPSourceConfig.xml
        String sfield = monitor.getSourceConfig().getSourceType(sourceName)
                .name().toLowerCase();
        FIELDS myField = null;
        if (sfield.equals(FFMPRecord.FIELDS.QPE.getFieldName())) {
            myField = FFMPRecord.FIELDS.QPE;
        } else if (sfield.equals(FFMPRecord.FIELDS.RATE.getFieldName())) {
            myField = FFMPRecord.FIELDS.RATE;
        } else if (sfield.equals(FFMPRecord.FIELDS.QPF.getFieldName())) {
            myField = FFMPRecord.FIELDS.QPF;
        } else if (sfield.equals(FFMPRecord.FIELDS.GUIDANCE.getFieldName())) {
            myField = FFMPRecord.FIELDS.GUIDANCE;
        } else if ("gage".equals(sfield)) {
            myField = FFMPRecord.FIELDS.VIRTUAL;
        }
        return myField;
    }

    /***
     * Gets the enum field
     *
     * @return enum field
     */
    public FIELDS getField() {
        return getResourceData().getField();
    }

    /**
     * gets the list of Domains
     *
     * @return list of Domains
     */
    public List<DomainXML> getDomains() {
        return getResourceData().getDomains();
    }

    /**
     * Sets the domains
     *
     * @param domains
     */
    public void setDomains(List<DomainXML> domains) {
        getResourceData().setDomains(domains);
    }

    /**
     * gets the product if available
     *
     * @return product
     */
    public ProductXML getProduct() {
        return getResourceData().getProduct();
    }

    /**
     * Get the oldest time available
     *
     * @return oldest time
     */
    public Date getOldestTime() {
        synchronized (timeOrderedKeys) {
            List<Date> times = getTimeOrderedKeys();
            if (times != null) {
                return times.get(0);
            }
        }

        return null;
    }

    /**
     * Gets the most recent time
     *
     * @return recent time
     */
    public Date getMostRecentTime() {
        if (mostRecentTime == null) {
            if (getTimeOrderedKeys() != null) {
                synchronized (timeOrderedKeys) {
                    List<Date> times = getTimeOrderedKeys();
                    if (times != null) {
                        mostRecentTime = times.get(times.size() - 1);
                        return mostRecentTime;
                    }
                }
            }
        }

        return mostRecentTime;
    }

    public void setMostRecentTime(Date mostRecentTime) {
        this.mostRecentTime = mostRecentTime;
    }

    /**
     * Order the dates
     *
     * @param set
     * @return ordered dates
     */
    public synchronized List<Date> getTimeOrderedKeys() {
        if (timeOrderedKeys == null || !toKeysInitialized) {
            toKeysInitialized = true;

            // stand alone displays use this
            timeOrderedKeys = new ArrayList<>();

            try {
                DataTime[] availableTimes = getResourceData()
                        .getAvailableTimes();
                if (availableTimes.length > 0) {
                    Date oldestCurrentTime = availableTimes[0].getRefTime();
                    Date oldestTime = new Date(oldestCurrentTime.getTime()
                            - TimeUtil.MILLIS_PER_HOUR * 24);

                    SortedSet<Date> keys = monitor
                            .getAvailableUris(getSiteKey(), getDataKey(),
                                    getPrimarySource(), oldestTime)
                            .keySet();
                    timeOrderedKeys.addAll(keys);
                } else {
                    statusHandler
                            .error("No available times for the FFMPResource");
                }
            } catch (VizException e) {
                statusHandler.error(
                        "Error retrieving available times for the FFMPResource",
                        e);
            }
        }
        return timeOrderedKeys;
    }

    /**
     * Add a time
     *
     * @param date
     */
    private void updateTimeOrderedkeys(Date date) {
        if (timeOrderedKeys != null) {

            synchronized (timeOrderedKeys) {
                if (!getTimeOrderedKeys().contains(date)) {
                    getTimeOrderedKeys().add(date);
                    // new most recent time
                    mostRecentTime = null;
                }
            }
        }
    }

    /**
     * Sets the sliderTime
     *
     * @return slider time
     */
    public double getTimeSliderOffset() {
        double sliderTime = 0.0d;
        if (getTableTime() != null) {
            Long offset = getMostRecentTime().getTime()
                    - getTableTime().getTime();

            sliderTime = Math.floor(
                    4 * (offset.doubleValue() / TimeUtil.MILLIS_PER_HOUR) + .25)
                    / 4;
            // sliderTime = Math.floor(((offset.doubleValue() / (1000 * 3600)) +
            // .005) * 100) / 100;
            setTime(sliderTime);
        }
        return sliderTime;
    }

    /**
     * update the data in the dialog
     */
    @Override
    public void updateDialog() {
        if (basinTableDlg != null) {
            monitor.fireMonitorEvent(basinTableDlg.getClass().getName());
        }
    }

    /**
     * Gets the guidance interpolation object
     *
     * @return the guidance interpolation object
     */
    private FFMPGuidanceInterpolation getGuidanceInterpolation(
            String guidType) {
        if (getResourceData().tableLoad) {
            if (interpolationMap == null || interpolationMap.size() == 0) {
                getGuidanceInterpolators();
            }

            return interpolationMap.get(guidType);
        }

        return null;
    }

    public Map<String, FFMPGuidanceInterpolation> getGuidanceInterpolators() {
        if (interpolationMap == null || interpolationMap.size() == 0) {
            interpolationMap = new HashMap<>();

            if (getProduct() != null) {
                List<String> guidSourceFamilies = monitor.getRunConfig()
                        .getRunner(getResourceData().wfo)
                        .getProduct(getSiteKey())
                        .getGuidanceSourceFamilies(getProduct());

                for (String guidSrcFamily : guidSourceFamilies) {
                    FFMPGuidanceInterpolation interpolation = new FFMPGuidanceInterpolation(
                            monitor.getSourceConfig(), getProduct(),
                            monitor.getRunConfig()
                                    .getRunner(getResourceData().wfo)
                                    .getProduct(getSiteKey()),
                            getPrimarySource(), guidSrcFamily, getSiteKey());
                    interpolation.setInterpolationSources(getTime());
                    interpolationMap.put(guidSrcFamily, interpolation);
                }
            }
        }

        return interpolationMap;
    }

    /**
     * Gets the value for the Guidance basin
     *
     * @param basin
     * @return he value for the Guidance basin
     */
    public float getGuidanceValue(FFMPGuidanceBasin basin, Date recentTime,
            String guidType) {
        float dvalue = Float.NaN;
        if (basin != null) {
            long fips = monitor.getTemplates(getSiteKey())
                    .getCountyFipsByPfaf(basin.getPfaf());
            basin.setCountyFips(fips);

            FFMPGuidanceInterpolation interp = getGuidanceInterpolation(
                    guidType);
            long sourceExpiration = getGuidSourceExpiration(guidType);
            if (getResourceData().tableLoad) {
                // interpolating
                if (interp.isInterpolate()) {
                    // Interpolating between sources
                    dvalue = basin.getInterpolatedValue(interp,
                            sourceExpiration);
                } else {
                    dvalue = basin.getValue(interp.getStandardSource(), interp,
                            sourceExpiration);
                }
                if (dvalue == FFMPUtils.MISSING) {
                    return Float.NaN;
                }
            } else {
                dvalue = basin.getValue(getPrimarySource(), recentTime, interp,
                        sourceExpiration);
            }
        }

        return dvalue;
    }

    public boolean isWorstCase() {
        return isWorstCase;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isAutoRefresh() {
        return isAutoRefresh;
    }

    public boolean isParent() {
        return isParent;
    }

    public boolean isMaintainLayer() {
        return isMaintainLayer;
    }

    public boolean isLinkToFrame() {
        return isLinkToFrame;
    }

    public boolean isSplit() {
        return isSplit;
    }

    /**
     * gets the list of the current centered agg
     *
     * @return list of the current centered agg
     */
    public List<Long> getCenteredAggregatePfafs() {
        if (centeredAggregatePfafList == null) {
            Long center = null;
            if (centeredAggregationKey instanceof String) {
                center = monitor.getTemplates(getSiteKey()).findAggregatedVGB(
                        (String) centeredAggregationKey, getSiteKey(),
                        getHuc());

            } else {
                center = (Long) centeredAggregationKey;
            }

            centeredAggregatePfafList = monitor.getTemplates(getSiteKey())
                    .getAggregatePfafs(center, getSiteKey(), getHuc());
        }

        return centeredAggregatePfafList;
    }

    /**
     * Set the centered aggreagte pfaf list
     *
     * @param centeredAggregatePfafList
     */
    public void setCenteredAggregatePfafs(
            List<Long> centeredAggregatePfafList) {
        this.centeredAggregatePfafList = centeredAggregatePfafList;
    }

    /**
     * Gets the GAP calculation for the FFMP dialog to display
     *
     * @return Array of Gap data
     */
    public List<FFMPGap> getGaps() {
        synchronized (timeOrderedKeys) {
            return FFMPGap.getGaps(getTimeOrderedKeys(),
                    getResourceData().getPrimarySourceXML()
                            .getExpirationMinutes(getSiteKey()),
                    getTableTime(), getMostRecentTime());
        }
    }

    /**
     *
     * @return instance of this class
     */
    public FFMPResource getResource() {
        return this;
    }

    /**
     * wrapper for the purge
     *
     * @param pdate
     */
    private void purge(Date pdate) {

        long time = pdate.getTime();
        time = time - getPurgePeriod();
        Date ndate = new Date(time);

        List<Date> removes = new ArrayList<>();
        for (Date date : getTimeOrderedKeys()) {
            if (date.before(ndate)) {
                removes.add(date);
            }
        }

        for (Date date : removes) {
            statusHandler.handle(Priority.INFO,
                    "FFMP CACHE removing time: " + date);
            getTimeOrderedKeys().remove(date);
        }

        if (monitor != null) {
            monitor.purgeFFMPData(getResourceData().getProduct(),
                    getResourceData().getPrimarySource(), getSiteKey(), ndate);
        }

        try {
            DataTime[] availableTimes = getResourceData().getAvailableTimes();
            if (availableTimes.length > 0) {
                DataTime lastDate = availableTimes[availableTimes.length - 1];
                for (DataTime dt : drawables.keySet()) {
                    if (!dt.greaterThan(lastDate)) {
                        drawables.remove(dt);
                    }
                }
            }
        } catch (VizException e) {
            statusHandler.error("Error purging old data", e);
        }

    }

    /**
     * source expiration value as a long
     *
     * @return
     */
    public long getQpeSourceExpiration() {
        if (qpeSourceExpiration == 0l) {

            this.qpeSourceExpiration = monitor.getSourceConfig()
                    .getSource(resourceData.getPrimarySource())
                    .getExpirationMinutes(getSiteKey())
                    * TimeUtil.MILLIS_PER_MINUTE;
        }
        return qpeSourceExpiration;
    }

    /**
     * source expiration value as a long
     *
     * @return
     */
    public long getQpfSourceExpiration() {
        if (qpfSourceExpiration == 0l) {
            SourceXML source = null;
            if (getProduct() != null) {
                FfmpTableConfigData ffmpTableCfgData = FfmpTableConfig
                        .getInstance().getTableConfigData(getSiteKey());
                String qpfDisplayName = ffmpTableCfgData.getQpfDisplayName();
                source = getProduct().getQpfSourcesByDisplayName(qpfDisplayName)
                        .get(0);
            } else {
                source = FFMPSourceConfigurationManager.getInstance()
                        .getSource(getResourceData().sourceName);
            }
            qpfSourceExpiration = source.getExpirationMinutes(getSiteKey())
                    * TimeUtil.MILLIS_PER_MINUTE;
        }
        return qpfSourceExpiration;
    }

    /**
     * Gets the guidance source expiration
     *
     * @return
     */
    public long getGuidSourceExpiration(String guidDispName) {
        if (guidSourceExpiration == 0l) {
            if (getProduct() != null) {
                SourceXML source = getProduct()
                        .getGuidanceSourcesBySourceFamily(guidDispName).get(0);
                guidSourceExpiration = source.getExpirationMinutes(getSiteKey())
                        * TimeUtil.MILLIS_PER_MINUTE;

            } else {
                guidSourceExpiration = monitor.getSourceConfig()
                        .getSource(resourceData.getPrimarySource())
                        .getExpirationMinutes(getSiteKey())
                        * TimeUtil.MILLIS_PER_MINUTE;
            }
        }

        return guidSourceExpiration;
    }

    /**
     * source expiration value in mins
     *
     * @return
     */
    public Boolean isRate() {
        if (isRate == null) {

            this.isRate = monitor.getSourceConfig()
                    .getSource(resourceData.getPrimarySource()).isRate();
        }
        return isRate;
    }

    /**
     * Is this a product load situation
     *
     * @return
     */
    public boolean isProductLoad() {
        if (getProduct() != null) {
            return true;
        }
        return false;
    }

    /**
     * Get the FFG used
     *
     * @return
     */
    public String getFFGName() {
        String ffgName = null;

        if (getResourceData().tableLoad) {
            String guidSrc = FFMPConfig.getInstance().getFFMPConfigData()
                    .getGuidSrc();
            if (guidSrc.startsWith("xxx")) {
                ffgName = "";
            } else {
                /*
                 * guidSrc will be the displayName but we need the source family
                 * name
                 */
                SourceXML source = getProduct()
                        .getGuidanceSourcesByDisplayName(guidSrc).get(0);
                ffgName = source.getSourceFamily();
            }
        } else {
            ffgName = getPrimarySource();
        }

        return ffgName;
    }

    /**
     * Sets the Basin Trace Color
     *
     * @param basinTraceColor
     */
    public void setBasinTraceColor(RGB basinTraceColor) {
        this.basinTraceColor = basinTraceColor;
        isQuery = true;
    }

    /**
     * Get the Basin Trace Color
     *
     * @return
     */
    public RGB getBasinTraceColor() {
        return this.basinTraceColor;
    }

    /**
     * Sets the Basin Boundary Color
     *
     * @param basinBoundryColor
     */
    public void setBasinBoundaryColor(RGB basinBoundaryColor) {
        this.basinBoundaryColor = basinBoundaryColor;
    }

    /**
     * Get the Basin Boundary Color
     *
     * @return
     */
    public RGB getBasinBoundaryColor() {
        return this.basinBoundaryColor;
    }

    /**
     * get the current drawable
     *
     * @param date
     * @return
     */
    public FFMPDrawable getDrawable(DataTime time) {
        FFMPDrawable drawable = null;
        if (drawables != null && time != null) {
            drawable = drawables.get(time);
        }
        return drawable;
    }

    @Override
    public void manualRefresh() {
        colorUtilsChange();
        refresh();
    }

    /**
     * Get the purge file time
     */
    public long getPurgePeriod() {

        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext lc = pm.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.SITE);
        LocalizationFile lfFile = pm.getLocalizationFile(lc,
                "purge/ffmpPurgeRules.xml");

        if (lfFile.exists()) {

            // TODO Need to figure out why we can't read in the purgeRules!
            /*
             * try { PurgeRuleSet prs = (PurgeRuleSet) SerializationUtil
             * .jaxbUnmarshalFromXmlFile(lfFile.getFile().getAbsolutePath());
             *
             * for (PurgeRule rule: prs.getRules()) { if
             * (rule.getId().equals("ffmp")) { return rule.getPeriodInMillis();
             * } }
             *
             * } catch (SerializationException e) { e.printStackTrace(); return
             * 3600*24*1000; }
             */

        }

        return 24 * TimeUtil.MILLIS_PER_HOUR;
    }

    @Override
    public DataTime[] getDataTimes() {
        List<Date> dates = getTimeOrderedKeys();
        DataTime[] dataTimes = new DataTime[dates.size()];
        for (int i = 0; i < dataTimes.length; i += 1) {
            dataTimes[i] = new DataTime(dates.get(i));
        }
        return dataTimes;
    }

    /**
     * This method creates the upper left legend text for HPE derived QPE
     * sources. It is only used for HPE QPE sources.
     *
     * @param date
     * @return
     */
    private String getHpeText(Date date) {
        List<String> products = hpeCacheLookup.get(date);
        if (products == null) {
            products = new ArrayList<>(0);
        }
        HpeLabelKey key = new HpeLabelKey();
        key.setDate(date);
        for (String product : products) {
            key.setProductName(product);
        }
        String text = hpeLegendMap.get(key);
        if (text == null) {
            String wfo = null;
            String siteKey = null;
            String dataKey = null;
            String sourceName = null;

            if (qpeRecord != null) {
                wfo = qpeRecord.getWfo();
                siteKey = qpeRecord.getSiteKey();
                dataKey = qpeRecord.getDataKey();
                sourceName = qpeRecord.getSourceName();
            } else if (qpfRecord != null) {
                wfo = qpfRecord.getWfo();
                siteKey = qpfRecord.getSiteKey();
                dataKey = qpfRecord.getDataKey();
                sourceName = qpfRecord.getSourceName();
            } else {
                return "";
            }
            String productId = monitor.getProductID(paintTime.getRefTime(), wfo,
                    siteKey, dataKey, sourceName);
            dataJob.scheduleRetrieval(date, productId);
            statusHandler.info("Loading product " + productId);
        }

        if (text == null) {
            text = "";
        }

        return text;
    }

    /**
     * HPE source lookup job. TODO: resolve duplication with
     * com.raytheon.uf.viz.hpe.rsc.HpeLabelResource.HpeSourceDataJob.
     *
     * <pre>
     *
     * SOFTWARE HISTORY
     *
     * Date         Ticket#    Engineer    Description
     * ------------ ---------- ----------- --------------------------
     * Nov 11, 2014  3026       dhladky     Initial creation
     *
     * </pre>
     *
     * @author dhladky
     */
    private class HpeSourceDataJob extends Job {
        private volatile String productId;

        private volatile Date date;

        public HpeSourceDataJob() {
            super("Get HPE Source");
        }

        protected void scheduleRetrieval(Date date, String productId) {
            this.productId = productId;
            this.date = date;
            if (this.getState() == Job.RUNNING
                    || this.getState() == Job.SLEEPING
                    || this.getState() == Job.WAITING) {
                return;
            }
            this.schedule();
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            // Request the text from edex
            try {
                HpeLabelDataRequest req = new HpeLabelDataRequest(productId,
                        date);
                HpeLabelDataResponse response = (HpeLabelDataResponse) ThriftClient
                        .sendRequest(req);
                Map<Date, String> data = response.getData();
                for (Entry<Date, String> entry : data.entrySet()) {
                    Date d = entry.getKey();
                    HpeLabelKey key = new HpeLabelKey(productId, d);
                    hpeLegendMap.put(key, entry.getValue());
                    if (!hpeCacheLookup.containsKey(d)) {
                        hpeCacheLookup.put(d, new ArrayList<String>());
                    }
                    hpeCacheLookup.get(d).add(productId);
                }
            } catch (VizException e) {
                statusHandler.error(e.getLocalizedMessage(), e);
            }

            return Status.OK_STATUS;
        }
    }

    @Override
    public Set<InterrogationKey<?>> getInterrogationKeys() {
        Set<InterrogationKey<?>> set = new HashSet<>();
        set.add(Interrogator.GEOMETRY);
        set.add(Interrogator.VALUE);
        set.add(new ClassInterrogationKey<>(Double.class));
        return set;
    }

    @Override
    public InterrogateMap interrogate(ReferencedCoordinate coordinate,
            DataTime time, InterrogationKey<?>... keys) {
        InterrogateMap map = new InterrogateMap();
        try {
            FFMPBasinMetaData metaBasin = monitor.getTemplates(getSiteKey())
                    .findBasinByLatLon(getSiteKey(), coordinate.asLatLon());
            if (metaBasin != null) {
                Float value = getBasinValue(metaBasin.getPfaf(),
                        getPaintTime().getRefTime(), true);
                if (value != null) {
                    ColorMapParameters parameters = getCapability(
                            ColorMapCapability.class).getColorMapParameters();
                    map.put(Interrogator.VALUE, Quantities.getQuantity(value,
                            parameters.getDisplayUnit()));
                }
                Geometry geom = monitor.getTemplates(getSiteKey())
                        .getRawGeometries(getSiteKey(), metaBasin.getCwa())
                        .get(metaBasin.getPfaf());
                if (geom != null) {
                    map.put(Interrogator.GEOMETRY, geom);
                }
            }
        } catch (TransformException | FactoryException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to transform coordinate to lat/lon value", e);
        }
        return map;
    }

    @Override
    public String[] getExtraText(DataTime time) {
        StringBuilder sb = new StringBuilder();
        if (getResourceData().tableLoad && isFfmpDataToggle()
                && (isAutoRefresh || isQuery)) {
            sb.append("FFMP ").append(df.format(getTime())).append(" hour ")
                    .append(FFMPRecord.getFieldLongDescription(getField()));
        }
        return new String[] { sb.toString() };
    }

    @Override
    public ZOOM unzoom(FFMPScreenCenterEvent fsce) {
        Object pfaf = fsce.getSource();

        FFMPTemplates templates = monitor.getTemplates(getSiteKey());
        String huc = getHuc();
        Coordinate center = templates.findAggregationCenter((Long) pfaf,
                getSiteKey(), huc);

        getRenderableDisplays().forEach(d -> d.getExtent().reset());
        int mapWidth = getDescriptor().getMapWidth() / 1000;
        float zoomLevel = (float) AGGREGATE_ZOOM / mapWidth;

        // reset the screen as if it where a pan
        if (center != null) {
            getRenderableDisplays().forEach(
                    d -> d.recenter(new double[] { center.x, center.y }));
        }

        zoom(zoomLevel);

        lowestCenter = FFMPRecord.ZOOM.AGGREGATE;
        this.refresh();

        return lowestCenter;
    }
}
