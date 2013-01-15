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
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ConcurrentHashMap;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPBasin;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPBasinMetaData;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPCacheRecord;
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
import com.raytheon.uf.common.geospatial.MapUtil;
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
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.util.TimeUtil;
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
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IColormapShadedShapeExtension;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IColormapShadedShapeExtension.IColormapShadedShape;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.map.MapDescriptor;
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
import com.raytheon.uf.viz.monitor.ffmp.ui.listeners.FFMPLoadListener;
import com.raytheon.uf.viz.monitor.ffmp.ui.listeners.FFMPLoaderEvent;
import com.raytheon.uf.viz.monitor.ffmp.ui.listeners.FFMPMaintainLayerEvent;
import com.raytheon.uf.viz.monitor.ffmp.ui.listeners.FFMPParentBasinEvent;
import com.raytheon.uf.viz.monitor.ffmp.ui.listeners.FFMPScreenCenterEvent;
import com.raytheon.uf.viz.monitor.ffmp.ui.listeners.FFMPStreamTraceEvent;
import com.raytheon.uf.viz.monitor.ffmp.ui.listeners.FFMPTimeChangeEvent;
import com.raytheon.uf.viz.monitor.ffmp.ui.listeners.FFMPWorstCaseEvent;
import com.raytheon.uf.viz.monitor.ffmp.ui.listeners.IFFMPResourceListener;
import com.raytheon.uf.viz.monitor.ffmp.ui.rsc.FFMPDataLoader.LOADER_TYPE;
import com.raytheon.viz.core.rsc.jts.JTSCompiler;
import com.raytheon.viz.core.rsc.jts.JTSCompiler.PointStyle;
import com.raytheon.viz.ui.input.EditableManager;
import com.raytheon.viz.ui.input.InputAdapter;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Point;

/**
 * Resource to display FFMP data
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 29 June, 2009 2521          dhladky     Initial creation
 * 11 Apr.  2012 DR 14522      gzhang      Fixing invalid thread error.
 * 31 July  2012 14517         mpduff      Fix for blanking map on update.
 * 14 Sep 2012   1048         njensen      Code cleanup
 * 07 Dec 2012   1353         rferrel      Changes for non-blocking FFMPSplash dialog.
 * 10 Jan 2103   1475         dhladky      Some cleanup
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class FFMPResource extends
        AbstractVizResource<FFMPResourceData, MapDescriptor> implements
        IResourceDataChanged, IFFMPResourceListener, FFMPListener,
        FFMPLoadListener {

    // TODO move ALL constant to common plugin
    private static final String ALL = "ALL";

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(FFMPResource.class);

    /**
     * The zoom level for an aggregate view.
     */
    private static final int AGGREGATE_ZOOM = 137;

    /**
     * The zoom level for a basin view.
     */
    private static final int BASIN_ZOOM = 55;

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

    /** the stream cross hatched area **/
    private IWireframeShape streamOutlineShape = null;

    /** small basins overlay **/
    private IWireframeShape smallBasinOverlayShape = null;

    /** the stream cross hatched area **/
    private IShadedShape streamShadedShape = null;

    /** always the same vertexes, one for each CWA **/
    private FFMPShapeContainer shadedShapes = new FFMPShapeContainer();

    /** Basin shaded shape **/
    protected ConcurrentHashMap<DataTime, FFMPDrawable> drawables = new ConcurrentHashMap<DataTime, FFMPDrawable>();

    /** VGB drawables **/
    protected HashMap<String, PixelCoverage> vgbDrawables = new HashMap<String, PixelCoverage>();

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

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.InputAdapter#handleMouseDown(int, int,
         * int)
         */
        @Override
        public boolean handleMouseUp(int x, int y, int mouseButton) {
            if (mouseButton == 3) {
                if (isStreamFollow()) {
                    traceClick(getResourceContainer().translateClick(x, y));
                    return true;
                }
            }
            return false;
        }

    };

    private FFMPCacheRecord rateRecord = null;

    private boolean isNewRate = true;

    private FFMPCacheRecord qpeRecord = null;

    private boolean isNewQpe = true;

    private FFMPCacheRecord guidRecord = null;

    private boolean isNewGuid = true;

    private FFMPCacheRecord qpfRecord = null;

    private boolean isNewQpf = true;

    private FFMPCacheRecord virtualRecord = null;

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
    public ArrayList<Long> centeredAggregatePfafList = null;

    /** table slider time **/
    private Date tableTime = null;

    // complete reset
    public boolean isQuery = true;

    /** the last extent **/
    private PixelExtent lastExtent;

    /**
     * FFMP basin table dialog.
     */
    public FfmpBasinTableDlg basinTableDlg;

    /** data loader **/
    private FFMPDataLoader loader = null;

    /** Guidance Interpolation Map **/
    public HashMap<String, FFMPGuidanceInterpolation> interpolationMap;

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

    /**
     * Drawable String for the field description.
     */
    private DrawableString fieldDescString = null;

    private DrawableString basinLocatorString = null;

    private RGB basinTraceColor = null;

    private RGB basinBoundaryColor = null;

    /** ordered list of times **/
    private ArrayList<Date> timeOrderedKeys = new ArrayList<Date>();

    private boolean toKeysInitialized = false;

    /** force utility **/
    private FFFGForceUtil forceUtil = null;

    /** Restore Table flag */
    private boolean restoreTable = false;

    /**
     * FFMP resource
     * 
     * @param getResourceData
     *            ()
     * @param loadProperties
     */
    protected FFMPResource(FFMPResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        getResourceData().addChangeListener(this);
        monitor = getResourceData().getMonitor();
        monitor.addResourceListener(this);

        if (getResourceData().tableLoad) {
            if (!isBasinToggle()) {
                setBasinToggle(true);
            }
            monitor.launchFFMPDialog(this);
        }
        // So we are not time agnostic
        dataTimes = new ArrayList<DataTime>();
    }

    @Override
    public void resourceChanged(ChangeType type, Object object) {

        if (type.equals(ChangeType.DATA_UPDATE)) {
            FFFGDataMgr.getUpdatedInstance();
            PluginDataObject[] pdos = (PluginDataObject[]) object;
            FFMPRecord ffmpRec = (FFMPRecord) pdos[pdos.length - 1];
            // an update clears everything
            clear();
            // only care about the most recent one
            try {

                if (ffmpRec.getSourceName()
                        .equals(getResourceData().sourceName)) {
                    // go back an extra time step
                    Date previousMostRecentTime = null;
                    List<Date> tok = getTimeOrderedKeys();
                    if (tok.size() >= 2) {
                        previousMostRecentTime = tok.get(tok.size() - 2);
                    } else {
                        previousMostRecentTime = tok.get(0);
                    }

                    updateTimeOrderedkeys(ffmpRec.getDataTime().getRefTime());

                    if (getResourceData().tableLoad) {
                        setTableTime();
                    }

                    setRecord(ffmpRec);

                    statusHandler.handle(Priority.INFO, "Updating : Previous: "
                            + previousMostRecentTime + " New: "
                            + ffmpRec.getDataTime().getRefTime());

                    if (getResourceData().tableLoad) {

                        if (loader == null) {
                            startLoader(previousMostRecentTime, ffmpRec
                                    .getDataTime().getRefTime(),
                                    LOADER_TYPE.GENERAL);
                        } else {
                            while (!loader.isDone) {
                                try {
                                    Thread.sleep(1000);
                                } catch (InterruptedException e) {
                                    e.printStackTrace();
                                }
                            }
                            startLoader(previousMostRecentTime, ffmpRec
                                    .getDataTime().getRefTime(),
                                    LOADER_TYPE.GENERAL);
                        }

                        while (!loader.isDone) {
                            try {
                                Thread.sleep(1000);
                            } catch (InterruptedException e) {
                                e.printStackTrace();
                            }
                        }

                        purge(ffmpRec.getDataTime().getRefTime());
                    }

                    resetRecords();
                }

            } catch (VizException ve) {
                statusHandler.handle(Priority.PROBLEM, "Error updating record",
                        ve);
            }
        }

        if (getResourceData().tableLoad) {
            allowNewTableUpdate();
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

    @Override
    public void hucChanged() {

        center = null;
        lowestCenter = FFMPRecord.ZOOM.WFO;

        if (isAutoRefresh()) {
            setQuery(true);
            dirty();
            issueRefresh();
        }
    }

    @Override
    public void centerChanged(FFMPScreenCenterEvent fsce) throws VizException {

        FFMPZoom zoom = (FFMPZoom) fsce.getSource();
        setCenter(zoom);

        if (isAutoRefresh()) {
            if (!isMaintainLayer() || isParent()) {
                setQuery(true);
            }

            refresh();
        }
    }

    @Override
    public void traceChanged(FFMPStreamTraceEvent fste) {
        if ((FFMPRecord.CLICK_TYPE) fste.getSource() == FFMPRecord.CLICK_TYPE.CLEAR) {
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

        if (isAutoRefresh()) {
            refresh();
        }
    }

    /**
     * Gets the FFMPBasin by key
     * 
     * @param key
     * @return
     * @throws VizException
     */
    private FFMPBasin getBasin(Long key, FFMPRecord.FIELDS bfield,
            Date recentTime, boolean aggregate) throws VizException {
        FFMPBasin basin = null;

        String huc = null;
        if (aggregate) {
            huc = getHuc();
        } else {
            huc = ALL;
        }
        basin = getRecord(bfield, recentTime).getBasinData(huc).getBasins()
                .get(key);

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
            if (getHuc().equals(ALL)) {
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
                ArrayList<Long> pfafs = monitor.getTemplates(getSiteKey())
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
                    value = getRateRecord(recentTime).getBasinData(ALL)
                            .getMaxValue(pfafs, recentTime);
                    break;
                }
                case QPF: {
                    value = getQpfRecord(recentTime).getBasinData(ALL)
                            .getAverageMaxValue(pfafs, recentTime,
                                    getQpfSourceExpiration());
                    break;
                }
                case GUIDANCE: {
                    long fips = monitor.getTemplates(getSiteKey())
                            .getCountyFipsByPfaf(pfafs.get(0));

                    value = getGuidanceRecord().getBasinData(ALL)
                            .getMaxGuidanceValue(pfafs,
                                    getGuidanceInterpolation(getFFGName()),
                                    getGuidSourceExpiration(), fips);
                    break;
                }
                case QPE: {
                    value = getQpeRecord().getBasinData(ALL).getAccumMaxValue(
                            pfafs, recentTime, getTableTime(),
                            getQpeSourceExpiration(),
                            getResourceData().getPrimarySourceXML().isRate());
                    break;
                }
                }
                // add the value to the worst case hash
                addWorstCase(key, recentTime, value);

            } else {
                ArrayList<Long> pfafs = null;

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
                        value = getBasin(key, field, recentTime, aggregate)
                                .getValue(recentTime);
                        break;
                    case QPF: {
                        value = getBasin(key, field, recentTime, aggregate)
                                .getAverageValue(recentTime,
                                        getQpfSourceExpiration());
                        break;
                    }
                    case GUIDANCE: {

                        value = getGuidanceValue(
                                (FFMPGuidanceBasin) getBasin(key, field,
                                        recentTime, aggregate), recentTime,
                                getFFGName());

                        break;
                    }
                    case QPE: {
                        value = getBasin(key, field, recentTime, aggregate)
                                .getAccumValue(
                                        getTableTime(),
                                        recentTime,
                                        getQpeSourceExpiration(),
                                        getResourceData().getPrimarySourceXML()
                                                .isRate());
                        break;
                    }
                    }
                } else {
                    switch (field) {
                    case QPF: {
                        value = getBasin(key, field, recentTime, aggregate)
                                .getAverageValue(recentTime,
                                        getQpfSourceExpiration());
                        break;
                    }
                    case GUIDANCE: {
                        value = getGuidanceValue(
                                (FFMPGuidanceBasin) getBasin(key, field,
                                        recentTime, aggregate), recentTime,
                                getFFGName());
                        break;
                    }
                    }
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return value;
    }

    private float forceValue(ArrayList<Long> pfafs, FFMPBasin basin,
            float unforcedValue) {
        float value = unforcedValue;

        if (forceUtil == null) {
            forceUtil = new FFFGForceUtil(this, getFFGName());
        }

        forceUtil.setSliderTime(this.getTime());

        if (pfafs != null) {
            forceUtil.calculateForcings(pfafs,
                    monitor.getTemplates(getSiteKey()), basin);

            ArrayList<Long> forcedPfafs = forceUtil.getForcedPfafList();
            ArrayList<Long> pfafList = forceUtil.getPfafList();
            boolean forced = forceUtil.isForced();
            if ((forcedPfafs.size() > 0) && forced) {
                // Recalculate the guidance using the forced value(s)
                value = guidRecord.getBasinData(ALL).getAverageGuidanceValue(
                        pfafList, this.getGuidanceInterpolation(getFFGName()),
                        new Float(value), forcedPfafs,
                        getGuidSourceExpiration());
            } else if (forcedPfafs.size() > 0) {
                value = guidRecord.getBasinData(ALL).getAverageGuidanceValue(
                        pfafList, this.getGuidanceInterpolation(getFFGName()),
                        Float.NaN, forcedPfafs, getGuidSourceExpiration());
            }
        }

        return value;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#getName()
     */
    @Override
    public String getName() {
        StringBuilder prefix = new StringBuilder();
        try {
            FIELDS sfield = getField();
            if ((sfield == FFMPRecord.FIELDS.RATIO)
                    || (sfield == FFMPRecord.FIELDS.DIFF)) {
                sfield = FFMPRecord.FIELDS.QPE;
            }

            PluginDataObject pdo = null;

            try {
                pdo = getRecord(sfield, paintTime.getRefTime());
            } catch (NullPointerException npe) {
                return "No Data Available";
            }

            if (pdo == null) {
                return "No Data Available";
            }

            FFMPRecord record = (FFMPRecord) pdo;
            prefix = new StringBuilder();
            prefix.append(record.getPluginName());
            prefix.append(" ");
            prefix.append(record.getSiteKey());
            prefix.append(" ");
            if (getResourceData().tableLoad) {
                prefix.append("Table Display ");
            } else {
                prefix.append("Image ");
                SourceXML source = monitor.getSourceConfig().getSource(
                        getResourceData().sourceName);
                if (source.getSourceType().equals(
                        FFMPSourceConfigurationManager.SOURCE_TYPE.GUIDANCE
                                .getSourceType())) {
                    prefix.append(source.getDisplayName() + " "
                            + source.getDurationHour() + " HR");
                } else {
                    prefix.append(source.getDisplayName());
                }
            }
        } catch (Exception e) {
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
            huc = ALL;
        } else {
            huc = getHuc();
        }
        return huc;
    }

    /**
     * Gets the record currently used
     * 
     * @return FFMPCacheRecord
     */
    public FFMPCacheRecord getRateRecord(Date recentTime) {

        if ((rateRecord == null) && isNewRate) {
            try {
                String huc = getHucIfWorstCase();
                rateRecord = monitor.getRateRecord(getProduct(), getSiteKey(),
                        getDataKey(), getPrimarySource(), recentTime, huc,
                        false);
                isNewRate = false;
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        return rateRecord;
    }

    /**
     * Gets the record currently used
     * 
     * @return FFMPCacheRecord
     */
    public FFMPCacheRecord getQpeRecord() {
        try {
            if ((qpeRecord == null) && (getTableTime() != null) && isNewQpe) {
                String huc = getHucIfWorstCase();
                qpeRecord = monitor.getQPERecord(getProduct(), getSiteKey(),
                        getDataKey(), getPrimarySource(), getTableTime(), huc,
                        false);
                isNewQpe = false;
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        // System.out.println("FFMPResource.getQPERecord(): " + getTableTime());

        return qpeRecord;
    }

    /**
     * Gets the record currently used
     * 
     * @return FFMPCacheRecord
     */
    public FFMPCacheRecord getGuidanceRecord() {
        try {
            if ((guidRecord == null) || isNewGuid) {
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

                String huc = getHucIfWorstCase();
                guidRecord = monitor.getGuidanceRecord(getProduct(),
                        getSiteKey(), sourceName, date, huc, isStandAlone);
                isNewGuid = false;
            }

        } catch (Exception e) {
            e.printStackTrace();
        }

        return guidRecord;
    }

    /**
     * Gets the record currently used
     * 
     * @return FFMPReFFMPCacheRecordcord
     */
    public FFMPCacheRecord getQpfRecord(Date recentTime) {
        try {
            if ((qpfRecord == null) && isNewQpf) {
                Date date = null;
                // Stand alone displays
                if (getResourceData().tableLoad) {
                    date = recentTime;
                } else {
                    if (paintTime != null) {
                        date = paintTime.getRefTime();
                    }
                }

                String huc = getHucIfWorstCase();
                qpfRecord = monitor.getQPFRecord(getProduct(), getSiteKey(),
                        getDataKey(), getPrimarySource(), date, huc, false);
                isNewQpf = false;
            }
        } catch (Exception e) {
            e.printStackTrace();
        }

        return qpfRecord;
    }

    /**
     * Gets the record currently used
     * 
     * @return FFMPCacheRecord
     */
    public FFMPCacheRecord getVirtualRecord() {
        try {
            if ((virtualRecord == null) && isNewVirtual) {
                virtualRecord = monitor.getVirtualRecord(getProduct(),
                        getSiteKey(), getDataKey(), getPrimarySource(),
                        getTableTime(), ALL, false);
                isNewVirtual = false;
            }

        } catch (Exception e) {
            e.printStackTrace();
        }

        return virtualRecord;
    }

    /**
     * General get record call
     * 
     * @param pfield
     * @return FFMPCacheRecord
     */
    public FFMPCacheRecord getRecord(FIELDS pfield, Date recentTime) {
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

    /**
     * Set the Record straight.
     * 
     * @throws VizException
     * 
     */
    protected void setRecord(FFMPRecord ffmpRecord) throws VizException {
        getResourceData().populateRecord(getProduct(), ffmpRecord, getHuc());
    }

    @Override
    protected void disposeInternal() {
        IDisplayPaneContainer container = getResourceContainer();
        if (container != null) {
            container.unregisterMouseHandler(inspectAdapter);
        }

        if (font != null) {
            font.dispose();
        }

        if (xfont != null) {
            xfont.dispose();
        }

        if (this.getName().indexOf("Table Display") > -1) {

            if (resourceData.floader != null) {
                resourceData.floader.removeListener(this);
                resourceData.floader.kill();
                resourceData.floader = null;
            }

            if (basinTableDlg != null) {
                closeDialog();
                if (smallBasinOverlayShape != null) {
                    smallBasinOverlayShape.dispose();
                }
            }

            HucLevelGeometriesFactory.getInstance().clear();

            if (monitor != null) {
                monitor.forceKillFFMPSplash();
            }
        }

        clear();

        if (monitor.getResourceListenerList().size() == 1) {
            monitor.nullifyMonitor();
        } else {
            monitor.removeResourceListener(this);
        }

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

            if ((getTimeOrderedKeys() != null)
                    && (getTimeOrderedKeys().size() > 0)) {
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
        org.eclipse.swt.widgets.Display.getDefault().asyncExec(new Runnable() {

            public void run() {

                if (/* this. */font == null) {
                    /* this. */font = target.initializeFont("Dialog", 11, null);
                }

                font.setMagnification(getCapability(
                        MagnificationCapability.class).getMagnification()
                        .floatValue());

                if (/* this. */xfont == null) {
                    IFont.Style[] styles = new IFont.Style[] { IFont.Style.BOLD };
                    /* this. */xfont = target.initializeFont("Monospace", 12,
                            styles);
                }

                xfont.setMagnification(getCapability(
                        MagnificationCapability.class).getMagnification()
                        .floatValue());

                fieldDescString = new DrawableString("FFMP "
                        + df.format(getTime()) + " hour "
                        + FFMPRecord.getFieldLongDescription(getField()),
                        getCapability(ColorableCapability.class).getColor());
                fieldDescString.font = font;
                fieldDescString.horizontalAlignment = HorizontalAlignment.CENTER;
                fieldDescString.verticallAlignment = VerticalAlignment.MIDDLE;

                basinLocatorString = new DrawableString("X", new RGB(255, 255,
                        255));
                basinLocatorString.font = xfont;
                basinLocatorString.horizontalAlignment = HorizontalAlignment.CENTER;
                basinLocatorString.verticallAlignment = VerticalAlignment.MIDDLE;
                basinLocatorString.textStyle = TextStyle.BLANKED;
            }
        });
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

        // clear the screen
        aTarget.clearClippingPlane();

        if ((getTimeOrderedKeys() != null) && (getTimeOrderedKeys().size() > 0)
                && (getDomains() != null)) {

            paintTime = paintProps.getDataTime();
            paintProps.setAlpha(getCapability(ImagingCapability.class)
                    .getAlpha());

            boolean isShaded = isPolygonal();
            FFMPDrawable drawable = null;

            if (paintTime != null) {
                if (loader != null && !loader.isDone
                        && loader.loadType == LOADER_TYPE.GENERAL) {
                    return;
                }
                if (!drawables.containsKey(paintTime)) {

                    drawable = new FFMPDrawable(getDomains());
                    drawables.put(paintTime, drawable);
                } else {
                    // we found it!
                    drawable = drawables.get(paintTime);
                    // System.out.println("Found the drawable");

                    if (!paintTime.equals(drawable.getTime())) {
                        drawable.setDirty(true);
                    }

                    if ((lastExtent == null)
                            || !lastExtent.getEnvelope().contains(
                                    drawable.getExt().getEnvelope())) {
                        drawable.setDirty(true);
                    }

                    // auto refresh state
                    if (isQuery) {
                        drawable.setDirty(true);
                    }
                }

                if (getResourceData().tableLoad
                        && !paintTime.getRefTime().equals(getMostRecentTime())) {
                    setMostRecentTime(paintTime.getRefTime());
                    setTableTime();
                    // if (isLinkToFrame && loader != null && loader.loadType !=
                    // LOADER_TYPE.GENERAL) {
                    if (isLinkToFrame) {
                        updateDialog();
                    }
                }
            } else {
                if (getResourceData().getMonitor().ffmpSplash != null) {
                    getResourceData().getMonitor().ffmpSplash.close();
                    getResourceData().getMonitor().ffmpSplash = null;
                }
            }

            if ((drawable != null) && drawable.isDirty()) {
                // only need to do the query if extent changed, pfafs may be
                // fine
                PixelExtent expandedExtent = getExpandedExtent((PixelExtent) paintProps
                        .getView().getExtent());
                drawable.setExt(expandedExtent);
                queryJob.request(aTarget, descriptor, isShaded, drawable,
                        expandedExtent, paintTime);
                lastExtent = expandedExtent;
            }

            boolean isAllHuc = getHuc().equals(ALL);
            for (DomainXML domain : getDomains()) {
                String cwa = domain.getCwa();
                if (isShaded) {
                    if (drawable != null) {
                        IColormapShadedShape shape = shadedShapes
                                .getDrawableShape(cwa, drawable.getShadedHuc());
                        Map<Object, RGB> colorMap = drawable.getColorMap(cwa);

                        if (this.isFfmpDataToggle() && (shape != null)
                                && (colorMap != null)) {

                            aTarget.getExtension(
                                    IColormapShadedShapeExtension.class)
                                    .drawColormapShadedShape(
                                            shape,
                                            colorMap,
                                            paintProps.getAlpha(),
                                            getCapability(
                                                    ImagingCapability.class)
                                                    .getBrightness());
                        }
                    }
                }

                if (getResourceData().tableLoad) {

                    int mapWidth = getDescriptor().getMapWidth() / 1000;
                    double zoom = getDescriptor().getRenderableDisplay()
                            .getZoom();

                    // determine whether or not to draw the small guys
                    if ((mapWidth * zoom) > 250.0) {
                        if (isSmallBasins) {
                            isSmallBasins = false;
                            refresh();
                        }

                    } else if ((mapWidth * zoom) < 250.0) {
                        if (!isSmallBasins) {
                            isSmallBasins = true;
                            if (smallBasinOverlayShape == null) {
                                drawable.setDirty(true);
                            } else {
                                refresh();
                            }
                        }
                    }

                    if ((lowestCenter == ZOOM.AGGREGATE)
                            || (lowestCenter == ZOOM.BASIN) || isAllHuc
                            || this.isBasinToggle() || isSmallBasins) {

                        if (isSmallBasins && this.isBasinToggle()) {
                            if ((smallBasinOverlayShape != null)
                                    && smallBasinOverlayShape.isDrawable()) {

                                if (basinBoundaryColor == null) {
                                    basinBoundaryColor = getCapability(
                                            ColorableCapability.class)
                                            .getColor();
                                }

                                aTarget.drawWireframeShape(
                                        smallBasinOverlayShape,
                                        basinBoundaryColor,
                                        getCapability(OutlineCapability.class)
                                                .getOutlineWidth(),
                                        getCapability(OutlineCapability.class)
                                                .getLineStyle());
                            } else if ((smallBasinOverlayShape == null)
                                    && getCapability(OutlineCapability.class)
                                            .isOutlineOn()) {
                                aTarget.setNeedsRefresh(true);
                            }
                        }
                    }

                    // the product string
                    if (isFfmpDataToggle() && fieldDescString != null) {
                        paintProductString(aTarget, paintProps);
                    }
                }
            }
            // re-centered ?
            if (centeredAggregationKey != null) {
                vgbDrawables.clear();
                // create pixelCoverages for the VGB's
                if (isAllHuc) {
                    for (DomainXML domain : getDomains()) {
                        for (Long pfaf : monitor
                                .getTemplates(getSiteKey())
                                .getMap(getSiteKey(), domain.getCwa(), getHuc())
                                .keySet()) {
                            ArrayList<FFMPVirtualGageBasinMetaData> fvgmdList = monitor
                                    .getTemplates(getSiteKey())
                                    .getVirtualGageBasinMetaData(getSiteKey(),
                                            domain.getCwa(), pfaf);
                            if (fvgmdList != null) {
                                for (FFMPVirtualGageBasinMetaData fvgmd : fvgmdList) {
                                    vgbDrawables.put(
                                            fvgmd.getLid(),
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
                            ArrayList<FFMPVirtualGageBasinMetaData> fvgmdList = monitor
                                    .getTemplates(getSiteKey())
                                    .getVirtualGageBasinMetaData(getSiteKey(),
                                            null, pfaf);
                            if (fvgmdList != null) {
                                for (FFMPVirtualGageBasinMetaData fvgmd : fvgmdList) {
                                    vgbDrawables.put(
                                            fvgmd.getLid(),
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
                                            domain.getCwa()).entrySet()) {
                                if (entry.getValue() != null) {
                                    vgbDrawables.put(
                                            entry.getKey(),
                                            getPixelCoverage(entry.getValue()
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
            if (!isFfmpDataToggle()) { // clear if ffmpDataToggle is false
                getCapability(ColorMapCapability.class).setColorMapParameters(
                        null);
            } else if (getColorUtil().getColorMapParameters() != null) {
                // restore if null
                getCapability(ColorMapCapability.class).setColorMapParameters(
                        getColorUtil().getColorMapParameters());
            }

            // draw stream trace?
            if (getShowStream() && isStreamFollow()) {
                paintUpAndDownStream(aTarget, paintProps, isShaded);
            }

            // always reset
            isQuery = false;
        }
    }

    /**
     * Draws the field text string
     * 
     * @param target
     * @param paintProps
     * @throws VizException
     */
    private void paintProductString(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        double[] pixel = paintProps.getView().getDisplayCoords(
                new double[] { 110, 50 }, target);

        if (isAutoRefresh || isQuery) {
            fieldDescString.setText("FFMP " + df.format(getTime()) + " hour "
                    + FFMPRecord.getFieldLongDescription(getField()),
                    getCapability(ColorableCapability.class).getColor());
        }

        fieldDescString.setCoordinates(pixel[0], pixel[1]);
        target.drawStrings(fieldDescString);
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
        double[] world = this.descriptor.worldToPixel(new double[] {
                getCenter().x, getCenter().y });
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
            PaintProperties paintProps, boolean isShaded) throws VizException {

        if (basinTraceColor == null) {
            basinTraceColor = getCapability(ColorableCapability.class)
                    .getColor();
        }

        if (getShowStream() && (streamShadedShape != null)
                && streamShadedShape.isDrawable() && isShaded) {
            target.drawShadedShape(streamShadedShape, paintProps.getAlpha());
        }
        if (getShowStream() && (streamOutlineShape != null)
                && streamOutlineShape.isDrawable()
                && getCapability(OutlineCapability.class).isOutlineOn()) {
            target.drawWireframeShape(streamOutlineShape, basinTraceColor,
                    getCapability(OutlineCapability.class).getOutlineWidth(),
                    getCapability(OutlineCapability.class).getLineStyle());
        } else if ((streamOutlineShape == null)
                && getCapability(OutlineCapability.class).isOutlineOn()) {
            target.setNeedsRefresh(true);
        }
    }

    @Override
    public void project(CoordinateReferenceSystem mapData) throws VizException {

        if (shadedShapes != null) {
            shadedShapes.clear();
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
                String schema = getResourceData().getTables()[0]
                        .substring(0, p);
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

    protected boolean isLineal() {
        return getGeometryType().endsWith("LINESTRING");
    }

    protected boolean isPolygonal() {
        return getGeometryType().endsWith("POLYGON");
    }

    /**
     * Get color wrapper class
     * 
     * @return
     * @throws VizException
     */
    private FFMPColorUtils getColorUtil() throws VizException {
        if (colorUtil == null) {
            colorUtil = new FFMPColorUtils(getField(), getTime(),
                    getResourceData().tableLoad);
        }
        return colorUtil;
    }

    /*
     * Inspect may frequently be implemented by calling interrogate, and
     * formatting the result as a string.
     * 
     * @param coord the coordinate
     * 
     * @return a string that represents the result of inspecting that point
     * 
     * @throws VizException
     */
    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        // No inspection by default
        StringBuffer buf = new StringBuffer();
        Long pfaf = null;
        boolean aggregate = false;
        try {
            FFMPBasinMetaData metaBasin = monitor.getTemplates(getSiteKey())
                    .findBasinByLatLon(getSiteKey(), coord.asLatLon());
            if (getHuc().equals(ALL) || centeredAggregationKey != null) {
                pfaf = metaBasin.getPfaf();
                if (isMaintainLayer) {
                    pfaf = monitor.getTemplates(getSiteKey())
                            .findAggregatedPfaf(pfaf, getSiteKey(), getHuc());
                    aggregate = true;
                }
            } else {
                pfaf = monitor.getTemplates(getSiteKey()).findAggregatedPfaf(
                        metaBasin.getPfaf(), getSiteKey(), getHuc());
                if (pfaf == null) {
                    pfaf = metaBasin.getPfaf();
                } else {
                    aggregate = true;
                }
            }
            if ((metaBasin != null) && (pfaf != null)) {
                if (!getHuc().equals("COUNTY")
                        || (centeredAggregationKey != null)) {
                    buf.append("Pfaf ID:  " + pfaf + "\n");
                    buf.append("Basin Name:  " + metaBasin.getStreamName()
                            + "\n");
                    buf.append("Root River:  " + metaBasin.getHucName() + "\n");

                }
                buf.append("County:  " + metaBasin.getState() + ", "
                        + metaBasin.getCounty() + "\n");
                String valst = null;
                Float val = getBasinValue(pfaf, getPaintTime().getRefTime(),
                        aggregate);
                if (val.isNaN() || (val == FFMPUtils.MISSING)) {
                    valst = "NO DATA";
                } else {
                    valst = df.format(getBasinValue(pfaf, getPaintTime()
                            .getRefTime(), aggregate));
                }

                if (!valst.equals("NO DATA")) {
                    buf.append(getField().getFieldName() + ":  " + valst + " "
                            + FFMPRecord.getUnitType(getField()));
                } else {
                    buf.append(getField().getFieldName() + ":  " + valst);
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
            // valst = getField().getFieldName() + ":  NO DATA";
            // } else {
            // valst = getField().getFieldName() + ":  "
            // + df.format(val) + " "
            // + FFMPRecord.getUnitType(getField());
            // }
            //
            // buf.append(valst);
            // }
            // }
            // }
        } catch (TransformException te) {
            te.printStackTrace();
            buf.append("");
        } catch (FactoryException fe) {
            fe.printStackTrace();
            buf.append("");
        } catch (NullPointerException npe) {
            buf.append("");
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
     * Gets the screen center from table
     * 
     * @return Coordinate
     */
    private Coordinate getCenter() {
        return center;
    }

    /**
     * Sets the screen center from the table
     * 
     * @param center
     */
    private void setCenter(FFMPZoom fz) {
        boolean isUpdateDialog = true;

        int mapWidth = getDescriptor().getMapWidth() / 1000;
        FFMPTemplates templates = monitor.getTemplates(getSiteKey());
        String huc = getHuc();
        boolean isAllHuc = huc.equals(ALL);

        if (centeredAggregationKey == null) {
            centeredAggregationKey = fz.getKey();
            if (!isAllHuc) {
                center = templates.findAggregationCenter(
                        (Long) centeredAggregationKey, getSiteKey(), huc);

            } else {
                setCenter(fz.getKey());
                isUpdateDialog = false;
            }

            getDescriptor().getRenderableDisplay().getExtent().reset();
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

        } else if (!centeredAggregationKey.equals(fz.getKey())) {
            centeredAggregationKey = fz.getKey();
            if (lowestCenter == FFMPRecord.ZOOM.WFO) {
                if (!isAllHuc) {
                    centeredAggregationKey = fz.getKey();
                    center = templates.findAggregationCenter(
                            (Long) centeredAggregationKey, getSiteKey(),
                            getHuc());
                } else {
                    setCenter(fz.getKey());
                    isUpdateDialog = false;
                }

                getDescriptor().getRenderableDisplay().getExtent().reset();
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
                setCenter(fz.getKey());
                if (lowestCenter != FFMPRecord.ZOOM.BASIN) {
                    getDescriptor().getRenderableDisplay().getExtent().reset();
                    float zoomLevel = (float) BASIN_ZOOM / mapWidth;
                    zoom(zoomLevel);
                    lowestCenter = FFMPRecord.ZOOM.BASIN;
                }
            }

        } else {
            centeredAggregationKey = fz.getKey();
            setCenter(fz.getKey());
            isUpdateDialog = false;
            lowestCenter = FFMPRecord.ZOOM.BASIN;
        }

        if (getResourceData().tableLoad) {

            if (isUpdateDialog) {
                updateDialog();
            }

            // stops the annoying wait cursor every time you re-center
            if (isAllHuc || (lowestCenter == FFMPRecord.ZOOM.BASIN)) {
                basinTableDlg.getShell().setCursor(null);
            }
        }

        // reset the screen as if it where a pan
        if (center != null) {
            getDescriptor().getRenderableDisplay().recenter(
                    new double[] { center.x, center.y });
        }
    }

    /**
     * Zoom the screen
     * 
     * @param zoomFactor
     */
    public void zoom(float zoomFactor) {
        getDescriptor().getRenderableDisplay().zoom(zoomFactor);
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
            }

            // only re-draw the screen for UP/DOWN
            if (!stream.equals(FFMPRecord.CLICK_TYPE.TREND)) {
                nextStreamPfaf = newPfaf;
                dirty();

            }
        } else {
            Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getShell();
            MessageBox mb = new MessageBox(shell, SWT.ICON_INFORMATION | SWT.OK);
            mb.setText("FFMP:  Invalid Basin Selection");
            mb.setMessage("The clicked basin is not in the list of FFMP basins.  Re-select one!");
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
            ArrayList<Long> pfafs) {

        float qpe = 0.0f;
        float guid = 0.0f;
        float diff = Float.NaN;

        try {
            if (aggregate) {
                if (isWorstCase()) {
                    ArrayList<Float> qpes = null;
                    ArrayList<Float> guids = null;
                    if ((getQpeRecord() != null)
                            && (getGuidanceRecord() != null)) {
                        qpes = getQpeRecord().getBasinData(ALL).getAccumValues(
                                pfafs, getTableTime(), recentTime,
                                getQpeSourceExpiration(), isRate());

                        guids = getGuidanceRecord().getBasinData(ALL)
                                .getGuidanceValues(pfafs,
                                        getGuidanceInterpolation(getFFGName()),
                                        getGuidSourceExpiration());
                    }
                    if ((qpes != null) && (guids != null)) {
                        diff = FFMPUtils.getMaxDiffValue(qpes, guids);
                    }
                } else {
                    if ((getQpeRecord() != null)
                            && (getGuidanceRecord() != null)) {
                        qpe = getQpeRecord()
                                .getBasinData(getHuc())
                                .get(key)
                                .getAccumValue(
                                        getTableTime(),
                                        recentTime,
                                        getQpeSourceExpiration(),
                                        getResourceData().getPrimarySourceXML()
                                                .isRate());

                        guid = getGuidanceValue(
                                (FFMPGuidanceBasin) getGuidanceRecord()
                                        .getBasinData(getHuc()).get(key),
                                recentTime, getFFGName());

                        diff = FFMPUtils.getDiffValue(qpe, guid);
                    }
                }
            } else {
                if ((getQpeRecord() != null) && (getGuidanceRecord() != null)) {
                    qpe = getQpeRecord()
                            .getBasinData(ALL)
                            .get(key)
                            .getAccumValue(getTableTime(), recentTime,
                                    getQpeSourceExpiration(), isRate());

                    guid = getGuidanceValue(
                            (FFMPGuidanceBasin) getGuidanceRecord()
                                    .getBasinData(ALL).get(key), recentTime,
                            getFFGName());
                    guid = forceValue(pfafs,
                            getBasin(key, getField(), recentTime, aggregate),
                            guid);
                    diff = FFMPUtils.getDiffValue(qpe, guid);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
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
            ArrayList<Long> pfafs) {

        float qpe = 0.0f;
        float guid = 0.0f;
        float ratio = Float.NaN;

        try {
            if (aggregate) {
                if (isWorstCase()) {
                    ArrayList<Float> qpes = null;
                    ArrayList<Float> guids = null;
                    if (getQpeRecord() != null) {
                        qpes = getQpeRecord().getBasinData(ALL).getAccumValues(
                                pfafs, getTableTime(), recentTime,
                                getQpeSourceExpiration(), isRate());
                    }
                    if (getGuidanceRecord() != null) {
                        guids = getGuidanceRecord().getBasinData(ALL)
                                .getGuidanceValues(pfafs,
                                        getGuidanceInterpolation(getFFGName()),
                                        getGuidSourceExpiration());
                    }
                    if ((qpes != null) && (guids != null)) {
                        ratio = FFMPUtils.getMaxRatioValue(qpes, guids);
                    }
                } else {
                    if ((getQpeRecord() != null)
                            && (getGuidanceRecord() != null)) {
                        qpe = getQpeRecord()
                                .getBasinData(getHuc())
                                .get(key)
                                .getAccumValue(
                                        getTableTime(),
                                        recentTime,
                                        getQpeSourceExpiration(),
                                        getResourceData().getPrimarySourceXML()
                                                .isRate());
                        guid = getGuidanceValue(
                                (FFMPGuidanceBasin) getGuidanceRecord()
                                        .getBasinData(getHuc()).get(key),
                                recentTime, getFFGName());
                        ratio = FFMPUtils.getRatioValue(qpe, guid);
                    }
                }
            } else {
                if ((getQpeRecord() != null) && (getGuidanceRecord() != null)) {
                    qpe = getQpeRecord()
                            .getBasinData(ALL)
                            .get(key)
                            .getAccumValue(getTableTime(), recentTime,
                                    getQpeSourceExpiration(), isRate());
                    guid = getGuidanceValue(
                            (FFMPGuidanceBasin) getGuidanceRecord()
                                    .getBasinData(ALL).get(key), recentTime,
                            getFFGName());
                    ratio = FFMPUtils.getRatioValue(qpe, guid);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return ratio;
    }

    /**
     * close our dialog(s)
     */
    public void closeDialog() {
        monitor.closeDialog(this);
    }

    public void allowNewTableUpdate() {
        if (basinTableDlg != null) {
            basinTableDlg.allowNewTableUpdate();
        }
    }

    /**
     * set the query runner
     */
    public void setQuery(boolean isQuery) {
        this.isQuery = isQuery;
    }

    /**
     * clear them
     */
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
                entry.getValue().removeTable(ALL);
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

    	//target.drawLine(lines)
        target.drawLine(pc.getLl().x, pc.getLl().y, 0.0, pc.getUl().x, pc
                .getUl().y, 0.0, getCapability(ColorableCapability.class)
                .getColor(), getCapability(OutlineCapability.class)
                .getOutlineWidth(), getCapability(OutlineCapability.class)
                .getLineStyle());
        target.drawLine(pc.getUl().x, pc.getUl().y, 0.0, pc.getUr().x, pc
                .getUr().y, 0.0, getCapability(ColorableCapability.class)
                .getColor(), getCapability(OutlineCapability.class)
                .getOutlineWidth(), getCapability(OutlineCapability.class)
                .getLineStyle());
        target.drawLine(pc.getUr().x, pc.getUr().y, 0.0, pc.getLr().x, pc
                .getLr().y, 0.0, getCapability(ColorableCapability.class)
                .getColor(), getCapability(OutlineCapability.class)
                .getOutlineWidth(), getCapability(OutlineCapability.class)
                .getLineStyle());
        target.drawLine(pc.getLr().x, pc.getLr().y, 0.0, pc.getLl().x, pc
                .getLl().y, 0.0, getCapability(ColorableCapability.class)
                .getColor(), getCapability(OutlineCapability.class)
                .getOutlineWidth(), getCapability(OutlineCapability.class)
                .getLineStyle());

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

        return (BOX_WIDTH / 3.0) / screenToWorldWidthRatio;
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

        return (BOX_HEIGHT / 3.0) / screenToWorldHeightRatio;
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

        for (String lid : vgbDrawables.keySet()) {
            drawSquare(vgbDrawables.get(lid), aTarget);
        }
    }

    /**
     * See if you are in the coverage of this feature
     * 
     * @param c
     * @return
     */
    private boolean contains(PixelCoverage pc, Coordinate c) {
        boolean inside = false;
        double[] center = descriptor.worldToPixel(new double[] { c.x, c.y });

        if ((center[0] > pc.getMinX()) && (center[0] < pc.getMaxX())
                && (center[1] > pc.getMinY()) && (center[1] < pc.getMaxY())) {
            inside = true;
        }
        return inside;
    }

    /**
     * Set the center
     * 
     * @param key
     */
    private void setCenter(Object key) {
        FFMPTemplates templates = monitor.getTemplates(getSiteKey());
        if (key instanceof Long) {

            Long pfaf = (Long) key;
            HucLevelGeometriesFactory hucGeomFactory = HucLevelGeometriesFactory
                    .getInstance();
            for (DomainXML domain : getDomains()) {
                try {
                    Map<Long, Geometry> map = hucGeomFactory.getGeometries(
                            templates, getSiteKey(), domain.getCwa(), ALL);

                    if (map.containsKey(pfaf)) {
                        center = map.get(pfaf).getCentroid().getCoordinate();
                        break;
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }

        } else {

            center = monitor.getTemplates(getSiteKey())
                    .getVirtualGageBasinMetaData(getSiteKey(), (String) key)
                    .getCoordinate();

        }
    }

    /**
     * recurse over up stream basins
     * 
     * @param basinIds
     */
    private void getUpStreamBasins(ArrayList<Long> basinIds) {

        for (Long pfaf : basinIds) {

            try {
                streamPfafIds.add(pfaf);

                ArrayList<Long> newBasinIds = monitor
                        .getTemplates(getSiteKey()).getUpStreamBasins(
                                getSiteKey(), pfaf);

                if ((newBasinIds != null) && (newBasinIds.size() > 0)) {
                    getUpStreamBasins(newBasinIds);
                }
            } catch (NullPointerException npe) {
                // exhausted all avenues
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

        try {
            Long newBasinId = monitor.getTemplates(getSiteKey())
                    .getDownStreamBasins(getSiteKey(), pfaf);
            streamPfafIds.add(pfaf);

            if (newBasinId != null) {
                getDownStreamBasins(newBasinId);
            }

        } catch (NullPointerException npe) {
            // exhausted all avenues
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

    /**
     * Gets the VGB value when querried
     * 
     * @param pfaf
     * @return
     */
    private float getVGBValue(Long pfaf, Date recentTime) {
        float value = 0.0f;
        if (getField() == FIELDS.RATE) {
            value = getVirtualRecord().getBasinsMap().get(ALL).get(pfaf)
                    .getValue(recentTime);
        } else if (getField() == FIELDS.QPE) {
            value = getVirtualRecord()
                    .getBasinsMap()
                    .get(ALL)
                    .get(pfaf)
                    .getAccumValue(getTableTime(), getMostRecentTime(),
                            getQpeSourceExpiration(), isRate());
        } else if (getField() == FIELDS.RATIO) {
            float qpe = getVirtualRecord()
                    .getBasinsMap()
                    .get(ALL)
                    .get(pfaf)
                    .getAccumValue(getTableTime(), getMostRecentTime(),
                            getQpeSourceExpiration(), isRate());
            float guidance = getGuidanceValue(
                    ((FFMPGuidanceBasin) getGuidanceRecord().getBasinsMap()
                            .get(ALL).get(pfaf)), recentTime, getFFGName());
            value = FFMPUtils.getRatioValue(qpe, guidance);
        } else if (getField() == FIELDS.DIFF) {
            float qpe = getVirtualRecord()
                    .getBasinsMap()
                    .get(ALL)
                    .get(pfaf)
                    .getAccumValue(getTableTime(), getMostRecentTime(),
                            getQpeSourceExpiration(), isRate());
            float guidance = getGuidanceValue(
                    ((FFMPGuidanceBasin) getGuidanceRecord().getBasinsMap()
                            .get(ALL).get(pfaf)), recentTime, getFFGName());
            value = FFMPUtils.getDiffValue(qpe, guidance);
        } else if (getField() == FIELDS.GUIDANCE) {
            value = getGuidanceValue(((FFMPGuidanceBasin) getGuidanceRecord()
                    .getBasinsMap().get(ALL).get(pfaf)), recentTime,
                    getFFGName());
        }
        return value;
    }

    private class FFMPDataRetrievalJob extends Job {
        private static final int QUEUE_LIMIT = 1;

        private HucLevelGeometriesFactory hucGeomFactory = HucLevelGeometriesFactory
                .getInstance();

        private ArrayBlockingQueue<Request> requestQueue = new ArrayBlockingQueue<Request>(
                QUEUE_LIMIT);

        public FFMPDataRetrievalJob() {
            super("Retrieving FFMP map information...");
        }

        private class Request {

            IGraphicsTarget target;

            IMapDescriptor descriptor;

            boolean shaded;

            FFMPDrawable drawable;

            PixelExtent extent;

            DataTime time;

            Request(IGraphicsTarget target, IMapDescriptor descriptor,
                    boolean shaded, FFMPDrawable drawable, PixelExtent extent,
                    DataTime time) {
                this.target = target;
                this.descriptor = descriptor;
                this.shaded = shaded;
                this.drawable = drawable;
                this.extent = extent;
                this.time = time;
            }
        }

        public void request(IGraphicsTarget target, IMapDescriptor descriptor,
                boolean shaded, FFMPDrawable drawable, PixelExtent extent,
                DataTime time) {
            if (drawable != null) {
                if (requestQueue.size() == QUEUE_LIMIT) {
                    requestQueue.poll();
                }

                Request req = new Request(target, descriptor, shaded, drawable,
                        extent, time);
                requestQueue.add(req);
                this.schedule();
            }
        }

        /**
         * Determines the area basins. Return true if they have changed, false
         * otherwise.
         * 
         * @param req
         * @param templates
         * @param aggrHuc
         * @return
         * @throws VizException
         */
        private Set<Long> getAreaBasins(String cwa, Request req, String phuc) {
            FFMPTemplates templates = monitor.getTemplates(getSiteKey());
            FFMPDrawable drawable = req.drawable;
            Set<Long> cwaBasins = drawable.getBasins(cwa);
            if (cwaBasins == null) {
                cwaBasins = new HashSet<Long>();
                drawable.setBasins(cwa, cwaBasins);
            }
            if ((cwaBasins.size() == 0)
                    || !req.extent.equals(drawable.getExt())
                    || !phuc.equals(drawable.getHuc()) || restoreTable) {
                Envelope env = null;
                try {
                    Envelope e = req.descriptor.pixelToWorld(req.extent,
                            req.descriptor.getCRS());
                    ReferencedEnvelope ref = new ReferencedEnvelope(e,
                            req.descriptor.getCRS());
                    env = ref.transform(MapUtil.LATLON_PROJECTION, true);
                } catch (Exception e) {
                    System.out.println("Error transforming extent");
                }

                cwaBasins.clear();

                try {
                    // use the envelopes from HUC0 to speed processing
                    // if necessary
                    Map<Long, Envelope> envMap = hucGeomFactory.getEnvelopes(
                            templates, getSiteKey(), cwa, phuc);
                    for (Entry<Long, Envelope> entry : envMap.entrySet()) {

                        if (env.intersects(entry.getValue())
                                || env.contains(entry.getValue())) {
                            // add the individual basins
                            cwaBasins.add(entry.getKey());
                        }
                    }
                } catch (Exception e) {
                    statusHandler.handle(Priority.DEBUG, "Domain: " + cwa
                            + " Outside of site: " + getSiteKey() + " area...");
                }
            }
            return cwaBasins;
        }

        /*
         * (non-Javadoc)
         * 
         * @seeorg.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
         * IProgressMonitor)
         */
        @SuppressWarnings({ "unchecked" })
        @Override
        protected IStatus run(IProgressMonitor progMonitor) {

            VizApp.runSync(new Runnable() {

                @Override
                public void run() {
                    Shell fshell = PlatformUI.getWorkbench()
                            .getActiveWorkbenchWindow().getShell();
                    fshell.setCursor(fshell.getDisplay().getSystemCursor(
                            SWT.CURSOR_WAIT));
                }

            });

            Request req = requestQueue.poll();
            while (req != null) {
                try {
                    // long t0 = System.currentTimeMillis();
                    FFMPDrawable drawable = req.drawable;
                    FFMPTemplates templates = monitor
                            .getTemplates(getSiteKey());

                    String phuc = getHuc();
                    boolean isAllPhuc = phuc.equals(ALL);
                    FIELDS field = getField();

                    if (getResourceData().tableLoad) {
                        if (!getFFGName().equals(drawable.getGuidType())) {
                            drawable.disposeImage();
                        }
                    }

                    boolean globalRegen = !(phuc.equals(drawable.getHuc())
                            && field.equals(drawable.getField())
                            && lastExtent.getEnvelope().contains(
                                    drawable.getExt().getEnvelope())
                            && req.time.equals(drawable.getTime())
                            && ((centeredAggregationKey == null) || !centeredAggregationKey
                                    .equals(drawable.getCenterAggrKey()))
                            && !drawable.isDirty()
                            && (isParent() == drawable.isParent())
                            && (isMaintainLayer() == drawable.isMaintainLayer()) && (isWorstCase() == drawable
                            .isWorstCase()));

                    if (globalRegen) {
                        resetRecords();
                    }

                    for (DomainXML domain : getDomains()) {
                        String cwa = domain.getCwa();

                        // Added isDirty call to if check to force a repaint of
                        // the
                        // the basin when the color map changes.
                        if (globalRegen || drawable.genCwa(cwa)) {
                            // System.out
                            // .println("Regenerating the entire image: CWA: +"
                            // + cwa
                            // + " Table:"
                            // + resourceData.tableLoad);
                            // get base aggr basins that are in screen area
                            Set<Long> cwaPfafs = null;
                            cwaPfafs = getAreaBasins(cwa, req, phuc);
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
                                                            getSiteKey(), phuc);
                                        } else {
                                            centeredAggr = (Long) drawable
                                                    .getCenterAggrKey();
                                            // this is a fall back for VGB's
                                            if (centeredAggr == null) {
                                                centeredAggr = templates
                                                        .findAggregatedVGB(
                                                                (String) centeredAggregationKey,
                                                                getSiteKey(),
                                                                phuc);
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
                                                                getSiteKey(),
                                                                phuc);
                                            }
                                        }
                                    }

                                    if (isParent()
                                            || (!isParent() && !isMaintainLayer())) {

                                        if (cwaPfafs.contains(centeredAggr)) {
                                            pfafsToProcess = new HashSet<Long>();
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

                            if ((pfafsToProcess != null)
                                    && (pfafsToProcess.size() > 0)) {

                                HashMap<Object, RGB> colorMap = new HashMap<Object, RGB>();
                                String shadedHuc = null;

                                if (!isAllPhuc) {

                                    Map<Long, Geometry> geomMap = hucGeomFactory
                                            .getGeometries(templates,
                                                    getSiteKey(), cwa, phuc);

                                    for (Long pfaf : pfafsToProcess) {

                                        if (!isMaintainLayer() && isParent()
                                                && pfaf.equals(centeredAggr)) {
                                            // add centered aggr to shape
                                            Collection<Long> allPfafs = null;

                                            if (isParent()) {
                                                allPfafs = templates.getMap(
                                                        getSiteKey(), cwa, ALL)
                                                        .keySet();
                                            } else {
                                                allPfafs = (List<Long>) (templates
                                                        .getMap(getSiteKey(),
                                                                cwa, phuc)
                                                        .get(centeredAggr));
                                            }

                                            if (allPfafs != null) {
                                                Map<Long, Geometry> allGeomMap = hucGeomFactory
                                                        .getGeometries(
                                                                templates,
                                                                getSiteKey(),
                                                                cwa, ALL);
                                                IColormapShadedShape shape = shadedShapes
                                                        .getShape(cwa, ALL,
                                                                req.target,
                                                                descriptor);
                                                shadedHuc = ALL;

                                                for (Long allPfaf : allPfafs) {

                                                    generateShapes(templates,
                                                            ALL, allPfaf,
                                                            allGeomMap, req,
                                                            shape, colorMap);
                                                }
                                            }
                                        } else if (!isMaintainLayer()
                                                && !isParent()
                                                && !ALL.equals(phuc)
                                                && pfaf.equals(centeredAggr)) {

                                            Collection<Long> allPfafs = templates
                                                    .getMap(getSiteKey(), cwa,
                                                            ALL).keySet();

                                            if (allPfafs != null) {

                                                Map<Long, Geometry> allGeomMap = hucGeomFactory
                                                        .getGeometries(
                                                                templates,
                                                                getSiteKey(),
                                                                cwa, ALL);

                                                IColormapShadedShape shape = shadedShapes
                                                        .getShape(cwa, ALL,
                                                                req.target,
                                                                descriptor);

                                                shadedHuc = ALL;

                                                for (Long allPfaf : allPfafs) {

                                                    generateShapes(templates,
                                                            ALL, allPfaf,
                                                            allGeomMap, req,
                                                            shape, colorMap);
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

                                    if (pfafsToProcess != null) {

                                        Map<Long, Geometry> allGeomMap = hucGeomFactory
                                                .getGeometries(templates,
                                                        getSiteKey(), cwa, ALL);

                                        IColormapShadedShape shape = shadedShapes
                                                .getShape(cwa, ALL, req.target,
                                                        descriptor);

                                        shadedHuc = ALL;

                                        for (Long allPfaf : pfafsToProcess) {
                                            generateShapes(templates, ALL,
                                                    allPfaf, allGeomMap, req,
                                                    shape, colorMap);
                                        }
                                    }
                                }

                                drawable.setColorMap(cwa, colorMap);
                                drawable.setShadedHuc(shadedHuc);
                                req.target.setNeedsRefresh(true);
                            }

                            // long t2 = System.currentTimeMillis();
                            // StringBuilder msg = new StringBuilder(300);

                            // msg.append("FFMP ").append(cwa)
                            // .append(" time data:\n");
                            // msg.append("  determing basins: ").append(t1 -
                            // t0)
                            // .append("ms\n");
                            // msg.append("  Wireframe construction: ")
                            // .append(t2 - t1).append("ms\n");
                            // msg.append("  Total time: ").append(t2 - t0)
                            // .append("ms");
                            // System.out.println(msg.toString());
                        }
                    }

                    if (restoreTable) {
                        restoreTable = false;
                    }

                    drawable.setTime(req.time);
                    if (lowestCenter != ZOOM.BASIN) {
                        drawable.setCenterAggrKey(centeredAggregationKey);
                    }
                    drawable.setExt(req.extent);
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
                    monitor.splashDisposeAndDataLoad(getResource());

                    if (getResourceData().tableLoad && isFirst) {
                        isFirst = false;
                        updateDialog();
                    }
                }

            });

            return Status.OK_STATUS;
        }

        private void generateShapes(FFMPTemplates templates, String huc,
                Long pfaf, Map<Long, Geometry> geomMap, Request req,
                IColormapShadedShape shape, HashMap<Object, RGB> colorMap) {

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
                            !ALL.equals(huc));

                    if (color != null) {
                        if (!shape.getColorKeys().contains(pfaf)) {
                            shape.addGeometry((Geometry) g.clone(), pfaf);
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

        private void generateStreamShapes(FFMPTemplates templates, Request req)
                throws VizException {
            // run stream queries
            if (streamPfafIds != null) {
                streamPfafIds.clear();
            } else {
                streamPfafIds = new HashSet<Long>();
            }

            IWireframeShape localWireframeShape = null;
            IShadedShape localShadedShape = null;

            // make new one's
            if (nextStreamPfaf != null) {
                if (FFMPRecord.CLICK_TYPE.UP.equals(stream)
                        || FFMPRecord.CLICK_TYPE.UP_DOWN.equals(stream)) {

                    ArrayList<Long> upstreamBasins = monitor.getTemplates(
                            getSiteKey()).getUpStreamBasins(getSiteKey(),
                            nextStreamPfaf);
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
                        descriptor, 0.0f);
                if (req.shaded) {
                    localShadedShape = req.target.createShadedShape(false,
                            descriptor.getGridGeometry(), true);
                }

                JTSCompiler jtsCompiler2 = new JTSCompiler(localShadedShape,
                        localWireframeShape, descriptor, PointStyle.CROSS);

                if (basinTraceColor == null) {
                    basinTraceColor = getCapability(ColorableCapability.class)
                            .getColor();
                }

                // read in geometries
                try {
                    for (DomainXML domains : templates.getDomains()) {
                        String cwa = domains.getCwa();

                        Map<Long, Geometry> geomMap = hucGeomFactory
                                .getGeometries(templates, getSiteKey(), cwa,
                                        ALL);

                        for (Long pfaf : streamPfafIds) {
                            // TODO: streamPfafIds should be ordered by
                            // domain
                            // already...
                            try {
                                Geometry g = geomMap.get(pfaf);
                                if (g != null) {
                                    jtsCompiler2.handle((Geometry) g.clone(),
                                            basinTraceColor);

                                }
                            } catch (Exception e) {
                                // just a missing geometry, no biggie
                            }
                        }
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }

                if (localWireframeShape != null) {
                    localWireframeShape.compile();
                    if (streamOutlineShape != null) {
                        streamOutlineShape.dispose();
                    }
                }

                if (req.shaded && (localShadedShape != null)) {
                    localShadedShape.compile();
                    localShadedShape.setFillPattern(FFMPUtils.STREAM_FILL);
                    if (streamShadedShape != null) {
                        streamShadedShape.dispose();
                    }
                }

                streamOutlineShape = localWireframeShape;
                streamShadedShape = localShadedShape;

            }
        }

        private void generateSmallBasins(FFMPTemplates templates, Request req)
                throws VizException {

            IWireframeShape basinShape = null;
            // create the frames/shaded shapes here
            try {
                // read in geometries
                basinShape = req.target.createWireframeShape(false, descriptor,
                        0.0f);
                JTSCompiler jtsCompiler3 = new JTSCompiler(null, basinShape,
                        descriptor, PointStyle.CROSS);
                RGB color = getCapability(ColorableCapability.class).getColor();

                for (DomainXML domains : templates.getDomains()) {
                    String cwa = domains.getCwa();
                    Map<Long, Geometry> geomMap = hucGeomFactory.getGeometries(
                            templates, getSiteKey(), cwa, ALL);

                    if (geomMap != null) {
                        for (Long pfaf : geomMap.keySet()) {
                            Geometry g = geomMap.get(pfaf);
                            if (g != null) {
                                jtsCompiler3
                                        .handle((Geometry) g.clone(), color);
                            }
                        }
                    }
                }
            } catch (Exception e) {
                e.printStackTrace();
            }

            if (basinShape != null) {
                basinShape.compile();
            }

            if (smallBasinOverlayShape != null) {
                smallBasinOverlayShape.dispose();
            }

            smallBasinOverlayShape = basinShape;
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.core.runtime.jobs.Job#canceling()
         */
        @Override
        protected void canceling() {
            super.canceling();
        }
    }

    @Override
    public void cwaChanged(FFMPCWAChangeEvent fcce) {

        @SuppressWarnings("unchecked")
        ArrayList<String> domainNames = (ArrayList<String>) fcce.getSource();
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
    }

    @Override
    public void timeChanged(FFMPTimeChangeEvent fhce, FFMPRecord.FIELDS fieldArg)
            throws VizException {

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
        getDescriptor().getRenderableDisplay().getExtent().reset();
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

    @Override
    public FFMPGraphData getGraphData(String pfafString) throws VizException {
        FfmpTableConfig tableConfig = FfmpTableConfig.getInstance();
        String ffgGraphType = tableConfig.getTableConfigData(getSiteKey())
                .getFfgGraphType();
        Long basinPfaf = null;
        Long dataId = null;
        FFMPVirtualGageBasinMetaData fvgbmd = null;
        FFMPBasin basin = null;

        // System.out.println("*************************************************");

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

        FFMPBasinMetaData mBasin = monitor.getTemplates(getSiteKey()).getBasin(
                getSiteKey(), basinPfaf); /*
                                           * TODO: mBasin is never used so it is
                                           * not clear if this should be
                                           * basinPfaf or dataId
                                           */

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

        // grabs the basins we need
        try {
            rateBasin = monitor.getGraphRateBasin(getProduct(), getSiteKey(),
                    getDataKey(), fvgbmd == null ? getProduct().getRate()
                            : getProduct().getVirtual(), oldestRefTime, ALL,
                    dataId);

            ArrayList<Double> rateTimes = new ArrayList<Double>();

            if (rateBasin != null) {
                for (Date date : rateBasin.getValues().keySet()) {

                    double dtime = FFMPGuiUtils.getTimeDiff(mostRecentRefTime,
                            date);
                    fgd.setRate(dtime, (double) rateBasin.getValue(date));
                    rateTimes.add(dtime);
                }
            }

            fgd.setRateTimes(rateTimes);

        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "FFMPMonitor: getGraphData(): missing RATE dataset.");
        }
        try {
            qpeBasin = monitor.getGraphQPEBasin(getProduct(), getSiteKey(),
                    getDataKey(), fvgbmd == null ? getProduct().getQpe()
                            : getProduct().getVirtual(), oldestRefTime, ALL,
                    dataId);

            ArrayList<Double> qpeTimes = new ArrayList<Double>();

            if (qpeBasin != null) {

                for (Date date : qpeBasin.getValues().keySet()) {

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
                    "FFMPMonitor: getGraphData(): missing QPE dataset.");
        }

        try {

            qpfBasin = monitor.getGraphQPFBasin(getProduct(), getSiteKey(),
                    getDataKey(), null, oldestRefTime, ALL, basinPfaf);

            Float qpfFloat = qpfBasin.getValue(monitor.getQpfWindow()
                    .getBeforeTime(), monitor.getQpfWindow().getAfterTime());

            fgd.setQpfValue(qpfFloat);

            ArrayList<Double> qpfTimes = new ArrayList<Double>();
            if (qpfBasin != null) {
                for (Date date : qpfBasin.getValues().keySet()) {

                    double dtime = FFMPGuiUtils.getTimeDiff(mostRecentRefTime,
                            date);

                    // TODO - START
                    // Float qpf =
                    // monitor.getq.get(qpfBasin.getPfaf()).getValue(
                    // monitor.getQpfWindow().getBeforeTime(),
                    // monitor.getQpfWindow().getAfterTime());

                    // TODO - END

                    fgd.setQpf(dtime, (double) qpfBasin.getValue(date));
                    qpfTimes.add(dtime);
                    // System.out.println("Have a time for QPF: " + dtime
                    // + " value: " + (double) qpfBasin.getValue(date));
                }
            }

            fgd.setQpfTimes(qpfTimes);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "FFMPMonitor: getGraphData(): missing QPF dataset.");
        }

        FFMPGuidanceInterpolation guidanceInterpolator = new FFMPGuidanceInterpolation(
                monitor.getSourceConfig(), getProduct(), monitor.getRunConfig()
                        .getRunner(getResourceData().wfo)
                        .getProduct(getSiteKey()), getPrimarySource(),
                ffgGraphType, getSiteKey());

        try {

            guidBasin = (FFMPGuidanceBasin) monitor.getGraphGuidanceBasin(
                    getProduct(), getSiteKey(), getDataKey(), null,
                    oldestRefTime, ALL, basinPfaf);
            ArrayList<Double> guidTimes = new ArrayList<Double>();
            for (SourceXML ffgSource : getProduct().getGuidanceSourcesByType(
                    ffgGraphType)) {
                if (guidBasin.getValue(ffgSource.getSourceName(),
                        guidanceInterpolator, getGuidSourceExpiration()) != null) {

                    double time = FFMPGuiUtils.getTimeDiff(mostRecentRefTime,
                            FFMPGuiUtils.getHourDisplacement(mostRecentRefTime,
                                    ffgSource.getDurationHour()));
                    fgd.setGuid(time, (double) guidBasin.getValue(
                            ffgSource.getSourceName(), guidanceInterpolator,
                            getGuidSourceExpiration()));
                    guidTimes.add(time);
                }
            }

            fgd.setGuidanceTimes(guidTimes);
            guid = true;
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "FFMPMonitor: getGraphData(): missing GUIDANCE dataset.");
        }

        if (fvgbmd != null) {
            try {
                // VGB's use a different timing sequenceFFMPResource
                String lid = fvgbmd.getLid();

                virtualBasin = monitor.getVirtualGageBasinData(dataId, lid,
                        mostRecentRefTime);

                if (virtualBasin != null) {
                    // System.out.println("VGB DATA EXISTS!     " + pfaf +
                    // "    "
                    // + lid);

                    SortedSet<Date> vgbTimes = new TreeSet<Date>(virtualBasin
                            .getValues().descendingKeySet());

                    fgd.setVirtualTimes(FFMPGuiUtils
                            .getTimeOffsetList(vgbTimes));
                    Iterator<Date> iter = vgbTimes.iterator();

                    for (int j = 0; j < fgd.getVirtualTimes().size(); j++) {
                        fgd.setVirtual(fgd.getVirtualTimes().get(j),
                                new Double(virtualBasin.getValue(iter.next())));
                    }
                }

            } catch (Exception e) {
                statusHandler
                        .handle(Priority.PROBLEM,
                                "FFMPMonitor: getGraphData(): missing VIRTUAL dataset.");
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
                                    guidanceInterpolator.getSource1(),
                                    guidanceInterpolator.getSource2(),
                                    guidanceInterpolator
                                            .getInterpolationOffset(),
                                    guidanceInterpolator,
                                    getGuidSourceExpiration());
                        } else {
                            if (guidanceInterpolator.getSource1() != null) {
                                guidancev = guidBasin.getValue(
                                        guidanceInterpolator.getSource1(),
                                        guidanceInterpolator,
                                        getGuidSourceExpiration());
                            }
                        }

                        diff = FFMPUtils.getDiffValue(qpev, guidancev);
                        ratio = FFMPUtils.getRatioValue(qpev, guidancev);
                    }

                    // System.out.println("----------------------------------");
                    // System.out.println("guid is: " + guid);
                    // System.out.println(">>> graphTime: = " + fgdQpeTime);
                    // System.out.println(">>> qpev = " + qpev);
                    // System.out.println(">>> guidancev = " + guidancev);
                    // System.out.println(">>> diff = " + diff);
                    // System.out.println(">>> ratio = " + ratio);

                    fgd.setRatio(fgdQpeTime, ratio);
                    fgd.setDiff(fgdQpeTime, diff);

                } catch (Exception e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "FFMP Can't retrieve graphing data", e);
                }
            }
        }

        return fgd;
    }

    public boolean getShowStream() {
        return showStream;
    }

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

        synchronized (tableTime) {
            Date recentTime = getMostRecentTime();
            long time = new Double(recentTime.getTime() - (TimeUtil.MILLIS_PER_HOUR)
                    * getTime()).longValue();
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
                .getSourceType().toLowerCase();
        FIELDS myField = null;
        if (sfield.equals(FFMPRecord.FIELDS.QPE.getFieldName())) {
            myField = FFMPRecord.FIELDS.QPE;
        } else if (sfield.equals(FFMPRecord.FIELDS.RATE.getFieldName())) {
            myField = FFMPRecord.FIELDS.RATE;
        } else if (sfield.equals(FFMPRecord.FIELDS.QPF.getFieldName())) {
            myField = FFMPRecord.FIELDS.QPF;
        } else if (sfield.equals(FFMPRecord.FIELDS.GUIDANCE.getFieldName())) {
            myField = FFMPRecord.FIELDS.GUIDANCE;
        } else if (sfield.equals("gage")) {
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
    public ArrayList<DomainXML> getDomains() {
        return getResourceData().getDomains();
    }

    /**
     * Sets the domains
     * 
     * @param domains
     */
    public void setDomains(List<DomainXML> domains) {
        getResourceData().setDomains((ArrayList<DomainXML>) domains);
    }

    /**
     * gets the product if available
     * 
     * @param sourceName
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
            ArrayList<Date> times = getTimeOrderedKeys();
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
                    ArrayList<Date> times = getTimeOrderedKeys();
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
    public synchronized ArrayList<Date> getTimeOrderedKeys() {
        if (timeOrderedKeys == null || !toKeysInitialized) {
            toKeysInitialized = true;

            // stand alone displays use this
            timeOrderedKeys = new ArrayList<Date>();

            try {
                Date oldestCurrentTime = getResourceData().getAvailableTimes()[0]
                        .getRefTime();
                Date oldestTime = new Date(oldestCurrentTime.getTime()
                        - (TimeUtil.MILLIS_PER_HOUR * 24));

                SortedSet<Date> keys = monitor.getAvailableUris(getSiteKey(),
                        getDataKey(), getPrimarySource(), oldestTime).keySet();

                for (Date date : keys) {
                    timeOrderedKeys.add(date);
                }

            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "No available times for the FFMPResource");
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

            sliderTime = Math
                    .floor(4 * (offset.doubleValue() / (TimeUtil.MILLIS_PER_HOUR)) + .25) / 4;
            // sliderTime = Math.floor(((offset.doubleValue() / (1000 * 3600)) +
            // .005) * 100) / 100;
            setTime(sliderTime);
        }
        return sliderTime;
    }

    /**
     * update the data in the dialog
     */
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
    private FFMPGuidanceInterpolation getGuidanceInterpolation(String guidType) {
        if (getResourceData().tableLoad) {
            if ((interpolationMap == null) || (interpolationMap.size() == 0)) {
                getGuidanceInterpolators();
            }

            return interpolationMap.get(guidType);
        }

        return null;
    }

    public HashMap<String, FFMPGuidanceInterpolation> getGuidanceInterpolators() {
        if ((interpolationMap == null) || (interpolationMap.size() == 0)) {
            interpolationMap = new HashMap<String, FFMPGuidanceInterpolation>();

            if (getProduct() != null) {
                ArrayList<String> guidTypes = monitor.getRunConfig()
                        .getRunner(getResourceData().wfo)
                        .getProduct(getSiteKey())
                        .getGuidanceTypes(getProduct());

                for (String guidType : guidTypes) {
                    FFMPGuidanceInterpolation interpolation = new FFMPGuidanceInterpolation(
                            monitor.getSourceConfig(), getProduct(), monitor
                                    .getRunConfig()
                                    .getRunner(getResourceData().wfo)
                                    .getProduct(getSiteKey()),
                            getPrimarySource(), guidType, getSiteKey());
                    interpolation.setInterpolationSources(getTime());
                    interpolationMap.put(guidType, interpolation);
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
            long fips = monitor.getTemplates(getSiteKey()).getCountyFipsByPfaf(
                    basin.getPfaf());
            basin.setCountyFips(fips);

            if (getResourceData().tableLoad) {
                // interpolating
                if (getGuidanceInterpolation(guidType).isInterpolate()) {
                    // Interpolating between sources
                    String source1 = getGuidanceInterpolation(guidType)
                            .getSource1();
                    String source2 = getGuidanceInterpolation(guidType)
                            .getSource2();
                    dvalue = basin.getInterpolatedValue(source1, source2,
                            getGuidanceInterpolation(guidType)
                                    .getInterpolationOffset(),
                            getGuidanceInterpolation(guidType),
                            getGuidSourceExpiration());
                } else {
                    dvalue = basin.getValue(getGuidanceInterpolation(guidType)
                            .getStandardSource(),
                            getGuidanceInterpolation(guidType),
                            getGuidSourceExpiration());
                }

                if (dvalue == FFMPUtils.MISSING) {
                    return Float.NaN;
                }
            } else {
                dvalue = basin.getValue(getPrimarySource(), recentTime,
                        getGuidanceInterpolation(guidType),
                        getGuidSourceExpiration());
            }
        }

        return dvalue;
    }

    public boolean isWorstCase() {
        return isWorstCase;
    }

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
    public ArrayList<Long> getCenteredAggregatePfafs() {
        if (centeredAggregatePfafList == null) {
            Long center = null;
            if (centeredAggregationKey instanceof String) {
                center = monitor.getTemplates(getSiteKey())
                        .findAggregatedVGB((String) centeredAggregationKey,
                                getSiteKey(), getHuc());

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
            ArrayList<Long> centeredAggregatePfafList) {
        this.centeredAggregatePfafList = centeredAggregatePfafList;
    }

    /**
     * Gets the GAP calculation for the FFMP dialog to display
     * 
     * @return Array of Gap data
     */
    public ArrayList<FFMPGap> getGaps() {
        synchronized (timeOrderedKeys) {
            return FFMPGap.getGaps(getTimeOrderedKeys(), getResourceData()
                    .getPrimarySourceXML().getExpirationMinutes(getSiteKey()),
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

        ArrayList<Date> removes = new ArrayList<Date>();
        for (Date date : getTimeOrderedKeys()) {
            if (date.before(ndate)) {
                removes.add(date);
            }
        }

        for (Date date : removes) {
            statusHandler.handle(Priority.INFO, "FFMP CACHE removing time: "
                    + date);
            getTimeOrderedKeys().remove(date);
        }

        monitor.purgeFFMPData(getResourceData().getProduct(), getResourceData()
                .getPrimarySource(), getSiteKey(), ndate);

        try {
            DataTime lastDate = getResourceData().getAvailableTimes()[getResourceData()
                    .getAvailableTimes().length - 1];

            for (DataTime dt : drawables.keySet()) {
                if (!dt.greaterThan(lastDate)) {
                    drawables.remove(dt);
                }
            }
        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
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
                    .getExpirationMinutes(getSiteKey()) * TimeUtil.MILLIS_PER_MINUTE;
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
                String qpfType = ffmpTableCfgData.getQpfType();

                source = getProduct().getQpfSourcesByType(qpfType).get(0);
            } else {
                source = FFMPSourceConfigurationManager.getInstance()
                        .getSource(getResourceData().sourceName);
            }
            qpfSourceExpiration = source.getExpirationMinutes(getSiteKey()) * TimeUtil.MILLIS_PER_MINUTE;
        }
        return qpfSourceExpiration;
    }

    /**
     * Gets the guidance source expiration
     * 
     * @return
     */
    public long getGuidSourceExpiration() {
        if (guidSourceExpiration == 0l) {
            if (getProduct() != null) {

                String guidSrc = FFMPConfig.getInstance().getFFMPConfigData()
                        .getIncludedGuids();
                if (guidSrc.contains(",")) {
                    String[] parts = guidSrc.split(",");
                    guidSrc = parts[0];
                }
                SourceXML source = getProduct().getGuidanceSourcesByType(
                        guidSrc).get(0);
                guidSourceExpiration = source
                        .getExpirationMinutes(getSiteKey()) * TimeUtil.MILLIS_PER_MINUTE;

            } else {
                guidSourceExpiration = monitor.getSourceConfig()
                        .getSource(resourceData.getPrimarySource())
                        .getExpirationMinutes(getSiteKey()) * TimeUtil.MILLIS_PER_MINUTE;
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

    @Override
    public void loadStatus(FFMPLoaderEvent event) {

        if (basinTableDlg != null) {
            // call to update the basin table dialog
            if (event.getSource() instanceof FFMPLoaderStatus) {
                FFMPLoaderStatus status = (FFMPLoaderStatus) event.getSource();
                basinTableDlg.updateLoadingLabel(status);
            }
        }
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
                // You can grab anyone of them within the type,
                // DisplayName is common between them. That's what has to be the
                // same
                SourceXML source = getProduct().getGuidanceSourcesByType(
                        guidSrc).get(0);
                ffgName = source.getDisplayName();
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
        if (drawables != null) {
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
     * Start up a loader
     * 
     * @param startDate
     * @param endDate
     * @param type
     */
    private void startLoader(Date startDate, Date endDate, LOADER_TYPE type) {

        ArrayList<String> hucsToLoad = new ArrayList<String>();

        if (isWorstCase) {
            hucsToLoad.add(ALL);
        }

        // tertiary loader only loads ALL
        if (type != LOADER_TYPE.TERTIARY) {
            if (!hucsToLoad.contains(getHuc())) {
                hucsToLoad.add(getHuc());
            }
        } else {
            if (!hucsToLoad.contains(ALL)) {
                hucsToLoad.add(ALL);
            }
        }
        // destroy any old loader
        if (loader != null) {
            loader = null;
        }

        loader = new FFMPDataLoader(getResourceData(), endDate, startDate,
                type, hucsToLoad);

        loader.addListener(this);

        try {
            if (!loader.isAlive()) {
                loader.start();
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, "FFMP " + type
                    + " Data update failed", e);
            loader.removeListener(this);
        }
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

    /**
     * Kicks off additional loaders that need to be fired off
     * 
     * @param loader
     * @param isDone
     */
    public void manageLoaders(FFMPLoaderStatus status) {

        if (status.getLoaderType() == LOADER_TYPE.SECONDARY) {
            if (status.isDone() && !this.getResourceData().isTertiaryLoad) {
                try {
                    Date startDate = new Date(getMostRecentTime().getTime()
                            - (6 * TimeUtil.MILLIS_PER_HOUR));
                    FFMPMonitor.getInstance().startLoad(this, startDate,
                            LOADER_TYPE.TERTIARY);
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Secondary Data Load failure", e);
                }
            }
        }

        // We don't really care about status of tertiary and general loaders
    }

}
