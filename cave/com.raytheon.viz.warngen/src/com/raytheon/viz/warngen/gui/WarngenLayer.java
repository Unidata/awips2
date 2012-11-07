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
package com.raytheon.viz.warngen.gui;

import java.awt.geom.Point2D;
import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TimeZone;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import org.eclipse.jface.action.IMenuManager;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.ui.PlatformUI;
import org.geotools.geometry.jts.JTS;
import org.geotools.referencing.GeodeticCalculator;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.activetable.ActiveTableRecord;
import com.raytheon.uf.common.dataplugin.warning.AbstractWarningRecord;
import com.raytheon.uf.common.dataplugin.warning.WarningRecord.WarningAction;
import com.raytheon.uf.common.dataplugin.warning.config.BulletActionGroup;
import com.raytheon.uf.common.dataplugin.warning.config.DialogConfiguration;
import com.raytheon.uf.common.dataplugin.warning.config.GridSpacing;
import com.raytheon.uf.common.dataplugin.warning.config.WarngenConfiguration;
import com.raytheon.uf.common.dataplugin.warning.gis.GeospatialData;
import com.raytheon.uf.common.dataplugin.warning.gis.GeospatialFactory;
import com.raytheon.uf.common.dataplugin.warning.gis.GeospatialMetadata;
import com.raytheon.uf.common.dataplugin.warning.util.CountyUserData;
import com.raytheon.uf.common.dataplugin.warning.util.GeometryUtil;
import com.raytheon.uf.common.geospatial.DestinationGeodeticCalculator;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.site.SiteMap;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.maps.MapManager;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.EditableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.uf.viz.core.rsc.tools.AwipsToolsResourceData;
import com.raytheon.viz.awipstools.ToolsDataManager;
import com.raytheon.viz.awipstools.common.StormTrackData;
import com.raytheon.viz.awipstools.common.stormtrack.AbstractStormTrackResource;
import com.raytheon.viz.awipstools.common.stormtrack.StormTrackState;
import com.raytheon.viz.awipstools.common.stormtrack.StormTrackState.DisplayType;
import com.raytheon.viz.awipstools.common.stormtrack.StormTrackState.LabelMode;
import com.raytheon.viz.awipstools.common.stormtrack.StormTrackState.Mode;
import com.raytheon.viz.awipstools.common.stormtrack.StormTrackUIManager;
import com.raytheon.viz.core.rsc.jts.JTSCompiler;
import com.raytheon.viz.radar.RadarHelper;
import com.raytheon.viz.warngen.WarngenException;
import com.raytheon.viz.warngen.gis.GisUtil;
import com.raytheon.viz.warngen.gis.PolygonUtil;
import com.raytheon.viz.warngen.util.CurrentWarnings;
import com.raytheon.viz.warngen.util.FipsUtil;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineSegment;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.geom.TopologyException;
import com.vividsolutions.jts.geom.prep.PreparedGeometry;
import com.vividsolutions.jts.geom.prep.PreparedGeometryFactory;
import com.vividsolutions.jts.io.ParseException;
import com.vividsolutions.jts.io.WKTReader;

/**
 * Warngen drawing layer. Need to do EVERYTHING in stereographic over centoid of
 * cwa/wfo... Then when template gets ran, it should convert to lat/lon
 * coordinates
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 4, 2010            mschenke     Initial creation
 * 02/01/2012   DR 14491   D. Friedman Load/unload only the maps not already loaded
 * 02/28/2012   DR 13596   Qinglu Lin  Call GisUtil.restoreAlaskaLon() in figurePoint().
 * 03/19/2012   DR 14690   Qinglu Lin  While newHatchedArea==null, handle the polygon differently
 *                                     for initial warning and followup (CON); and 
 *                                     convert ratio to percentage while doing comparison.
 * 10/29/2012   DR 15479   Qinglu Lin  Added code to call removeDuplicateCoordinate() 
 *                                     in redrawBoxFromHatched().
 * 11/02/2012   DR 15455   Qinglu Lin  Added setWarningAction(), called redrawBoxFromTrack() while
 *                                     warningAction is neither null nor WarningAction.NEW, removed 
 *                                     some code from redrawBoxFromHatched().                                    
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class WarngenLayer extends AbstractStormTrackResource {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(WarngenLayer.class);

    private static class GeospatialDataList {

        private static final String LOCAL_GEOM = "localGeometry";

        private static final String LOCAL_PREP_GEOM = "localPreparedGeometry";

        GeospatialData[] features;

        MathTransform latLonToLocal;

        MathTransform localToLatLon;

        IExtent localExtent;

        int nx, ny;
    }

    private static Map<String, GeospatialDataList> siteMap = new HashMap<String, GeospatialDataList>();

    private static Map<String, Geometry> timezoneMap = new HashMap<String, Geometry>();

    public static final String GID = "gid";

    private static final String AREA = "WARNGEN_AREA";

    public static final String MARINE = "MarineZones";

    /** The color white to be used for the resource */
    private static final RGB WHITE = new RGB(255, 255, 255);

    private static final byte[] fillPattern = new byte[128];

    /** The covered area to draw */
    private IWireframeShape coveredAreaFrame;

    /** The shaded area to draw */
    private IShadedShape shadedCoveredArea;

    private boolean hasDrawnShaded = false;

    private boolean shouldDrawShaded = true;

    private WarngenConfiguration configuration;

    private DialogConfiguration dialogConfig;

    private WarngenDialog dialog;

    private WarngenUIManager manager;

    private String templateName;

    private String backupSite;

    private boolean boxEditable = true;

    private Map<MapDescriptor, Set<String>> loadedCustomMaps;

    protected Mode lastMode = null;

    protected boolean redrawBoxFromTrack = false;

    protected WarngenUIState state = new WarngenUIState();

    private boolean modifiedVertexNeedsToBeUpdated = false;

    private static UnitConverter knotToMeterPerSec = NonSI.KNOT
            .getConverterTo(SI.METERS_PER_SECOND);

    private static UnitConverter meterSqToKmSq = SI.METRE.times(SI.METRE)
            .getConverterTo(SI.KILOMETRE.times(SI.KILOMETRE));

    private GeospatialDataList geoData = null;

    private WarningAction warningAction = null;

    static {
        for (int i = 0; i < 128; i++) {
            if (i % 32 == 0) {
                fillPattern[i] = 0x11;
            } else if (i % 32 == 1) {
                fillPattern[i] = 0x11;
            } else if (i % 32 == 2) {
                fillPattern[i] = 0x11;
            } else if (i % 32 == 3) {
                fillPattern[i] = 0x11;
            }
        }
    }

    public WarngenLayer(
            AwipsToolsResourceData<? extends AbstractStormTrackResource> resourceData,
            LoadProperties loadProperties, MapDescriptor descriptor) {
        super(resourceData, loadProperties, descriptor);
        displayState.displayType = DisplayType.POINT;
        getCapability(ColorableCapability.class).setColor(WHITE);
        loadedCustomMaps = new HashMap<MapDescriptor, Set<String>>();

        try {
            dialogConfig = DialogConfiguration
                    .loadDialogConfig(getLocalizedSite());
        } catch (Exception e) {
            statusHandler.handle(Priority.SIGNIFICANT,
                    "Error loading config.xml", e);
        }
        // Load default template
        String defaultTemplate = dialogConfig.getDefaultTemplate();
        if (defaultTemplate.equals("")) {
            statusHandler.handle(Priority.SIGNIFICANT,
                    "Default template is not defined in config.xml",
                    new WarngenException(
                            "Default Template is not defined in config.xml"));
        } else {
            setTemplateName(defaultTemplate);
        }

        setSpeedAndAngle();
        setDuration();
    }

    @Override
    protected String getResourceName() {
        return "Interactive Warngen";
    }

    private void setDuration() {
        if (getConfiguration() != null
                && getConfiguration().getDefaultDuration() != 0) {
            displayState.duration = getConfiguration().getDefaultDuration();
        } else {
            displayState.duration = 30;
        }
    }

    @Override
    protected void initializeState(StormTrackState state) {
        FramesInfo info = descriptor.getFramesInfo();
        // Setup the initial state for the storm track
        // Default angle for POINT
        displayState.labelMode = LabelMode.TIME;
        state.angle = 60;
        state.speed = 35;
        state.dragMePoint = null;
        state.resetAnchor = true;
        state.dragMeLine = null;
        // default for POLY, will calculate
        state.lineOfStormsLength = -1;
        state.mode = StormTrackState.Mode.DRAG_ME;
        state.numDragMePoints = 2;
        state.pivotIndex = trackUtil.getCurrentFrame(info);
        state.otherPivotIndex = displayState.pivotIndex > 0 ? 0 : trackUtil
                .getFrameCount(info) - 1;
        state.thingToDragTo = "storm";
        setSpeedAndAngle();
        setDuration();

        // Warngen specific state
        clearWarningGeometries();
        resetInitialFrame();
    }

    /**
     * Clear out old warning data
     */
    public void clearWarningGeometries() {
        if (state != null) {
            state.clear();
        }
    }

    public void setSpeedAndAngle() {
        if (configuration != null) {
            StormTrackData data = null;
            try {
                // TODO: Check to see if feasible to try STIData first, might be
                // too slow
                if (checkStormTrackData(data = ToolsDataManager.getInstance()
                        .getStormTrackData())) {
                    displayState.angle = adjustAngle(data.getMotionDirection());
                    displayState.speed = knotToMeterPerSec.convert(data
                            .getMotionSpeed());
                } else if (checkStormTrackData((data = RadarHelper
                        .getSTIData(SiteMap.getInstance()
                                .getSite4LetterId(getLocalizedSite())
                                .toLowerCase())))) {
                    displayState.angle = adjustAngle(data.getMotionDirection() - 180);
                    displayState.speed = knotToMeterPerSec.convert(data
                            .getMotionSpeed());
                } else {
                    displayState.angle = 60;
                    displayState.speed = 25;
                }
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Problem setting warngen speed and direction", e);
            }
        }
    }

    private boolean checkStormTrackData(StormTrackData data) {
        if (data == null) {
            return false;
        }
        Date now = SimulatedTime.getSystemTime().getTime();
        Date then = data.getDate();
        if (then == null
                || ((now.getTime() - then.getTime()) / (60 * 1000) > 10)) {
            return false;
        }

        if (Double.isNaN(data.getMotionDirection())
                || Double.isNaN(data.getMotionSpeed())
                || data.getMotionSpeed() == 0) {
            return false;
        }

        return true;
    }

    @Override
    protected void disposeInternal() {
        for (Entry<MapDescriptor, Set<String>> entry : loadedCustomMaps
                .entrySet()) {
            for (String map : entry.getValue()) {
                MapManager.getInstance(entry.getKey()).unloadMap(map);
            }
        }
        loadedCustomMaps.clear();

        super.disposeInternal();

        synchronized (WarngenLayer.class) {
            dialog.close();
            dialog = null;
        }

        if (shadedCoveredArea != null) {
            shadedCoveredArea.dispose();
        }
        if (coveredAreaFrame != null) {
            coveredAreaFrame.dispose();
        }

        manager.dispose();
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        WarngenLayer theOne = getLayerForContainer();
        if (theOne != null && theOne != this) {
            /**
             * For multipane containers, warngen should only have a single
             * resource shared by all panes, when deserializing you end up with
             * separate resources on the different panes. To solve this when we
             * initialize we find the first layer and if it is not this then
             * replace this with the other layer.
             */
            for (ResourcePair rp : descriptor.getResourceList()) {
                if (rp.getResource() == this) {
                    rp.setResource(theOne);
                    descriptor.getTimeMatcher().redoTimeMatching(theOne);
                    descriptor.getTimeMatcher().redoTimeMatching(descriptor);
                    return;
                }
            }
        }
        super.initInternal(target);
        VizApp.runSync(new Runnable() {

            @Override
            public void run() {
                createDialog();
            }
        });
        manager = new WarngenUIManager(this);
        coveredAreaFrame = target.createWireframeShape(true, this.descriptor);
        shadedCoveredArea = target.createShadedShape(true, this.descriptor,
                true);
    }

    /**
     * Find the warngen layer that should be used for this container. If a
     * container managed to get multiple warngen resources, possibly through
     * deserialization then this will return an initialized resource or the
     * first resource.
     * 
     * @return
     */
    private WarngenLayer getLayerForContainer() {
        WarngenLayer layer = null;
        IDisplayPaneContainer container = this.getResourceContainer();
        if (container != null) {
            for (IDisplayPane pane : container.getDisplayPanes()) {
                List<WarngenLayer> otherwarngens = pane.getDescriptor()
                        .getResourceList()
                        .getResourcesByTypeAsType(WarngenLayer.class);
                for (WarngenLayer other : otherwarngens) {
                    // grab the first layer, unless another layer exists and is
                    // already intialized
                    if (layer == null
                            || other.getStatus() == ResourceStatus.INITIALIZED) {
                        layer = other;
                    }
                }
            }
        }
        return layer;
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        if (displayState.lineOfStormsLength == -1) {
            displayState.lineOfStormsLength = 50000 / ((paintProps.getView()
                    .getExtent().getWidth() / paintProps.getCanvasBounds().width) * 10);
        }

        super.paintInternal(target, paintProps);

        int frameCount = trackUtil.getFrameCount(paintProps.getFramesInfo());

        // TODO: Issues with frameCount == 1? Could happen if we update on all
        // tilts where we had multiple frames then they went away
        if ((displayState.mode == Mode.TRACK && lastMode == Mode.DRAG_ME)
                || (frameCount == 1 && displayState.geomChanged)) {
            if (frameCount == 1 && displayState.geomChanged) {
                displayState.geomChanged = false;
            }
            if (warningAction == null || warningAction == WarningAction.NEW) {
            	// Initialize box
            	if (((configuration.isTrackEnabled() == false || configuration
            			.getPathcastConfig() == null) && this.displayState.displayType != DisplayType.POLY)
            			|| frameCount == 1) {
            		createSquare();
            		resetInitialFrame();
            	} else {
            		redrawBoxFromTrack();
            	}
            } else {
            	redrawBoxFromTrack();
            }
        }

        if (configuration.getEnableDamBreakThreat()
                && displayState.mode == Mode.NONE && lastMode == Mode.NONE) {
            resetInitialFrame();
        }

        if (displayState.mode == Mode.TRACK) {
            paintPolygon(target, paintProps, state.getWarningPolygon());
            paintText(target, paintProps);
            if (state.geometryChanged) {
                drawShadedPoly(state.getWarningArea());
                state.geometryChanged = false;
            }

            if (hasDrawnShaded && shouldDrawShaded) {
                target.drawShadedShape(shadedCoveredArea, 1.0f);
            }

            if (displayState.trackVisible == false
                    && trackUtil.getCurrentFrame(paintProps.getFramesInfo()) != displayState.intialFrame) {
                displayState.trackVisible = true;
            }
        }

        lastMode = displayState.mode;
    }

    public void resetInitialFrame() {
        FramesInfo info = descriptor.getFramesInfo();
        displayState.intialFrame = trackUtil.getCurrentFrame(info);
    }

    /**
     * @param target
     * @param paintProps
     * @param thePrimitivePolygon2
     */
    private void paintPolygon(IGraphicsTarget target,
            PaintProperties paintProps, Polygon thePrimitivePolygon)
            throws VizException {
        RGB color = getCapability(ColorableCapability.class).getColor();
        float LINE_WIDTH = getCapability(OutlineCapability.class)
                .getOutlineWidth();
        float zoomLevel = paintProps.getZoomLevel();
        if (LINE_WIDTH < 1.5f) {
            LINE_WIDTH = 1.5f;
        }
        if (thePrimitivePolygon != null) {
            Coordinate[] c = thePrimitivePolygon.getExteriorRing()
                    .getCoordinates();

            double[] in1 = new double[2];
            double[] in2 = new double[2];

            for (int i = 0; i < c.length - 1; i++) {
                in1[0] = c[i].x;
                in1[1] = c[i].y;
                in2[0] = c[i + 1].x;
                in2[1] = c[i + 1].y;

                double[] out1 = this.descriptor.worldToPixel(in1);
                double[] out2 = this.descriptor.worldToPixel(in2);
                target.drawLine(out1[0], out1[1], 0.0, out2[0], out2[1], 0.0,
                        color, LINE_WIDTH);

                double delta;

                if (!boxEditable) {
                    delta = 25 * zoomLevel;
                } else {
                    delta = 80 * zoomLevel;
                }
                // Build triangle control points

                double[] triTop = new double[] { out1[0], out1[1] - delta };
                double[] triLeft = new double[] { out1[0] - delta,
                        out1[1] + delta };
                double[] triRight = new double[] { out1[0] + delta,
                        out1[1] + delta };

                target.drawLine(triTop[0], triTop[1], 0.0, triLeft[0],
                        triLeft[1], 0.0, color, LINE_WIDTH);
                target.drawLine(triTop[0], triTop[1], 0.0, triRight[0],
                        triRight[1], 0.0, color, LINE_WIDTH);
                target.drawLine(triLeft[0], triLeft[1], 0.0, triRight[0],
                        triRight[1], 0.0, color, LINE_WIDTH);

            }

        }
    }

    /**
     * @param target
     * @param paintProps
     */
    private void paintText(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
        Double magnification = getCapability(MagnificationCapability.class)
                .getMagnification();
        RGB textColor = getCapability(ColorableCapability.class).getColor();

        double ratio = paintProps.getView().getExtent().getWidth()
                / paintProps.getCanvasBounds().width;
        double boundary = 40 * ratio;
        double minX = paintProps.getView().getExtent().getMinX() + boundary;
        double maxX = paintProps.getView().getExtent().getMaxX() - boundary;
        double minY = paintProps.getView().getExtent().getMinY() + boundary;
        double maxY = paintProps.getView().getExtent().getMaxY() - boundary;
        List<DrawableString> strings = new ArrayList<DrawableString>();
        if (state.strings != null && state.strings.size() > 0) {
            Iterator<Coordinate> coords = state.strings.keySet().iterator();
            double[] in = new double[3];
            while (coords.hasNext()) {
                Coordinate c = coords.next();
                String text = state.strings.get(c);
                in[0] = c.x;
                in[1] = c.y;
                in[2] = c.z;
                double[] out = this.descriptor.worldToPixel(in);
                if (out[0] > maxX) {
                    out[0] = maxX;
                } else if (out[0] < minX) {
                    out[0] = minX;
                }
                if (out[1] > maxY) {
                    out[1] = maxY;
                } else if (out[1] < minY) {
                    out[1] = minY;
                }
                DrawableString string = new DrawableString(text, textColor);
                string.magnification = magnification;
                string.setCoordinates(out[0], out[1]);
                strings.add(string);
            }
        }

        if (false) {
            // set to true for debug and drawing coordinate order
            Coordinate[] coords = state.getWarningPolygon().getCoordinates();
            for (int i = 0; i < coords.length - 1; ++i) {
                double[] out = descriptor.worldToPixel(new double[] {
                        coords[i].x, coords[i].y });
                DrawableString string = new DrawableString("" + i, textColor);
                string.setCoordinates(out[0], out[1]);
                strings.add(string);
            }
        }

        target.drawStrings(strings);
    }

    /**
     * @param templateName
     *            the templateName to set
     */
    public void setTemplateName(String templateName) {
        this.templateName = templateName;

        WarngenConfiguration config = null;
        try {
            config = WarngenConfiguration.loadConfig(templateName,
                    getLocalizedSite());
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error occurred loading template " + templateName, e);
        }
        if (config != null) {
            init(config);
            displayState.trackVisible = this.configuration.isTrackEnabled()
                    && this.configuration.getPathcastConfig() != null;
        }
    }

    public void setLastMode(Mode lastMode) {
        this.lastMode = lastMode;
    }

    /**
     * @return the templateName
     */
    public String getTemplateName() {
        return templateName;
    }

    /**
     * @param configuration2
     */
    private void init(WarngenConfiguration config) {
        long t0 = System.currentTimeMillis();

        String site = getLocalizedSite();
        Map<String, GeospatialMetadata> metadataMap = GeospatialFactory
                .getMetaDataMap(config);

        synchronized (siteMap) {
            for (String areaSource : metadataMap.keySet()) {
                String currKey = areaSource + "." + site;
                GeospatialMetadata gmd = metadataMap.get(areaSource);
                GeospatialDataList gData = siteMap.get(currKey);

                if (gData == null) {
                    try {
                        long tq0 = System.currentTimeMillis();
                        gData = new GeospatialDataList();
                        gData.features = GeospatialFactory.getGeoSpatialList(
                                getLocalizedSite(), gmd);

                        // set the CountyUserData
                        List<Geometry> geoms = new ArrayList<Geometry>(
                                gData.features.length);
                        for (GeospatialData gd : gData.features) {
                            geoms.add(gd.geometry);
                            CountyUserData cud = new CountyUserData(gd,
                                    String.valueOf(gd.attributes
                                            .get(WarngenLayer.GID)));
                            GeometryUtil.setUserData(gd.geometry, cud);
                        }

                        List<Geometry> locals = new ArrayList<Geometry>();

                        Coordinate c = new GeometryFactory()
                                .buildGeometry(geoms).getCentroid()
                                .getCoordinate();

                        gData.latLonToLocal = MapUtil
                                .getTransformFromLatLon(MapUtil
                                        .constructStereographic(
                                                MapUtil.AWIPS_EARTH_RADIUS,
                                                MapUtil.AWIPS_EARTH_RADIUS,
                                                c.y, c.x));
                        gData.localToLatLon = gData.latLonToLocal.inverse();
                        for (GeospatialData gd : gData.features) {
                            Geometry local = JTS.transform(gd.geometry,
                                    gData.latLonToLocal);
                            gd.attributes.put(AREA, local.getArea());
                            gd.attributes.put(GeospatialDataList.LOCAL_GEOM,
                                    local);
                            gd.attributes.put(
                                    GeospatialDataList.LOCAL_PREP_GEOM,
                                    PreparedGeometryFactory.prepare(local));
                            locals.add(local);
                        }

                        Envelope env = new GeometryFactory().buildGeometry(
                                locals).getEnvelopeInternal();
                        IExtent localExtent = new PixelExtent(env.getMinX(),
                                env.getMaxX(), env.getMinY(), env.getMaxY());

                        int nx = 600;
                        int ny = 600;
                        // Boolean to change the aspect ratio of the extent to
                        // match
                        boolean keepAspectRatio = true;

                        GridSpacing gridSpacing = dialogConfig.getGridSpacing();

                        if (gridSpacing != null && gridSpacing.getNx() != null
                                && gridSpacing != null) {
                            nx = gridSpacing.getNx();
                            ny = gridSpacing.getNy();
                            keepAspectRatio = gridSpacing.isKeepAspectRatio();
                        }

                        double xinc, yinc;
                        double width = localExtent.getWidth();
                        double height = localExtent.getHeight();
                        if (!keepAspectRatio) {
                            xinc = (width / nx);
                            yinc = (height / ny);
                        } else {
                            if (width > height) {
                                ny = (int) ((height * nx) / width);
                            } else if (height > width) {
                                nx = (int) ((width * ny) / height);
                            }

                            xinc = yinc = (width / nx);
                        }
                        gData.localExtent = new PixelExtent(
                                localExtent.getMinX() - xinc,
                                localExtent.getMaxX() + xinc,
                                localExtent.getMinY() - yinc,
                                localExtent.getMaxY() + yinc);
                        gData.nx = nx;
                        gData.ny = ny;

                        System.out.println("Time to lookup geospatial data "
                                + (System.currentTimeMillis() - tq0));
                        siteMap.put(currKey, gData);

                        GeospatialData[] timezones = GeospatialFactory
                                .getTimezones();
                        if (timezones != null) {
                            for (GeospatialData timezone : timezones) {
                                if (timezone.attributes.containsKey(gmd
                                        .getTimeZoneField())) {
                                    String oneLetterTimezone = String
                                            .valueOf(timezone.attributes
                                                    .get(gmd.getTimeZoneField()));
                                    if (timezoneMap
                                            .containsKey(oneLetterTimezone) == false) {
                                        timezoneMap.put(oneLetterTimezone,
                                                timezone.geometry);
                                    }
                                }
                            }
                        }

                    } catch (Exception e) {
                        statusHandler.handle(Priority.WARN,
                                "Error in initializing geometries.", e);
                    }
                }
            }

            String areaSource = config.getGeospatialConfig().getAreaSource();
            geoData = siteMap.get(areaSource + "." + site);
        }// end synchronize
        loadCustomMaps(descriptor, config);
        this.configuration = config;
        System.out.println("Time to init warngen config: "
                + (System.currentTimeMillis() - t0));
    }

    protected void loadCustomMaps(MapDescriptor descriptor,
            WarngenConfiguration config) {
        if (config == null || descriptor == null) {
            return;
        }
        long t1 = System.currentTimeMillis();
        MapManager mapManager = MapManager.getInstance(descriptor);
        List<String> maps = Arrays.asList(config.getMaps());
        Set<String> loadedCustomMaps = this.loadedCustomMaps.get(descriptor);
        if (loadedCustomMaps == null) {
            loadedCustomMaps = new HashSet<String>();
            this.loadedCustomMaps.put(descriptor, loadedCustomMaps);
        }
        Iterator<String> it = loadedCustomMaps.iterator();
        while (it.hasNext()) {
            String map = it.next();
            if (!maps.contains(map)) {
                it.remove();
                mapManager.unloadMap(map);
            }
        }
        for (String map : maps) {
            if (!loadedCustomMaps.contains(map)) {
                if (!mapManager.isMapLoaded(map)) {
                    mapManager.loadMapByName(map);
                    loadedCustomMaps.add(map);
                }
            }
        }
        System.out.println("Time to load custom maps: "
                + (System.currentTimeMillis() - t1));
    }

    public GeospatialData[] getGeodataFeatures(String key) {
        GeospatialDataList geoDataList = siteMap.get(key);
        if (geoDataList != null) {
            return geoDataList.features;
        }

        return new GeospatialData[0];
    }

    public Geometry getTimezoneGeom(String oneLetterTimezone) {
        return timezoneMap.get(oneLetterTimezone);
    }

    /**
     * Gets the polygon in lat/lon projection
     * 
     * @return
     */
    public Polygon getPolygon() {
        return state.getWarningPolygon();
    }

    public void setBackupSite(String site) {
        if (site.equalsIgnoreCase("none")) {
            backupSite = "";
        } else {
            backupSite = site;
        }
    }

    public String getLocalizedSite() {
        String site = "";
        if (backupSite == null || "".equals(backupSite)) {
            site = LocalizationManager.getInstance().getCurrentSite();
        } else {
            site = backupSite;
        }
        return site;
    }

    /**
     * Get the warning area in local projection
     * 
     * @return
     */
    public Geometry getWarningArea() {
        return state.getWarningArea();
    }

    public WarngenConfiguration getConfiguration() {
        return configuration;
    }

    public DialogConfiguration getDialogConfig() {
        return dialogConfig;
    }

    public void setBoxEditable(boolean boxEditable) {
        this.boxEditable = boxEditable;
    }

    public boolean isBoxEditable() {
        return boxEditable;
    }

    public void setOldWarningPolygon(AbstractWarningRecord record) {
        if (record != null) {
            state.setOldWarningPolygon((Polygon) record.getGeometry().clone());
            state.setOldWarningArea(getWarningAreaFromPolygon(
                    state.getOldWarningPolygon(), record));
        } else {
            state.setOldWarningArea(null);
            state.setOldWarningPolygon(null);
        }
    }

    public Geometry getWarningAreaForGids(List<String> gids,
            Geometry warningArea) {
        List<Geometry> areas = new ArrayList<Geometry>();
        List<Geometry> allAreas = new ArrayList<Geometry>();
        GeometryUtil.buildGeometryList(allAreas, warningArea);
        for (Geometry g : allAreas) {
            if (gids.contains(GeometryUtil.getPrefix(g.getUserData()))) {
                areas.add(g);
            }
        }
        return new GeometryFactory().createGeometryCollection(areas
                .toArray(new Geometry[areas.size()]));
    }

    /**
     * Get the warning area from the record/polygon
     * 
     * @param polygon
     *            polygon in lat/lon projection
     * @param record
     * @return the warning area in local projection
     */
    public Geometry getWarningAreaFromPolygon(Polygon polygon,
            AbstractWarningRecord record) {
        Map<String, String[]> countyMap = FipsUtil.parseCountyHeader(record
                .getCountyheader());
        try {
            return getArea(polygon, countyMap);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
        return null;
    }

    /**
     * Creates and sets a geometry based on county header of the watch
     * 
     * @param activeTableRecord
     */
    public void createGeometryForWatches(Polygon polygon,
            List<ActiveTableRecord> records) {
        // Build area once
        Geometry area = buildArea(polygon);
        for (ActiveTableRecord activeTableRecord : records) {
            Map<String, String[]> countyMap = FipsUtil
                    .parseCountyHeader(activeTableRecord.getCountyheader());
            // get area with precalculated area
            activeTableRecord.setGeometry(getArea(area, countyMap));
        }
    }

    /**
     * Build the geometry area that intersects the cwa filter for the polygon in
     * local projection space
     * 
     * @param polygon
     *            polygon to intersect with in lat/lon space
     * @return the warning area in screen projection
     */
    private Geometry buildArea(Polygon polygon) {
        polygon = latLonToLocal(polygon);
        Geometry area = null;
        if (polygon != null) {
            for (GeospatialData r : geoData.features) {
                PreparedGeometry prepGeom = (PreparedGeometry) r.attributes
                        .get(GeospatialDataList.LOCAL_PREP_GEOM);
                try {
                    Geometry intersection = GeometryUtil.intersection(polygon,
                            prepGeom);
                    if (intersection.isEmpty()) {
                        continue;
                    }
                    if (area == null) {
                        area = intersection;
                    } else {
                        area = GeometryUtil.union(area, intersection);
                    }
                } catch (Exception e) {
                    // TODO handle exception correctly!!!
                    e.printStackTrace();
                }
            }
        }
        return localToLatLon(area);
    }

    /**
     * Give the intersection area and polygon, build the area for the county map
     * 
     * @param area
     * @param polygon
     * @param countyMap
     * @return
     */
    private Geometry getArea(Geometry area, Map<String, String[]> countyMap) {
        if (area == null) {
            return null;
        }

        // Now remove counties not present in warning
        List<Geometry> geoms = new ArrayList<Geometry>();
        GeometryUtil.buildGeometryList(geoms, area);
        List<Geometry> newList = new ArrayList<Geometry>();
        for (Geometry geom : geoms) {
            CountyUserData data = (CountyUserData) geom.getUserData();

            String fips = null;
            String[] ids = null;
            if (configuration.getGeospatialConfig().getAreaSource()
                    .equalsIgnoreCase(MARINE)) {
                fips = String.valueOf(data.entry.attributes.get(configuration
                        .getHatchedAreaSource().getFipsField()));
                if (countyMap.containsKey(fips.substring(0, 2))) {
                    ids = countyMap.get(fips.substring(0, 2));
                    for (String id : ids) {
                        if (fips.endsWith(id)) {
                            newList.add(geom);
                            break;
                        }
                    }
                }
            } else {
                String stateAbbr = String.valueOf(data.entry.attributes
                        .get(configuration.getHatchedAreaSource()
                                .getAreaNotationField()));
                if (countyMap.containsKey(stateAbbr)) {
                    ids = countyMap.get(stateAbbr);
                    fips = String.valueOf(data.entry.attributes
                            .get(configuration.getHatchedAreaSource()
                                    .getFipsField()));
                    for (String id : ids) {
                        if (fips.endsWith(id)) {
                            newList.add(geom);
                            break;
                        }
                    }
                }
            }
        }
        return area.getFactory().createGeometryCollection(
                newList.toArray(new Geometry[newList.size()]));
    }

    /**
     * Build the area for the county map given the polygon to intersect with in
     * lat/lon space, returns the warning area in local projection
     * 
     * @param polygon
     * @param countyMap
     * @return
     */
    private Geometry getArea(Polygon polygon, Map<String, String[]> countyMap) {
        return getArea(buildArea(polygon), countyMap);
    }

    /**
     * Create the WarnGen dialog if it has not been created.
     */
    public void createDialog() {
        if (dialog == null || dialog.isDisposed() == true) {
            dialog = new WarngenDialog(PlatformUI.getWorkbench()
                    .getActiveWorkbenchWindow().getShell(), this);
            dialog.open();
            addDialogDisposeListener(descriptor);
        } else {
            showDialog(true);
        }
    }

    public void addDialogDisposeListener(final MapDescriptor descriptor) {
        if (dialog != null) {
            dialog.getShell().addDisposeListener(new DisposeListener() {
                @Override
                public void widgetDisposed(DisposeEvent e) {
                    descriptor.getResourceList().removeRsc(WarngenLayer.this);
                }
            });
        }
    }

    @Override
    public void setDescriptor(MapDescriptor descriptor) {
        super.setDescriptor(descriptor);
        addDialogDisposeListener(descriptor);
        loadCustomMaps(descriptor, configuration);
    }

    /**
     * Show the WarnGen dialog and move it to the front.
     */
    public void showDialog(boolean show) {
        if (dialog != null && dialog.isDisposed() == false) {
            dialog.showDialog(show);
        }
    }

    /**
     * 
     * @param snapHatchedAreaToPolygon
     *            If True, any hatched area outside the polygon will be
     *            eliminated.
     * @param determineInclusion
     *            If True, the area percent and/or area coverage will be used to
     *            determine if a county/zone should e included
     * @throws VizException
     */
    public void updateWarnedAreas(boolean snapHatchedAreaToPolygon,
            boolean determineInclusion) throws VizException {
        if (getPolygon() == null) {
            return;
        }
        state.snappedToArea = false;

        Geometry warningArea = latLonToLocal(state.getWarningArea());
        Geometry warningPolygon = latLonToLocal(state.getWarningPolygon());
        Geometry oldWarningArea = latLonToLocal(state.getOldWarningArea());
        Geometry oldWarningPolygon = latLonToLocal(state.getOldWarningPolygon());

        if (warningArea == null || snapHatchedAreaToPolygon) {
            warningArea = warningPolygon;
        }

        Geometry hatchedArea = warningArea;
        long t0 = System.currentTimeMillis();
        Geometry newHatchedArea = null;
        boolean insideCWA = false;
        // Loop through each of our counties returned from the query
        for (GeospatialData f : geoData.features) {
            // get the geometry of the county and make sure it intersects with
            // our hatched area
            PreparedGeometry prepGeom = (PreparedGeometry) f.attributes
                    .get(GeospatialDataList.LOCAL_PREP_GEOM);
            Geometry geom = (Geometry) f.attributes
                    .get(GeospatialDataList.LOCAL_GEOM);
            Geometry intersection = null;
            try {
                // Get intersection between county and hatched boundary
                intersection = GeometryUtil.intersection(hatchedArea, prepGeom);
                if (intersection.isEmpty()) {
                    continue;
                }
                insideCWA = true;
            } catch (RuntimeException e) {
                continue;
                // This is a workaround for JTS 1.7.1
            }

            try {
                double ratio = intersection.getArea() / geom.getArea();
                double ratioInPercent = ratio * 100.;
                Double areaOfGeom = (Double) f.attributes.get(AREA);
                double areaInKmSqOfIntersection = meterSqToKmSq
                        .convert(areaOfGeom * ratio);

                boolean includeArea = false;
                if (!determineInclusion) {
                    includeArea = areaInKmSqOfIntersection > 1;
                } else if (getConfiguration().getHatchedAreaSource()
                        .getInclusionAndOr().equalsIgnoreCase("AND")) {
                    if ((ratioInPercent >= getConfiguration()
                            .getHatchedAreaSource().getInclusionPercent())
                            && (areaInKmSqOfIntersection > getConfiguration()
                                    .getHatchedAreaSource().getInclusionArea())) {
                        includeArea = true;
                    }
                } else {
                    if ((ratioInPercent >= getConfiguration()
                            .getHatchedAreaSource().getInclusionPercent())
                            || (areaInKmSqOfIntersection > getConfiguration()
                                    .getHatchedAreaSource().getInclusionArea())) {
                        includeArea = true;
                    }
                }
                if (includeArea
                        && (oldWarningPolygon == null || prepGeom
                                .intersects(oldWarningPolygon))) {
                    if (newHatchedArea == null) {
                        newHatchedArea = intersection;
                    } else {
                        newHatchedArea = GeometryUtil.union(newHatchedArea,
                                intersection);
                    }
                }
            } catch (TopologyException e) {
                statusHandler.handle(Priority.VERBOSE,
                        "Geometry error simplifying hatched area.", e);
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Inclusion Area not properly configured.", e);
            }
        }
        // All area has been removed from the polygon...
        if (insideCWA == false) {
            state.strings.clear();
            state.setWarningArea(null);
            state.geometryChanged = true;
            dialog.getDisplay().asyncExec(new Runnable() {
                public void run() {
                    dialog.setInstructions();
                }
            });
            if (!state.isMarked() || oldWarningArea == null) {
                return;
            }
        }
        if (oldWarningArea != null) {
            int areaPercent = Double
                    .valueOf(
                            ((oldWarningPolygon.intersection(warningPolygon)
                                    .getArea() / oldWarningArea.getArea()) * 100))
                    .intValue();
            if (GeometryUtil.intersects(oldWarningPolygon, warningPolygon) == false
                    && !state.isMarked()) {
                // Snap back to polygon
                state.setWarningPolygon(localToLatLon((Polygon) oldWarningPolygon));
                newHatchedArea = (Geometry) oldWarningArea.clone();
            } else if (areaPercent < 10 && state.isMarked()) {
                // snap back to last valid user selected area
                state.setWarningPolygon((Polygon) state
                        .getMarkedWarningPolygon().clone());
                newHatchedArea = latLonToLocal((Geometry) state
                        .getMarkedWarningArea().clone());
                state.resetMarked();
            } else if (warningPolygon != null) {
                // want intersection of warningPolygon and oldWarningArea
                Geometry intersection = null;
                for (int n = 0; n < oldWarningArea.getNumGeometries(); ++n) {
                    Geometry oldArea = oldWarningArea.getGeometryN(n);
                    Geometry geom = GeometryUtil.intersection(warningPolygon,
                            oldArea);
                    if (geom.isEmpty() == false) {
                        if (intersection == null) {
                            intersection = geom;
                        } else {
                            intersection = GeometryUtil.union(intersection,
                                    geom);
                        }
                    }
                }
                newHatchedArea = intersection;
            }
        }
        System.out.println("determining hatchedArea took "
                + (System.currentTimeMillis() - t0));
        if (newHatchedArea == null) {
            boolean initialWarning = false;
            String[] followUps = this.getConfiguration().getFollowUps();
            if (followUps.length == 0)
                initialWarning = true;
            else
                for (String followup : followUps) {
                    if (followup.equals("NEW")) {
                        initialWarning = true;
                        break;
                    }
                }
            if (initialWarning)
                state.clear2(); // not to hatch polygon for initial warning
            else {
                // Snap back for follow-ups
                state.setWarningPolygon((Polygon) state
                        .getMarkedWarningPolygon().clone());
                newHatchedArea = latLonToLocal((Geometry) state
                        .getMarkedWarningArea().clone());
                state.resetMarked();
                updateWarnedAreas(snapHatchedAreaToPolygon, true);
            }
        } else {
            newHatchedArea = localToLatLon(newHatchedArea);
            state.setWarningArea(newHatchedArea);
            state.mark(newHatchedArea);
            // add "W" strings
            if (determineInclusion) {
                populateStrings();
            }
        }

        // Apply new hatched area
        state.geometryChanged = true;
        issueRefresh();

        VizApp.runAsync(new Runnable() {
            public void run() {
                if (dialog != null) {
                    dialog.setInstructions();
                }
            }
        });
    }

    /**
     * Warned area to shade in lat/lon space
     * 
     * @param poly
     * @throws VizException
     */
    public void drawShadedPoly(Geometry poly) throws VizException {
        shadedCoveredArea.reset();
        shadedCoveredArea.setFillPattern(fillPattern);
        if (poly != null) {
            JTSCompiler comp = new JTSCompiler(shadedCoveredArea, null,
                    descriptor);
            comp.handle((Geometry) state.getWarningArea().clone(),
                    getCapability(ColorableCapability.class).getColor());
            hasDrawnShaded = true;
        } else {
            hasDrawnShaded = false;
        }
    }

    public void createSquare() throws VizException {
        GeodeticCalculator gc = new GeodeticCalculator();
        GeometryFactory gf = new GeometryFactory();

        Point startPoint = displayState.dragMePoint;
        displayState.resetAnchor = true;

        gc.setStartingGeographicPoint(startPoint.getX(), startPoint.getY());
        gc.setDirection(45, 15 * 1500);
        Point2D p1 = gc.getDestinationGeographicPoint();
        gc.setDirection(135, 15 * 1500);
        Point2D p2 = gc.getDestinationGeographicPoint();
        gc.setDirection(-135, 15 * 1500);
        Point2D p3 = gc.getDestinationGeographicPoint();
        gc.setDirection(-45, 15 * 1500);
        Point2D p4 = gc.getDestinationGeographicPoint();

        Coordinate[] c = new Coordinate[5];
        c[0] = new Coordinate(p1.getX(), p1.getY());
        c[1] = new Coordinate(p2.getX(), p2.getY());
        c[2] = new Coordinate(p3.getX(), p3.getY());
        c[3] = new Coordinate(p4.getX(), p4.getY());
        c[4] = c[0];

        PolygonUtil.truncate(c, 2);

        LinearRing lr = gf.createLinearRing(c);
        state.setWarningPolygon(gf.createPolygon(lr, null));

        updateWarnedAreas(true, true);
    }

    public void redrawBoxFromTrack() throws VizException {

        if (displayState.mode == Mode.DRAG_ME) {
            return;
        }
        DestinationGeodeticCalculator gc = new DestinationGeodeticCalculator();
        GeometryFactory gf = new GeometryFactory();

        Coordinate start = null;
        Coordinate end = null;
        if (displayState.timePoints != null) {
            DataTime current = new DataTime(SimulatedTime.getSystemTime()
                    .getTime());
            StormTrackState.StormCoord lastStormCoord = displayState.timePoints[displayState.timePoints.length - 1];
            gc.setStartingGeographicPoint(lastStormCoord.coord.x,
                    lastStormCoord.coord.y);

            double angle = displayState.angle;
            double oppositeAngle = adjustAngle(angle + 180);
            double distance = displayState.speed
                    * trackUtil.timeBetweenDataTimes(lastStormCoord.time,
                            current);
            gc.setStartingGeographicPoint(lastStormCoord.coord.x,
                    lastStormCoord.coord.y);
            if (current.greaterThan(lastStormCoord.time)) {
                gc.setDirection(angle, distance);
            } else {
                /*
                 * Opposite angle is used if simulated time is manually changed
                 * to an older time
                 */
                gc.setDirection(oppositeAngle, distance);
            }
            Point2D point = gc.getDestinationGeographicPoint();
            start = new Coordinate(point.getX(), point.getY());

            distance = displayState.speed * displayState.duration * 60;
            gc.setStartingGeographicPoint(start.x, start.y);
            gc.setDirection(angle, distance);
            point = gc.getDestinationGeographicPoint();
            end = new Coordinate(point.getX(), point.getY());
        } else if (displayState.displayType.equals(DisplayType.POLY)) {
            // hydro line of storms with no track
            double angle = displayState.angle;
            double distance = 0;
            Point startPoint = displayState.dragMePoint;
            Point2D point = null;

            gc.setStartingGeographicPoint(startPoint.getX(), startPoint.getY());
            gc.setDirection(angle, distance);
            point = gc.getDestinationGeographicPoint();

            start = new Coordinate(point.getX(), point.getY());
            angle = angle < 0 ? angle - 90 : angle + 90;
            angle = angle > 180 ? angle - 360 : angle;
            gc.setStartingGeographicPoint(startPoint.getX(), startPoint.getY());
            gc.setDirection(angle, distance);
            point = gc.getDestinationGeographicPoint();

            end = new Coordinate(point.getX(), point.getY());
        }

        if (start == null || end == null) {
            return;
        }

        /*
         * LINE OF STORMS
         */
        if (displayState.displayType == DisplayType.POLY) {

            int factor = displayState.futurePoints != null ? 2 : 1;
            double shortDistance = 8000;
            gc.setStartingGeographicPoint(start.x, start.y);
            gc.setDestinationGeographicPoint(end.x, end.y);
            double farDistance = displayState.futurePoints != null ? gc
                    .getOrthodromicDistance() + shortDistance * 1.8
                    : shortDistance;
            double angle = displayState.angle;
            Point2D startPoint = null;
            ArrayList<Coordinate> linearRing = new ArrayList<Coordinate>();
            GeodeticCalculator gc2 = new GeodeticCalculator();

            Coordinate[] coords = getUIManager().figureLineFromPoint(
                    displayState.dragMeLine, gf.createPoint(start))
                    .getCoordinates();
            int size = coords.length;
            // Points ahead of track
            for (int i = 0; i < size; i++) {
                Coordinate coord = coords[i];
                gc.setStartingGeographicPoint(coord.x, coord.y);
                if (i == 0) {
                    gc2.setStartingGeographicPoint(coord.x, coord.y);
                    gc2.setDestinationGeographicPoint(coords[i + 1].x,
                            coords[i + 1].y);
                    gc.setDirection(adjustAngle(angle), farDistance);
                    gc.setStartingGeographicPoint(gc
                            .getDestinationGeographicPoint());
                    gc.setDirection(adjustAngle(gc2.getAzimuth() - 180),
                            shortDistance * factor);
                } else if (i == size - 1) {
                    gc2.setStartingGeographicPoint(coord.x, coord.y);
                    gc2.setDestinationGeographicPoint(coords[i - 1].x,
                            coords[i - 1].y);
                    gc.setDirection(adjustAngle(angle), farDistance);
                    gc.setStartingGeographicPoint(gc
                            .getDestinationGeographicPoint());
                    gc.setDirection(adjustAngle(gc2.getAzimuth() - 180),
                            shortDistance * factor);
                } else {
                    gc.setDirection(adjustAngle(angle), farDistance);
                }
                Point2D p1 = gc.getDestinationGeographicPoint();
                if (i == 0) {
                    startPoint = p1;
                }
                linearRing.add(new Coordinate(p1.getX(), p1.getY()));
            }

            // Points behind track
            for (int i = size - 1; i >= 0; i--) {
                Coordinate coord = coords[i];
                gc.setStartingGeographicPoint(coord.x, coord.y);
                if (i == 0) {
                    gc2.setStartingGeographicPoint(coord.x, coord.y);
                    gc2.setDestinationGeographicPoint(coords[i + 1].x,
                            coords[i + 1].y);
                    gc.setDirection(adjustAngle(angle - 180), shortDistance);
                    gc.setStartingGeographicPoint(gc
                            .getDestinationGeographicPoint());
                    gc.setDirection(adjustAngle(gc2.getAzimuth() - 180),
                            shortDistance);
                } else if (i == size - 1) {
                    gc2.setStartingGeographicPoint(coord.x, coord.y);
                    gc2.setDestinationGeographicPoint(coords[i - 1].x,
                            coords[i - 1].y);
                    gc.setDirection(adjustAngle(angle - 180), shortDistance);
                    gc.setStartingGeographicPoint(gc
                            .getDestinationGeographicPoint());
                    gc.setDirection(adjustAngle(gc2.getAzimuth() - 180),
                            shortDistance);
                } else {
                    gc.setDirection(adjustAngle(angle - 180), shortDistance);
                }
                Point2D p2 = gc.getDestinationGeographicPoint();
                linearRing.add(new Coordinate(p2.getX(), p2.getY()));
            }
            linearRing
                    .add(new Coordinate(startPoint.getX(), startPoint.getY()));

            Coordinate[] c = new Coordinate[linearRing.size()];
            c = linearRing.toArray(c);
            PolygonUtil.truncate(c, 2);
            LinearRing lr = gf.createLinearRing(c);
            state.setWarningPolygon(gf.createPolygon(lr, null));

            updateWarnedAreas(true, true);

            /*
             * NOT LINE OF STORMS
             */
        } else {
            double angle = displayState.angle;

            double dist1 = 8000;

            final int d1 = 10 * 1000;
            final int d2 = 15 * 1000;

            double dist2 = 11000;

            final double hyp1 = Math.sqrt(d1 * d1 + dist1 * dist1);
            final double hyp2 = Math.sqrt(d2 * d2 + dist2 * dist2);

            double ang1 = 90 + Math.toDegrees(Math.atan(dist1 / d1));
            double ang2 = Math.toDegrees(Math.atan(d2 / dist2));

            gc.setStartingGeographicPoint(start.x, start.y);
            gc.setDirection(adjustAngle(angle + Math.abs(ang1)), hyp1);
            Point2D p1 = gc.getDestinationGeographicPoint();
            gc.setDirection(adjustAngle(angle - Math.abs(ang1)), hyp1);
            Point2D p2 = gc.getDestinationGeographicPoint();

            gc.setStartingGeographicPoint(end.x, end.y);
            gc.setDirection(adjustAngle(angle - Math.abs(ang2)), hyp2);
            Point2D p3 = gc.getDestinationGeographicPoint();
            gc.setDirection(adjustAngle(angle + Math.abs(ang2)), hyp2);
            Point2D p4 = gc.getDestinationGeographicPoint();

            Coordinate[] c = new Coordinate[5];
            c[0] = new Coordinate(p1.getX(), p1.getY());
            c[1] = new Coordinate(p2.getX(), p2.getY());
            c[2] = new Coordinate(p3.getX(), p3.getY());
            c[3] = new Coordinate(p4.getX(), p4.getY());
            c[4] = c[0];

            PolygonUtil.truncate(c, 2);

            LinearRing lr = gf.createLinearRing(c);
            state.setWarningPolygon(gf.createPolygon(lr, null));

            updateWarnedAreas(true, true);
        }
        if (dialog.box.getSelection()) {
            displayState.editable = false;
        }
        dialog.setInstructions();
    }

    /**
     * 
     */
    public void redrawBoxFromHatched() throws VizException {
        if (state.snappedToArea == false) {
            if (state.getWarningArea() == null
                    || state.getWarningArea().isEmpty()) {
                return;
            }

            try {
                long t0 = System.currentTimeMillis();
                state.removeDuplicateCoordinate();
                Polygon hatched = new PolygonUtil(this, geoData.nx, geoData.ny,
                        20, geoData.localExtent, geoData.localToLatLon)
                        .hatchWarningArea(state.getWarningPolygon(),
                                state.getWarningArea());
                if (hatched != null) {
                    state.setWarningPolygon(hatched);
                    state.snappedToArea = true;
                }
                System.out.println("Time to createWarningPolygon: "
                        + (System.currentTimeMillis() - t0) + "ms");
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error hatching polygon", e);
            }
            issueRefresh();
        }
    }

    public void createDamThreatArea(Coordinate[] coordinates) {
        GeometryFactory gf = new GeometryFactory();
        LinearRing lr = gf.createLinearRing(coordinates);
        Coordinate pt = lr.getCentroid().getCoordinate();

        try {
            state.setWarningPolygon(gf.createPolygon(lr, null));
            state.rightClickSelected = false;
            updateWarnedAreas(true, true);
            displayState.dragMeGeom = gf.createPoint(pt);
            displayState.dragMePoint = gf.createPoint(pt);
            displayState.mode = Mode.TRACK;
            lastMode = Mode.TRACK;

        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, "WarnGen Error", e);
        }
    }

    /**
     * This method draws a polygon and storm track on the Warngen Layer. The
     * arguments involved are a geometry representing the LAT...LON line and the
     * individual elements of the TML line (if provided).
     * 
     * @param g
     * @param motdir
     * @param motspd
     * @param loc
     * @throws VizException
     */
    public void createPolygonFromRecord(AbstractWarningRecord record)
            throws VizException {
        Geometry g = (Geometry) record.getGeometry().clone();
        Integer motdir = record.getMotdir();
        if (motdir != null) {
            motdir -= 180;
            while (motdir < 0) {
                motdir += 360;
            }
        }
        Integer motspd = record.getMotspd();
        String loc = record.getLoc();
        Coordinate[] coords = g.getCoordinates();
        coords[coords.length - 1] = coords[0];
        GeometryFactory gf = new GeometryFactory();
        LinearRing lr = gf.createLinearRing(coords);

        Coordinate pt = lr.getCentroid().getCoordinate();
        Geometry parsed = null;
        if (loc != null) {
            try {
                parsed = new WKTReader().read(loc);
            } catch (ParseException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error parsing storm location", e);
            }
        }

        if (parsed != null) {
            Coordinate[] parsedCoords = parsed.getCoordinates();
            if (parsedCoords.length == 1) {
                displayState.displayType = DisplayType.POINT;
                dialog.oneStorm.setSelection(true);
                dialog.lineOfStorms.setSelection(false);
                displayState.dragMeGeom = displayState.dragMePoint = gf
                        .createPoint(parsedCoords[0]);
                displayState.dragMeLine = null;
            } else {
                displayState.displayType = DisplayType.POLY;
                dialog.oneStorm.setSelection(false);
                dialog.lineOfStorms.setSelection(true);
                displayState.dragMeGeom = displayState.dragMeLine = gf
                        .createLineString(parsed.getCoordinates());
                displayState.dragMePoint = StormTrackUIManager
                        .getPointFromLine(displayState.dragMeLine);
            }
        } else if (pt != null) {
            displayState.dragMeGeom = displayState.dragMePoint = gf
                    .createPoint(pt);
            displayState.dragMeLine = null;
        }

        Point point = displayState.dragMePoint;
        displayState.trackVisible = (motdir != null && motspd != null);
        if (displayState.trackVisible) {
            displayState.angle = adjustAngle(motdir);
            displayState.speed = knotToMeterPerSec.convert(motspd);

            DataTime recordFrameTime = recordFrameTime(record);
            DataTime currentFrameTime = descriptor.getTimeForResource(this);

            if (currentFrameTime == null) {
                FramesInfo framesInfo = getDescriptor().getFramesInfo();
                DataTime[] dt = framesInfo.getFrameTimes();
                currentFrameTime = dt[framesInfo.getFrameIndex()];
            }

            point = gf.createPoint(figurePoint(recordFrameTime,
                    currentFrameTime, displayState.speed, displayState.angle));
        }

        // Uses the new dragMePoint when creating a track
        displayState.mode = Mode.TRACK;
        displayState.originalTrack = true;
        displayState.geomChanged = true;
        displayState.resetAnchor = true;
        displayState.timePoints = null;

        if (displayState.dragMePoint != null) {
            displayState.dragMeGeom = displayState.dragMePoint = point;
        } else {
            displayState.dragMeGeom = displayState.dragMeLine = getUIManager()
                    .figureLineFromPoint(displayState.dragMeLine, point);
        }

        Polygon warnPolygon = gf.createPolygon(lr, null);
        state.setWarningPolygon(warnPolygon);
        state.setWarningArea(getWarningAreaFromPolygon(
                state.getWarningPolygon(), record));
        updateWarnedAreas(true, true);
    }

    private DataTime recordFrameTime(AbstractWarningRecord warnRecord) {
        Calendar frameTime;
        String rawMessage = warnRecord.getRawmessage();
        Pattern tmlPtrn = Pattern
                .compile("TIME...MOT...LOC (\\d{1,2})(\\d{2})Z");
        Matcher m = tmlPtrn.matcher(rawMessage);

        if (m.find()) {
            int day = warnRecord.getIssueTime().get(Calendar.DAY_OF_MONTH);
            int hour = Integer.parseInt(m.group(1));
            int minute = Integer.parseInt(m.group(2));
            frameTime = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
            frameTime.set(Calendar.DAY_OF_MONTH, day);
            frameTime.set(Calendar.HOUR_OF_DAY, hour);
            frameTime.set(Calendar.MINUTE, minute);
        } else {
            frameTime = warnRecord.getIssueTime();
        }
        return new DataTime(frameTime);
    }

    /**
     * Determines what the current coordinate for the drag me dot should be if
     * the frame is not on the same frame time the warning was originally issued
     * 
     * @param frameTime
     * @param currentFrameTime
     * @param speed
     * @param motdir
     * @return
     */
    private Coordinate figurePoint(DataTime frameTime,
            DataTime currentFrameTime, double speed, double motdir) {
        Coordinate recordDragMePoint = new Coordinate(
                displayState.dragMePoint.getCoordinate());
        recordDragMePoint = GisUtil.restoreAlaskaLon(recordDragMePoint);

        long currentInMillis = currentFrameTime.getRefTimeAsCalendar()
                .getTimeInMillis();
        long frameInMillis = frameTime.getRefTimeAsCalendar().getTimeInMillis();
        double timeDiffInS = Math.abs(currentInMillis - frameInMillis) / 1000.0;

        double searchAngle = adjustAngle(motdir);
        if (timeDiffInS < 60) {
            // equivalent times
            return recordDragMePoint;
        } else if (frameTime.greaterThan(currentFrameTime)) {
            searchAngle = adjustAngle(searchAngle - 180);
        }

        DestinationGeodeticCalculator gc = new DestinationGeodeticCalculator();
        gc.setStartingGeographicPoint(recordDragMePoint.x, recordDragMePoint.y);
        gc.setDirection(searchAngle, displayState.speed * timeDiffInS);
        Point2D p = gc.getDestinationGeographicPoint();

        return new Coordinate(p.getX(), p.getY());
    }

    public Coordinate updateCoordinate(Coordinate coordinate, boolean current) {
        FramesInfo info = descriptor.getFramesInfo();
        int currentFrame = trackUtil.getCurrentFrame(info);
        int frameCount = trackUtil.getFrameCount(info);
        if (currentFrame == frameCount - 1
                || displayState.trackVisible == false) {
            return coordinate;
        }
        DataTime[] datatimes = trackUtil.getDataTimes(info);
        double distance = displayState.speed
                * trackUtil.timeBetweenDataTimes(datatimes[currentFrame],
                        datatimes[frameCount - 1]);
        double angle = current ? adjustAngle(displayState.angle + 180)
                : displayState.angle;

        DestinationGeodeticCalculator gc = new DestinationGeodeticCalculator();
        gc.setStartingGeographicPoint(coordinate.x, coordinate.y);
        gc.setDirection(angle, distance);
        Point2D point = gc.getDestinationGeographicPoint();

        return new Coordinate(point.getX(), point.getY());
    }

    public Coordinate[] getStormLocations(StormTrackState stormTrackState) {
        Coordinate[] cc = null;
        switch (stormTrackState.displayType) {
        case POINT:
            cc = new Coordinate[] { stormTrackState.futurePoints == null ? stormTrackState.dragMePoint
                    .getCoordinate() : stormTrackState.futurePoints[0].coord };
            break;
        case POLY:
            Coordinate[] polyPoints = stormTrackState.dragMeLine
                    .getCoordinates();
            cc = new Coordinate[polyPoints.length];
            for (int i = 0; i < cc.length; i++) {
                cc[i] = updateCoordinate((Coordinate) polyPoints[i].clone(),
                        false);
            }
        }
        return cc;
    }

    /**
     * @param selection
     */
    public void setShouldDrawShaded(boolean selection) {
        shouldDrawShaded = selection;
    }

    public void refreshTemplateForFollowUp(FollowupData data) {
        if (configuration.getBulletActionGroups() != null) {
            for (BulletActionGroup group : configuration
                    .getBulletActionGroups()) {
                if (data.getAct().equals(group.getAction())
                        && data.getPhen().equals(group.getPhen())
                        && data.getSig().equals(group.getSig())) {
                    configuration.setBullets(group.getBullets());
                    configuration.setDamInfoBullets(group.getDamInfoBullets());
                    break;
                }
            }
        }
        init(configuration);
    }

    public WarngenUIState getWarngenState() {
        return this.state;
    }

    public boolean isPolygonLocked() {
        return !boxEditable;
    }

    /**
     * Translate a single vertex point
     * 
     * @param vertexId
     * @param delta
     */
    public int translatePolygonVertex(int vertexId, Coordinate newCoord,
            boolean deleteSameVertex) {
        Polygon warningPolygon = state.getWarningPolygon();
        IDisplayPaneContainer container = getResourceContainer();
        Coordinate[] coords = warningPolygon.getExteriorRing().getCoordinates();

        if (vertexId >= coords.length || vertexId < 0) {
            return vertexId;
        }
        int rval = vertexId;
        Object newArray = Array.newInstance(coords.getClass()
                .getComponentType(), coords.length);
        System.arraycopy(coords, 0, newArray, 0, coords.length);
        coords = (Coordinate[]) newArray;
        coords[coords.length - 1] = coords[0];

        int delete = -1;
        double[] v = container.translateInverseClick(coords[vertexId]);
        Coordinate vc = new Coordinate(v[0], v[1]);
        for (int i = 0; i < coords.length; i++) {
            if (i == vertexId) {
                continue;
            }
            double[] a = container.translateInverseClick(coords[i]);
            double distance = new Coordinate(a[0], a[1]).distance(vc);
            if (distance < 5
                    && !((i == coords.length - 1 && vertexId == 0) || (i == 0 && vertexId == coords.length - 1))) {
                delete = i;
            }
        }

        coords[vertexId] = newCoord;
        if (vertexId == 0) {
            coords[coords.length - 1] = newCoord;
        } else if (vertexId == coords.length - 1) {
            coords[0] = newCoord;
        }
        if (deleteSameVertex && delete != -1 && coords.length > 4) {
            setModifiedVertexNeedsToBeUpdated(true);
            Coordinate[] newCoords = new Coordinate[coords.length - 1];
            int ctr = 0;
            if (delete == 0 || delete == coords.length - 1) {
                for (int i = 1; i < coords.length - 1; i++) {
                    newCoords[ctr++] = coords[i];
                }
                newCoords[newCoords.length - 1] = newCoords[0];
            } else {
                for (int i = 0; i < coords.length - 1; i++) {
                    if (i != delete) {
                        newCoords[ctr++] = coords[i];
                    }
                }
                newCoords[newCoords.length - 1] = newCoords[0];
            }
            coords = newCoords;
            for (int i = 0; i < coords.length; ++i) {
                if (coords[i] == newCoord) {
                    rval = i;
                    break;
                }
            }
        }

        try {
            GeometryFactory gf = new GeometryFactory(
                    warningPolygon.getPrecisionModel());

            LinearRing ring = gf.createLinearRing(coords);

            LineSegment[] ls = new LineSegment[coords.length - 1];

            for (int i = 0; i < coords.length - 1; i++) {
                ls[i] = new LineSegment(coords[i], coords[i + 1]);
            }
            boolean intersectFlag = false;
            for (int i = 0; i < ls.length; i++) {
                for (int j = 0; j < ls.length; j++) {
                    if (i != j
                            && ls[i].intersection(ls[j]) != null
                            && ls[i].intersection(ls[j]).equals(
                                    ls[i].getCoordinate(0)) == false
                            && ls[i].intersection(ls[j]).equals(
                                    ls[i].getCoordinate(1)) == false) {
                        intersectFlag = true;
                    }
                }
            }

            if (!intersectFlag) {
                state.setWarningPolygon(gf.createPolygon(ring, null));
            }
        } catch (Exception e) {

        }

        return rval;
    }

    public boolean conWarnAreaChanged(FollowupData data) {
        CurrentWarnings cw = CurrentWarnings.getInstance(getLocalizedSite());
        AbstractWarningRecord warnRec = cw.getNewestByTracking(data.getEtn(),
                data.getPhen() + "." + data.getSig());

        if (warnRec == null
                || GisUtil.equivalent(warnRec.getGeometry(),
                        state.getWarningPolygon()) == false) {
            return true;
        }

        Coordinate[] coords = warnRec.getGeometry().getCoordinates();
        coords[coords.length - 1] = coords[0];
        GeometryFactory gf = new GeometryFactory();
        Geometry warnArea = getWarningAreaFromPolygon(
                gf.createPolygon(gf.createLinearRing(coords), null), warnRec);

        Set<String> warnAreaPrefixes = new HashSet<String>(
                Arrays.asList(GeometryUtil.getGID(state.getWarningArea())));
        Set<String> recordPrefixes = new HashSet<String>(
                Arrays.asList(GeometryUtil.getGID(warnArea)));

        if (warnAreaPrefixes.equals(recordPrefixes) == false) {
            return true;
        }

        return false;
    }

    public boolean isModifiedVertexNeedsToBeUpdated() {
        return modifiedVertexNeedsToBeUpdated;
    }

    public void setModifiedVertexNeedsToBeUpdated(
            boolean modifiedVertexNeedsToBeUpdated) {
        this.modifiedVertexNeedsToBeUpdated = modifiedVertexNeedsToBeUpdated;
    }

    /**
     * Translate (move) a polygon
     * 
     * @param delta
     */
    public void translatePolygon(Coordinate delta) {
        Coordinate[] coords = state.getWarningPolygon().getCoordinates();
        if (!coords[0].equals(coords[coords.length - 1])) {
            int length = Array.getLength(coords);
            int newLength = length + 1;
            Object newArray = Array.newInstance(coords.getClass()
                    .getComponentType(), newLength);
            System.arraycopy(coords, 0, newArray, 0, length);
            coords = (Coordinate[]) newArray;
            coords[coords.length - 1] = coords[0];
        }
        for (int i = 0; i < coords.length - 1; i++) {
            coords[i].x += delta.x;
            coords[i].y += delta.y;
            coords[i].z += delta.z;
        }
        coords[coords.length - 1] = coords[0];

        GeometryFactory gf = new GeometryFactory();
        LinearRing ring = gf.createLinearRing(coords);

        state.setWarningPolygon(gf.createPolygon(ring, null));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.cmenu.IRightClickCapableResource#addContextMenuItems
     * (org.eclipse.jface.action.IMenuManager)
     */
    @Override
    public void addContextMenuItems(IMenuManager menuManager, int x, int y) {
        super.addContextMenuItems(menuManager, x, y);
        menuManager.add(manager.getSelectLocationAction());

        if (displayState.mode == Mode.DRAG_ME || !boxEditable) {
            return;
        }

        if (manager.closeToPoint()) {
            menuManager.add(manager.getDeleteAction());
            menuManager.add(manager.getMoveAction());
        } else if (manager.closeToLine()) {
            menuManager.add(manager.getMoveElementAction());
            menuManager.add(manager.getAddAction());
        }

    }

    @Override
    protected String getAddVertexText() {
        return "Add Vertex";
    }

    @Override
    protected String getDeleteVertexText() {
        return "Remove Vertex";
    }

    /**
     * 
     * @param coord
     */
    public void addOrRemoveCounty(Coordinate coord) {
        try {
            Geometry oldWarningArea = state.getOldWarningArea();
            Polygon oldWarningPolygon = state.getOldWarningPolygon();
            Polygon warningPolygon = state.getWarningPolygon();

            GeometryFactory gf = new GeometryFactory();
            Point point = gf.createPoint(coord);
            // potentially adding or removing a county, figure out county
            for (GeospatialData f : geoData.features) {
                Geometry geom = f.geometry;
                if (f.prepGeom.contains(point)) {
                    if (GeometryUtil.contains(state.getWarningArea(), point)) {
                        // remove county
                        Geometry tmp = GeometryUtil.difference(
                                state.getWarningArea(), geom);
                        if (tmp.isEmpty()) {
                            break;
                        }

                        state.setWarningArea(tmp);
                    } else {
                        if (oldWarningArea != null) {
                            // for a CON, prevents extra areas to be added
                            Set<String> limit = new HashSet<String>();
                            limit.addAll(Arrays.asList(GeometryUtil
                                    .getGID(oldWarningArea)));
                            String prefix = GeometryUtil.getPrefix(geom
                                    .getUserData());

                            if (limit.contains(prefix) == false) {
                                break;
                            } else if (oldWarningPolygon.contains(point) == true) {
                                // only add in intersecting area from old
                                // warning
                                geom = GeometryUtil.intersection(
                                        oldWarningArea, geom);
                                if (warningPolygon.contains(point)) {
                                    geom = GeometryUtil.intersection(
                                            warningPolygon, geom);
                                }
                                state.setWarningArea(GeometryUtil.union(
                                        state.getWarningArea(), geom));
                            }
                        } else {
                            // add county
                            if (warningPolygon.contains(point)) {
                                // add part of county
                                geom = GeometryUtil.intersection(
                                        warningPolygon, geom);
                            }
                            state.setWarningArea(GeometryUtil.union(
                                    state.getWarningArea(), geom));
                        }
                    }
                    state.snappedToArea = false;
                    populateStrings();
                    issueRefresh();
                    break;
                }
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, "Error toggling county: "
                    + e.getLocalizedMessage(), e);
        }
    }

    /**
     * Populate the W strings with the included counties
     */
    private void populateStrings() {
        state.strings.clear();
        Set<String> prefixes = new HashSet<String>(Arrays.asList(GeometryUtil
                .getGID(state.getWarningArea())));

        for (GeospatialData f : geoData.features) {
            Geometry geom = f.geometry;
            if (prefixes.contains(GeometryUtil.getPrefix(geom.getUserData()))) {
                Coordinate center = GisUtil.d2dCoordinate(geom.getCentroid()
                        .getCoordinate());
                state.strings.put(center, "W");
            }
        }
    }

    public boolean featureProduct(Coordinate c) {
        if (dialog.updateListCbo.getItemCount() > 2) {
            FollowupData f = null;
            AbstractWarningRecord featuredWarning = null;
            if (state.followupData != null) {
                f = state.followupData;
                featuredWarning = CurrentWarnings.getInstance(
                        getLocalizedSite()).getNewestByTracking(f.getEtn(),
                        f.getPhen() + "." + f.getSig());
            }

            FollowupData prev = null;
            double minDistance = 61000;
            String closestItem = null;

            for (String item : dialog.updateListCbo.getItems()) {
                FollowupData s = (FollowupData) dialog.updateListCbo
                        .getData(item);
                if (s == null) {
                    // invalid follow up selection
                    continue;
                } else if (f != null && f.equals(s)) {
                    // follow up already featured
                    continue;
                } else if (prev != null && s.equals(prev)) {
                    // geometry for this warning has already been tested
                    continue;
                }

                AbstractWarningRecord w = CurrentWarnings.getInstance(
                        getLocalizedSite()).getNewestByTracking(s.getEtn(),
                        s.getPhen() + "." + s.getSig());
                GeometryFactory gf = new GeometryFactory();
                if (featuredWarning != null
                        && featuredWarning.getGeometry().contains(
                                gf.createPoint(c))) {
                    if (isPolygonLocked()) {
                        // if a product is already featured,
                        // clicking only outside the polygon is allowed
                        continue;
                    } else {
                        // this is so that add or remove county is not skipped
                        // this is if the follow up is a CON
                        return false;
                    }
                }

                if (w == null || w.getGeometry() == null) {
                    continue;
                }

                // check if mouse click is near the polygon
                Coordinate[] coords = w.getGeometry().getCoordinates();
                for (int i = 0; i < coords.length - 1; i++) {
                    LineSegment segment = new LineSegment(coords[i],
                            coords[i + 1]);
                    double dist = segment.distance(c);

                    if (dist < minDistance) {
                        // searching for the closest item to feature
                        minDistance = dist;
                        closestItem = item;
                    }
                }

                prev = s;
            }
            if (closestItem != null) {
                dialog.updateListCbo.select(dialog.updateListCbo
                        .indexOf(closestItem));
                // method takes about 2 seconds
                dialog.updateListSelected();
                issueRefresh();
                return true;
            }
        }

        return false;
    }

    @Override
    public void propertiesChanged(ResourceProperties props) {
        super.propertiesChanged(props);
        if (manager != null) {
            manager.setHandleInput(props.isVisible());
        }
    }

    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        displayState.geomChanged = true;
        drawShadedPoly(state.getWarningArea());
        issueRefresh();
    }

    @Override
    public void resourceChanged(ChangeType type, Object object) {
        super.resourceChanged(type, object);
        if (type == ChangeType.CAPABILITY
                && object instanceof EditableCapability) {
            if (dialog != null) {
                final boolean editable = isEditable();
                boxEditable = editable;
                displayState.editable = editable;
                if (editable) {
                    boxEditable = dialog.boxEditable();
                    displayState.editable = dialog.trackEditable();
                }
                final WarngenDialog dlg = dialog;
                dialog.getDisplay().asyncExec(new Runnable() {
                    public void run() {
                        if (dlg.isDisposed() == false
                                && dlg.getShell().isVisible() != editable) {
                            dlg.showDialog(editable);
                        }
                    }
                });
            }
            issueRefresh();
        }
    }

    /**
     * Converts the lat lon geometry to screen space
     * 
     * @param geom
     * @return
     */
    public <T> T latLonToLocal(T geom) {
        return convertGeom(geom, geoData.latLonToLocal);
    }

    /**
     * Converts the screen geometry to a lat lon projection
     * 
     * @param geom
     * @return
     */
    public <T> T localToLatLon(T geom) {
        return convertGeom(geom, geoData.localToLatLon);
    }

    /**
     * Converts the local geometry to a lat lon projection
     * 
     * @param <T>
     * @param geom
     * @return
     */
    @SuppressWarnings("unchecked")
    public <T> T convertGeom(T geom, MathTransform transform) {
        if (geom == null) {
            return null;
        }
        try {
            if (geom instanceof Coordinate) {
                return (T) JTS.transform(
                        new GeometryFactory().createPoint((Coordinate) geom),
                        transform).getCoordinate();
            } else if (geom instanceof Geometry) {
                return (T) JTS.transform((Geometry) geom, transform);
            } else {
                throw new RuntimeException("Invalid type passed in: "
                        + geom.getClass());
            }
        } catch (Exception e) {
            throw new RuntimeException("Error transforming object, "
                    + e.getLocalizedMessage(), e);
        }
    }
    
    public void setWarningAction(WarningAction warningAction) {
    	this.warningAction = warningAction;
    }
}
