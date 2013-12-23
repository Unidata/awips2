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
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.ui.PlatformUI;
import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.geometry.GeneralEnvelope;
import org.geotools.geometry.jts.JTS;
import org.geotools.referencing.GeodeticCalculator;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.dataplugin.warning.AbstractWarningRecord;
import com.raytheon.uf.common.dataplugin.warning.WarningRecord.WarningAction;
import com.raytheon.uf.common.dataplugin.warning.config.AreaSourceConfiguration;
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
import com.raytheon.uf.common.time.util.TimeUtil;
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
import com.raytheon.uf.viz.core.rsc.tools.GenericToolsResourceData;
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
import com.vividsolutions.jts.geom.GeometryCollection;
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
 * 11/15/2012   DR 15430   D. Friedman Use correct county/zone in createGeometryForWatches.
 * 11/29/2012   DR 15571   Qinglu Lin  Called compuateCurrentStormCenter() in getStormLocations();
 *                                     For CON, CAN, and COR, calculate Coordinate array, cc, specifically in 
 *                                     getStormLocations().
 * 12/10/2012   DR 15571   Qinglu Lin  Change warningAction's initialization from null to WarningAction.NEW, and add code
 *                                     in getStormLocations() for handling case when warningAction equals WarningAction.NEW;
 * 12/13/2012   DR 15559   Qinglu Lin  Added code to call WarngenUIState's adjustPolygon().
 * 12/17/2012   DR 15571   Qinglu Lin  For hydro products,futurePoints is null. Resolved an issue caused by trying to get 
 *                                     Coordinate[] from futurePoints.
 * 12/18/2012   DR 15571   Qinglu Lin  Resolved coordinate issue in TML line caused by clicking Restart button.
 * 01/24/2013   DR 15723   Qinglu Lin  Added initRemovedGids() and updated updateWarnedAreas() to prevent the removed 
 *                                     counties from being re-hatched.
 * 03/06/2013   DR 15831   D. Friedman Use area inclusion filter in followups.
 * 03/28/2013   DR 15973   Qinglu Lin  Added adjustVertex() and applied it invalid polygon.
 * 03/28/2013   DR 15974   D. Friedman Preserve the set of selected counties when recreating the polygon from the
 *                                     hatched area and remember marked counties outside the polygon on followup.
 * 04/03/2013   1858       jsanchez    Handled creating follow up warnings when created before 0z but issued after 0z.
 * 04/03/2013   DR 15942   Qinglu Lin  Added code to prevent small area from being toggled on that
 *                                     does not meet inclusionPercent/inclusionArea criteria.
 * 04/10/2013   DR 16044   D. Friedman Fix NPE in getAllFipsInArea.
 * 04/11/2013   1894       jsanchez    Kept tracked of the currently loaded custom maps.
 * 04/12/1013   DR 16045   Qinglu Lin  Updated AreaHatcher's run() by calling removeDuplicateCoordinate().
 * 04/23/1013   DR 16064   Qinglu Lin  Added removeDuplicateGid() and applies it in populateStrings().
 * 04/24/2013   1943       jsanchez    Replaced used of areaConfig with areaSource.
 * 05/16/2013   2008       jsanchez    Allowed warned areas for follow ups to be resized to less than 10%
 * 05/23/2013  DR 16169    D. Friedman Improve redraw-from-hatched-area polygons.
 * 05/31/2013  DR 16237    D. Friedman Refactor goespatial data routines and watch handling.
 * 06/05/2013  DR 16279    D. Friedman Fix determination of frame time from parsed storm track.
 * 06/17/1013  DR 15787    Qinglu Lin  Called removeTriplyOverlaidLinesegments().
 * 06/24/2013  DR 16317    D. Friedman Handle "motionless" track.
 * 06/25/2013  DR 16013    Qinglu Lin  Added setUniqueFip() and code for re-hatching polygon.
 * 07/09/2013  DR 16376    Qinglu Lin  Removed calling removeOverTriplylaidLinesegment() but called removeOverlaidLinesegment().
 * 07/26/2013  DR 16376    Qinglu Lin  Moved adjustVertex() and computeSlope() to PolygonUtil; removed calculateDistance();
 *                                     updated AreaHatcher's run().
 * 07/26/2013  DR 16450    D. Friedman Fix logic errors when frame count is one.
 * 08/19/2013   2177       jsanchez    Set a GeneralGridGeometry object in the GeospatialDataList.
 * 09/17/2013  DR 16496    D. Friedman Make editable state more consistent.
 * 10/01/2013  DR 16632    Qinglu Lin  Catch exceptions thrown while doing areaPercent computation and union().
 * 10/15/2013   2463       jsanchez    Create a square polygon when time matched with a resource with no data.
 * 10/21/2013  DR 16632    D. Friedman Modify areaPercent exception handling.  Fix an NPE.
 *                                     Use A1 hatching behavior when no county passes the inclusion filter.
 * 10/29/2013  DR 16734    D. Friedman If redraw-from-hatched-area fails, don't allow the pollygon the be used.
 * 12/17/2013  DR 16567    Qinglu Lin  Added findLargestGeometry() and findLargestQuadrant(), and updated
 *                                     populateStrings() and paintText(). TEST gerrit!!!
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class WarngenLayer extends AbstractStormTrackResource {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(WarngenLayer.class);

    String uniqueFip = null;

    private static class GeospatialDataList {

        private static final String LOCAL_GEOM = "localGeometry";

        private static final String LOCAL_PREP_GEOM = "localPreparedGeometry";

        GeospatialData[] features;

        MathTransform latLonToLocal;

        MathTransform localToLatLon;

        IExtent localExtent;

        int nx, ny;

        GeneralGridGeometry localGridGeometry;
    }

    private static class GeospatialDataAccessor {
        GeospatialDataList geoData;

        AreaSourceConfiguration areaConfig;

        public GeospatialDataAccessor(GeospatialDataList geoData,
                AreaSourceConfiguration areaConfig) {
            if (geoData == null || areaConfig == null) {
                throw new IllegalArgumentException(
                        "GeospatialDataAccessor must not be null");
            }
            this.geoData = geoData;
            this.areaConfig = areaConfig;
        }

        /**
         * Build the geometry area that intersects the cwa filter for the
         * polygon in local projection space
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
                        Geometry intersection = GeometryUtil.intersection(
                                polygon, prepGeom);
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

        private String getFips(GeospatialData data) {
            return (String) data.attributes.get(areaConfig.getFipsField());
        }

        private String getFips(Geometry g) {
            Object o = g.getUserData();
            if (o != null) {
                return getFips(((CountyUserData) o).entry);
            } else {
                for (int n = 0; n < g.getNumGeometries(); ++n) {
                    Geometry g2 = g.getGeometryN(n);
                    if (g != g2) {
                        String fips = getFips(g2);
                        if (fips != null)
                            return fips;
                    }
                }
            }
            return null;
        }

        private Set<String> getAllFipsInArea(Geometry warningArea) {
            Set<String> fipsIds = new HashSet<String>();
            for (int n = 0; n < warningArea.getNumGeometries(); ++n) {
                Geometry area = warningArea.getGeometryN(n);
                fipsIds.add(getFips(area));
            }
            return fipsIds;
        }

    }

    private class CustomMaps extends Job {

        private Set<String> customMaps = new HashSet<String>();

        private Set<String> mapsToLoad;

        private final MapManager manager;

        public CustomMaps() {
            super("Loading WarnGen Maps");
            manager = MapManager.getInstance(descriptor);
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            boolean done = false;
            while (!done) {
                Set<String> toLoad = null;
                synchronized (this) {
                    if (mapsToLoad != null) {
                        toLoad = mapsToLoad;
                        mapsToLoad = null;
                    }
                }

                if (toLoad != null) {
                    for (String loaded : customMaps) {
                        manager.unloadMap(loaded);
                    }

                    for (String load : toLoad) {
                        manager.loadMapByName(load);
                    }
                    customMaps = toLoad;
                    issueRefresh();
                }

                done = mapsToLoad == null;
            }
            return Status.OK_STATUS;
        }

        public void loadCustomMaps(Collection<String> maps) {
            synchronized (this) {
                mapsToLoad = new HashSet<String>(maps);
            }

            schedule();
        }

        public void clearMaps() {
            loadCustomMaps(new HashSet<String>());
        }
    }

    private class AreaHatcher extends Job {

        private final PolygonUtil polygonUtil;

        private Polygon hatchedArea;

        private Geometry hatchedWarningArea;

        private Geometry warningArea;

        private Polygon warningPolygon;

        private Polygon oldWarningPolygon;

        private boolean haveInput;

        public AreaHatcher(PolygonUtil polygonUtil) {
            super("Hatching Warning Area");
            setSystem(true);
            this.polygonUtil = polygonUtil;
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
         * IProgressMonitor)
         */
        @Override
        protected IStatus run(IProgressMonitor monitor) {
            while (this.warningArea != null && this.warningPolygon != null) {
                Geometry warningArea;
                Polygon warningPolygon;
                synchronized (polygonUtil) {
                    warningArea = this.warningArea;
                    warningPolygon = this.warningPolygon;
                    this.warningArea = this.warningPolygon = null;
                }

                try {
                    warningPolygon = PolygonUtil
                            .removeDuplicateCoordinate(warningPolygon);
                    Polygon hatched = polygonUtil.hatchWarningArea(
                            warningPolygon,
                            removeCounties(warningArea,
                                    state.getFipsOutsidePolygon()),
                            oldWarningPolygon);
                    if (hatched != null) {
                        // DR 15559
                        Coordinate[] coords = hatched.getCoordinates();
                        PolygonUtil.round(coords, 2);
                        PolygonUtil.adjustPolygon(coords);
                        PolygonUtil.round(coords, 2);
                        coords = PolygonUtil.removeDuplicateCoordinate(coords);
                        coords = PolygonUtil.removeOverlaidLinesegments(coords);
                        GeometryFactory gf = new GeometryFactory();
                        LinearRing lr = gf.createLinearRing(coords);
                        hatchedArea = gf.createPolygon(lr, null);
                        int adjustPolygon_counter = 0;
                        while (!hatchedArea.isValid()
                                && adjustPolygon_counter < 1) {
                            System.out.println("Calling adjustPolygon #"
                                    + adjustPolygon_counter);
                            PolygonUtil.adjustPolygon(coords);
                            PolygonUtil.round(coords, 2);
                            coords = PolygonUtil
                                    .removeDuplicateCoordinate(coords);
                            coords = PolygonUtil
                                    .removeOverlaidLinesegments(coords);
                            lr = gf.createLinearRing(coords);
                            hatchedArea = gf.createPolygon(lr, null);
                            adjustPolygon_counter += 1;
                        }
                        int counter = 0;
                        if (!hatchedArea.isValid() && counter < 2) {
                            System.out
                                    .println("calling adjustVertex & alterVertexes: loop #"
                                            + counter);
                            int adjustVertex_counter = 0;
                            lr = gf.createLinearRing(coords);
                            hatchedArea = gf.createPolygon(lr, null);
                            while (!hatchedArea.isValid()
                                    && adjustVertex_counter < 5) {
                                System.out.println("    Calling adjustVertex #"
                                        + adjustVertex_counter);
                                coords = PolygonUtil.adjustVertex(coords);
                                coords = PolygonUtil
                                        .removeDuplicateCoordinate(coords);
                                coords = PolygonUtil
                                        .removeOverlaidLinesegments(coords);
                                lr = gf.createLinearRing(coords);
                                hatchedArea = gf.createPolygon(lr, null);
                                adjustVertex_counter += 1;
                            }
                            int inner_counter = 0;
                            System.out.println("");
                            while (!hatchedArea.isValid() && inner_counter < 5) {
                                System.out
                                        .println("    Calling alterVertexes #"
                                                + inner_counter);
                                coords = PolygonUtil.alterVertexes(coords);
                                coords = PolygonUtil
                                        .removeDuplicateCoordinate(coords);
                                coords = PolygonUtil
                                        .removeOverlaidLinesegments(coords);
                                lr = gf.createLinearRing(coords);
                                hatchedArea = gf.createPolygon(lr, null);
                                inner_counter += 1;
                            }
                            counter += 1;
                        }
                        hatchedWarningArea = createWarnedArea(
                                latLonToLocal(hatchedArea),
                                latLonToLocal(warningArea));
                    } else {
                        this.hatchedArea = null;
                        this.hatchedWarningArea = null;
                    }
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
            }

            return Status.OK_STATUS;
        }

        public synchronized void hatchArea(Polygon warningPolygon,
                Geometry warningArea, Polygon oldWarningPolygon) {
            synchronized (polygonUtil) {
                this.warningPolygon = warningPolygon;
                this.warningArea = warningArea;
                this.oldWarningPolygon = oldWarningPolygon;
                this.haveInput = true;
            }
            schedule();
        }

        public synchronized Geometry[] getHatchedAreas() {
            Polygon hatchedArea = null;
            Geometry hatchedWarningArea = null;
            while (getState() != Job.NONE) {
                try {
                    join();
                } catch (InterruptedException e) {
                    break;
                }
            }
            if (! this.haveInput)
                return null;
            hatchedArea = this.hatchedArea;
            hatchedWarningArea = this.hatchedWarningArea;
            return new Geometry[] { hatchedArea, hatchedWarningArea };
        }

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

    private final CustomMaps customMaps;

    private AreaHatcher areaHatcher;

    protected Mode lastMode = null;

    protected boolean redrawBoxFromTrack = false;

    protected WarngenUIState state = new WarngenUIState();

    private boolean modifiedVertexNeedsToBeUpdated = false;

    private static UnitConverter knotToMeterPerSec = NonSI.KNOT
            .getConverterTo(SI.METERS_PER_SECOND);

    private static UnitConverter meterSqToKmSq = SI.METRE.times(SI.METRE)
            .getConverterTo(SI.KILOMETRE.times(SI.KILOMETRE));

    private GeospatialDataList geoData = null;

    private GeospatialDataAccessor geoAccessor = null;

    private WarningAction warningAction = WarningAction.NEW;

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
            GenericToolsResourceData<? extends AbstractStormTrackResource> resourceData,
            LoadProperties loadProperties, MapDescriptor descriptor) {
        super(resourceData, loadProperties, descriptor);
        displayState.displayType = DisplayType.POINT;
        getCapability(ColorableCapability.class).setColor(WHITE);
        customMaps = new CustomMaps();

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
                displayState.setInitiallyMotionless(!configuration
                        .isTrackEnabled());
                if (!configuration.isTrackEnabled()) {
                    displayState.angle = 0;
                    displayState.speed = 0;
                } else if (checkStormTrackData(data = ToolsDataManager
                        .getInstance().getStormTrackData())) {
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
        customMaps.clearMaps();

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
        // tilts where we had multiple frames then they went away.
        if (displayState.mode == Mode.TRACK && lastMode == Mode.DRAG_ME) {
            if (warningAction == null || warningAction == WarningAction.NEW) {
                // Initialize box
                redrawBoxFromTrack();
                if (((configuration.isTrackEnabled() == false || configuration
                        .getPathcastConfig() == null) && this.displayState.displayType != DisplayType.POLY)
                        || frameCount == 1) {
                    resetInitialFrame();
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
        double minX = paintProps.getView().getExtent().getMinX();
        double maxX = paintProps.getView().getExtent().getMaxX();
        double minY = paintProps.getView().getExtent().getMinY();
        double maxY = paintProps.getView().getExtent().getMaxY();
        double boundary = 40 * ratio;
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
                    out[0] = maxX - boundary;
                } else if (out[0] < minX) {
                    out[0] = minX + boundary;
                }
                if (out[1] > maxY) {
                    out[1] = maxY - boundary;
                } else if (out[1] < minY) {
                    out[1] = minY + boundary;
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
            displayState.setInitiallyMotionless(this.configuration
                    .isTrackEnabled() == false
                    || this.configuration.getPathcastConfig() == null);
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

        synchronized (siteMap) {
            loadGeodataForConfiguration(config);

            String areaSource = config.getGeospatialConfig().getAreaSource();
            geoData = siteMap.get(areaSource + "." + site);
            geoAccessor = new GeospatialDataAccessor(geoData,
                    config.getHatchedAreaSource());

            try {
                areaHatcher = new AreaHatcher(new PolygonUtil(this, geoData.nx,
                        geoData.ny, 20, geoData.localExtent,
                        geoData.localToLatLon));
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }// end synchronize
        customMaps.loadCustomMaps(Arrays.asList(config.getMaps()));

        this.configuration = config;
        System.out.println("Total time to init warngen config = "
                + (System.currentTimeMillis() - t0) + "ms");
    }

    /**
     * Adds geospatial data to siteMap and timezoneMap for the given template
     * configuration. This must not have any site effects on the currently
     * loaded template or the current product being edited.
     * 
     * @param config
     */
    private void loadGeodataForConfiguration(WarngenConfiguration config) {
        Map<String, GeospatialMetadata> metadataMap = GeospatialFactory
                .getMetaDataMap(config);
        String site = getLocalizedSite();

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

                        GeneralGridEnvelope range = new GeneralGridEnvelope(
                                new int[] { 0, 0 }, new int[] { gData.nx,
                                        gData.ny }, false);
                        GeneralEnvelope ge = new GeneralEnvelope(new double[] {
                                gData.localExtent.getMinX(),
                                gData.localExtent.getMaxY() }, new double[] {
                                gData.localExtent.getMaxX(),
                                gData.localExtent.getMinY() });

                        gData.localGridGeometry = new GeneralGridGeometry(
                                range, ge);

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
        }// end synchronize
    }

    public GeospatialData[] getGeodataFeatures(String areaSource,
            String localizedSite) {
        GeospatialDataList geoDataList = getGeodataList(areaSource,
                localizedSite);
        if (geoDataList != null) {
            return Arrays.copyOf(geoDataList.features,
                    geoDataList.features.length);
        }
        return new GeospatialData[0];
    }

    public GeneralGridGeometry getLocalGridGeometry() {
        return geoData.localGridGeometry;
    }

    public MathTransform getlocalToLatLon() {
        return geoData.localToLatLon;
    }

    private GeospatialDataList getGeodataList(String areaSource,
            String localizedSite) {
        String key = areaSource + "." + localizedSite;
        return siteMap.get(key);
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
            Geometry oldArea = getWarningAreaFromPolygon(
                    state.getOldWarningPolygon(), record);
            if (oldArea.getUserData() instanceof Set)
                state.setFipsOutsidePolygon((Set<String>) oldArea.getUserData());
            state.setOldWarningArea(oldArea);
        } else {
            state.setOldWarningArea(null);
            state.setOldWarningPolygon(null);
            state.setFipsOutsidePolygon(null);
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
     * Returns a set of UGCs for each area in the CWA that intersects the given
     * polygon.
     */
    public Set<String> getUgcsForCountyWatches(Polygon polygon)
            throws Exception {
        GeospatialDataAccessor gda = getCountyGeospatialDataAcessor();
        Set<String> ugcs = new HashSet<String>();
        for (String fips : gda.getAllFipsInArea(gda.buildArea(polygon))) {
            ugcs.add(FipsUtil.getUgcFromFips(fips));
        }
        return ugcs;
    }

    public Set<String> getAllCountyUgcs() throws Exception {
        GeospatialDataAccessor gda = getCountyGeospatialDataAcessor();
        Set<String> ugcs = new HashSet<String>();
        for (GeospatialData r : gda.geoData.features) {
            ugcs.add(FipsUtil.getUgcFromFips(gda.getFips(r)));
        }
        return ugcs;
    }

    private GeospatialDataAccessor getCountyGeospatialDataAcessor()
            throws Exception {
        GeospatialDataList gdl = searchCountyGeospatialDataAccessor();
        if (gdl == null) {
            // Cause county geospatial data to be loaded
            // TODO: Should not be referencing tornadoWarning.
            WarngenConfiguration torConfig = WarngenConfiguration.loadConfig(
                    "tornadoWarning", getLocalizedSite());
            loadGeodataForConfiguration(torConfig);
            gdl = searchCountyGeospatialDataAccessor();
        }

        // TODO: There should be some way to get the "county" configuration by
        // name
        // independent of a template
        AreaSourceConfiguration areaConfig = new AreaSourceConfiguration();
        areaConfig.setFipsField("FIPS");

        return new GeospatialDataAccessor(gdl, areaConfig);
    }

    private GeospatialDataList searchCountyGeospatialDataAccessor() {
        synchronized (siteMap) {
            for (Map.Entry<String, GeospatialDataList> entry : siteMap
                    .entrySet()) {
                String[] keyParts = entry.getKey().split("\\.");
                if (keyParts.length == 2
                        && "county".equalsIgnoreCase(keyParts[0])
                        && getLocalizedSite().equals(keyParts[1])) {
                    return entry.getValue();
                }
            }
        }
        return null;
    }

    /**
     * Given the intersection area and polygon, build the area for the county
     * map
     * 
     * @param area
     * @param countyMap
     * @param includeAllEntries
     *            if true, ensure all entries in countyMap are represented in
     *            the result even if not in {@code area}.
     * @return the resulting area. If includeAllEntries is true and there are
     *         areas in countyMap not inside {@code area}, the user data will be
     *         set to a Set of the FIPS IDs (or equivalent) of those outside
     *         areas.
     */
    private Geometry getArea(Geometry area, Map<String, String[]> countyMap,
            boolean includeAllEntries) {
        if (area == null) {
            return null;
        }

        // Now remove counties not present in warning

        Set<String> idsOutsidePolygon = null;
        Set<String> fipsOutsidePolygon = null;
        if (includeAllEntries) {
            idsOutsidePolygon = new HashSet<String>();
            for (Map.Entry<String, String[]> entry : countyMap.entrySet()) {
                String state = entry.getKey();
                for (String id : entry.getValue()) {
                    idsOutsidePolygon.add(state + '-' + id);
                }
            }
        }

        List<Geometry> geoms = new ArrayList<Geometry>();
        GeometryUtil.buildGeometryList(geoms, area);
        List<Geometry> newList = new ArrayList<Geometry>();
        boolean isMarineZone = configuration.getGeospatialConfig()
                .getAreaSource().equalsIgnoreCase(MARINE);
        for (Geometry geom : geoms) {
            CountyUserData data = (CountyUserData) geom.getUserData();

            String fips = null;
            String[] ids = null;
            if (isMarineZone) {
                fips = String.valueOf(data.entry.attributes.get(configuration
                        .getHatchedAreaSource().getFipsField()));
                if (countyMap.containsKey(fips.substring(0, 2))) {
                    ids = countyMap.get(fips.substring(0, 2));
                    for (String id : ids) {
                        if (fips.endsWith(id)) {
                            if (idsOutsidePolygon != null) {
                                idsOutsidePolygon.remove(fips.substring(0, 2)
                                        + '-' + id);
                            }
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
                            if (idsOutsidePolygon != null) {
                                idsOutsidePolygon.remove(stateAbbr + '-' + id);
                            }
                            newList.add(geom);
                            break;
                        }
                    }
                }
            }
        }

        if (includeAllEntries && !idsOutsidePolygon.isEmpty()) {
            if (geoData != null) {
                fipsOutsidePolygon = new HashSet<String>();
                for (GeospatialData f : geoData.features) {
                    CountyUserData data = (CountyUserData) f.geometry
                            .getUserData();
                    String fips = String.valueOf(data.entry.attributes
                            .get(configuration.getHatchedAreaSource()
                                    .getFipsField()));
                    String key;
                    if (isMarineZone) {
                        key = fips.substring(0, 2) + '-' + fips.substring(3);
                    } else {
                        String stateAbbr = String.valueOf(data.entry.attributes
                                .get(configuration.getHatchedAreaSource()
                                        .getAreaNotationField()));
                        key = stateAbbr + '-' + fips.substring(2);
                    }
                    if (idsOutsidePolygon.contains(key)) {
                        newList.add((Geometry) f.geometry.clone());
                        fipsOutsidePolygon.add(getFips(f));
                    }
                }
            }
        }

        Geometry result = area.getFactory().createGeometryCollection(
                newList.toArray(new Geometry[newList.size()]));
        if (fipsOutsidePolygon != null)
            result.setUserData(fipsOutsidePolygon);

        return result;
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
        return getArea(geoAccessor.buildArea(polygon), countyMap, true);
    }

    /**
     * Create the WarnGen dialog if it has not been created.
     */
    public void createDialog() {
        if (dialog == null || dialog.isDisposed() == true) {
            dialog = new WarngenDialog(PlatformUI.getWorkbench()
                    .getActiveWorkbenchWindow().getShell(), this);
            dialog.open();
            dialog.addListener(SWT.Dispose, new Listener() {
                @Override
                public void handleEvent(Event event) {
                    descriptor.getResourceList().removeRsc(WarngenLayer.this);
                }
            });
        } else {
            showDialog(true);
        }
    }

    /**
     * Show the WarnGen dialog and move it to the front.
     */
    public void showDialog(boolean show) {
        if (dialog != null && dialog.isDisposed() == false) {
            dialog.showDialog(show);
        }
    }

    public void updateWarnedAreas(boolean snapHatchedAreaToPolygon)
            throws VizException {
        updateWarnedAreas(snapHatchedAreaToPolygon, false);
    }

    /**
     * 
     * @param snapHatchedAreaToPolygon
     *            If True, any hatched area outside the polygon will be
     *            eliminated.
     * @throws VizException
     */
    public void updateWarnedAreas(boolean snapHatchedAreaToPolygon,
            boolean preservedSelection) throws VizException {
        if (getPolygon() == null) {
            return;
        }

        long t0 = System.currentTimeMillis();

        Geometry warningArea = state.getWarningArea();
        Geometry warningPolygon = state.getWarningPolygon();
        Geometry newWarningArea = createWarnedArea(
                latLonToLocal((snapHatchedAreaToPolygon || warningArea == null) ? warningPolygon
                        : warningArea), preservedSelection
                        && warningArea != null ? latLonToLocal(warningArea)
                        : null);
        updateWarnedAreaState(newWarningArea, snapHatchedAreaToPolygon);

        System.out.println("determining hatchedArea took "
                + (System.currentTimeMillis() - t0));
    }

    /**
     * Creates a warning area based on the hatched area in local coordinates
     * 
     * @param hatchedArea
     * @param preservedSelection
     *            if not null, the result contains all entities in this Geometry
     *            even if they do not intersect hatchedArea or do not pass the
     *            inclusion filter
     * @return
     */
    private Geometry createWarnedArea(Geometry hatchedArea,
            Geometry preservedSelection) {
        Geometry oldWarningPolygon = latLonToLocal(state.getOldWarningPolygon());
        Geometry oldWarningArea = latLonToLocal(state.getOldWarningArea());
        Geometry newHatchedArea = null;
        Geometry newUnfilteredArea = null;
        boolean useFilteredArea = false;
        boolean useFallback = getConfiguration().getHatchedAreaSource().isInclusionFallback();

        /*
         * The resultant warning area is constructed in one of two ways:
         *
         * 1. When preservedSelection is null:
         *
         * If at least one county in hatchedArea passes the inclusion filter,
         * the result contains only the counties in hatchedArea that pass the
         * inclusion filter. Otherwise, all counties in hatchedArea are
         * included.
         *
         * This behavior reflects A1 baseline template logic. The fallback can
         * be disabled by setting AreaSourceConfiguration.isInclusionFallback to
         * false.
         *
         * 2. When preservedSelection is not null:
         *
         * A county is included in the result if and only if it is contained in
         * preservedSelection. If the portion of the county in hatchedArea is
         * non-empty, it used. Otherwise, the hatched portion from
         * preservedSelection is used.
         *
         *
         * In both cases, when there is an old warning area in effect (i.e., for
         * followups), the intersection of hatchedArea and the old warning area
         * is used instead of hatchedArea.
         */

        Set<String> selectedFips = null;
        List<Geometry> selectedGeoms = null;
        if (preservedSelection != null)
            selectedFips = getAllFipsInArea(preservedSelection);

        // Loop through each of our counties returned from the query
        for (GeospatialData f : geoData.features) {
            // get the geometry of the county and make sure it intersects
            // with our hatched area
            PreparedGeometry prepGeom = (PreparedGeometry) f.attributes
                    .get(GeospatialDataList.LOCAL_PREP_GEOM);
            Geometry intersection = null;
            try {
                // Get intersection between county and hatched boundary
                intersection = GeometryUtil.intersection(hatchedArea, prepGeom);
                if (oldWarningArea != null) {
                    intersection = GeometryUtil.intersection(intersection,
                            oldWarningArea);
                }
                if (intersection.isEmpty()) {
                    if (selectedFips == null
                            || !selectedFips.contains(getFips(f))) {
                        continue;
                    } else if (!selectedFips.isEmpty()) {
                        /*
                         * Add whatever part of the area was previously hatched
                         * despite being outside the new polygon.
                         */
                        if (selectedGeoms == null) {
                            selectedGeoms = new ArrayList<Geometry>();
                            GeometryUtil.buildGeometryList(selectedGeoms,
                                    preservedSelection);
                        }
                        intersection = null;
                        String prefix = GeometryUtil.getPrefix(f.geometry
                                .getUserData());
                        for (Geometry g : selectedGeoms) {
                            if (g.getUserData() != null) {
                                if (prefix.equals(GeometryUtil.getPrefix(g
                                        .getUserData()))) {
                                    intersection = intersection == null ? g
                                            : GeometryUtil.union(intersection,
                                                    g);
                                }
                            }
                        }
                        if (intersection == null) {
                            // This part of the area was not previously
                            // selected.
                            continue;
                        }
                    }
                }
            } catch (RuntimeException e) {
                continue;
                // This is a workaround for JTS 1.7.1
            }

            try {
                boolean include;
                if (selectedFips != null) {
                    include = selectedFips.contains(getFips(f));
                    useFilteredArea = true;
                } else {
                    boolean passed = filterArea(f, intersection, true);
                    useFilteredArea = useFilteredArea || passed;
                    include = (passed || filterAreaSecondChance(f, intersection, true))
                            && (oldWarningPolygon == null
                                    || prepGeom.intersects(oldWarningPolygon) || isOldAreaOutsidePolygon(f));
                    newUnfilteredArea = union(newUnfilteredArea, intersection);
                }
                if (include) {
                    newHatchedArea = union(newHatchedArea, intersection);
                }

            } catch (TopologyException e) {
                statusHandler.handle(Priority.VERBOSE,
                        "Geometry error simplifying hatched area.", e);
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Inclusion Area not properly configured.", e);
            }
        }

        newHatchedArea = useFilteredArea && newHatchedArea != null ? newHatchedArea :
                useFallback ? newUnfilteredArea : null;
        return newHatchedArea != null ? newHatchedArea : new GeometryFactory()
                .createGeometryCollection(new Geometry[0]);
    }

    private static Geometry union(Geometry a, Geometry b) {
        if (a != null && b != null)
            return GeometryUtil.union(a, b);
        else
            return a != null ? a : b;
    }

    private void updateWarnedAreaState(Geometry newHatchedArea,
            boolean snapToHatchedArea) throws VizException {
        try {
            // Ensure all geometries in local coords
            Geometry warningPolygon = latLonToLocal(state.getWarningPolygon());
            Geometry oldWarningArea = latLonToLocal(state.getOldWarningArea());
            Geometry oldWarningPolygon = latLonToLocal(state
                    .getOldWarningPolygon());

            // All area has been removed from the polygon...
            if (newHatchedArea.isEmpty()) {
                state.strings.clear();
                state.setWarningArea(null);
                state.geometryChanged = true;
                dialog.getDisplay().asyncExec(new Runnable() {
                    @Override
                    public void run() {
                        dialog.setInstructions();
                    }
                });
                if (!state.isMarked() || oldWarningArea == null) {
                    return;
                }
            }

            if (oldWarningArea != null) {
                int areaPercent = -1;
                try {
                    areaPercent = Double.valueOf(
                            ((oldWarningPolygon.intersection(warningPolygon)
                                    .getArea() / oldWarningArea.getArea()) * 100))
                                    .intValue();
                } catch (Exception e) {
                    statusHandler.handle(Priority.VERBOSE,
                            "Error determining amount of overlap with original polygon", e);
                    areaPercent = 100;
                }
                if (oldWarningPolygon.intersects(warningPolygon) == false
                        && !state.isMarked()) {
                    // Snap back to polygon
                    state.setWarningPolygon(localToLatLon((Polygon) oldWarningPolygon));
                    newHatchedArea = (Geometry) oldWarningArea.clone();
                } else if (oldWarningPolygon.intersects(warningPolygon) == false
                        && areaPercent < 10 && state.isMarked()) {
                    // snap back to last valid user selected area
                    state.setWarningPolygon((Polygon) state
                            .getMarkedWarningPolygon().clone());
                    newHatchedArea = state.getMarkedWarningArea();
                    state.resetMarked();
                } else if (warningPolygon != null) {
                    // want intersection of warningPolygon and oldWarningArea
                    Set<String> selectedGids = new HashSet<String>(
                            Arrays.asList(GeometryUtil.getGID(newHatchedArea)));
                    Geometry selectedArea = newHatchedArea;
                    newHatchedArea = GeometryUtil.intersection(warningPolygon,
                            oldWarningArea);
                    Set<String> newGids = new HashSet<String>(
                            Arrays.asList(GeometryUtil.getGID(newHatchedArea)));
                    if (!selectedGids.equals(newGids)) {
                        // Remove areas with gid in removedGids
                        List<Geometry> areas = new ArrayList<Geometry>(
                                newHatchedArea.getNumGeometries());
                        Set<String> seenGids = new HashSet<String>();
                        for (int n = 0; n < newHatchedArea.getNumGeometries(); ++n) {
                            Geometry newArea = newHatchedArea.getGeometryN(n);
                            String[] gids = GeometryUtil.getGID(newArea);
                            boolean flag = false;
                            for (String gid : gids) {
                                if (!selectedGids.contains(gid)) {
                                    flag = true;
                                    break;
                                }
                            }
                            if (!flag) {
                                areas.add(newArea);
                                seenGids.addAll(Arrays.asList(gids));
                            }
                        }
                        selectedGids.removeAll(seenGids);
                        if (!selectedGids.isEmpty()) {
                            for (int n = 0; n < selectedArea.getNumGeometries(); ++n) {
                                Geometry area = selectedArea.getGeometryN(n);
                                String[] gids = GeometryUtil.getGID(area);
                                boolean flag = false;
                                for (String gid : gids) {
                                    if (selectedGids.contains(gid)) {
                                        flag = true;
                                        break;
                                    }
                                }
                                if (flag)
                                    areas.add(area);
                            }
                        }
                        newHatchedArea = GeometryUtil.union(areas
                                .toArray(new Geometry[0]));
                    }
                }
            }

            if (newHatchedArea == null || newHatchedArea.isEmpty()) {
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
                    state.resetMarked();
                    updateWarnedAreas(snapToHatchedArea);
                }
            } else {
                state.setWarningArea(localToLatLon(newHatchedArea));
                state.mark(newHatchedArea);
                // add "W" strings
                populateStrings();
            }

            // Apply new hatched area
            state.geometryChanged = true;
            issueRefresh();

            VizApp.runAsync(new Runnable() {
                @Override
                public void run() {
                    if (dialog != null) {
                        dialog.setInstructions();
                    }
                }
            });
        } finally {
            warningAreaChanged();
        }
    }

    /**
     * Determine if the given area of the reference area passes the inclusion
     * filter. Subroutine of {@link #filterArea}.
     * 
     * @param areaToConsider
     * @param wholeArea
     * @param areaInMetersSq
     * @param anyAmountOfArea
     * @return
     */
    private boolean filterCheck(Geometry areaToConsider, Geometry wholeArea,
            double areaInMetersSq) {
        double ratio = areaToConsider.getArea() / wholeArea.getArea();
        double ratioInPercent = ratio * 100.;
        double areaInKmSqOfIntersection = meterSqToKmSq.convert(areaInMetersSq
                * ratio);

        boolean percentOk = ratioInPercent >= getConfiguration()
                .getHatchedAreaSource().getInclusionPercent();
        boolean areaOk = areaInKmSqOfIntersection > getConfiguration()
                .getHatchedAreaSource().getInclusionArea();
        return getConfiguration().getHatchedAreaSource().getInclusionAndOr()
                .equalsIgnoreCase("AND") ? percentOk && areaOk : percentOk
                || areaOk;
    }

    /**
     * Determine if a feature should be included based on how much of it is
     * hatched and the configured inclusion criteria.
     * 
     * @param feature
     * @param featureAreaToConsider
     *            the portion of the feature that is hatched
     * @param localCoordinates
     *            if true, use local CRS; otherwise, use lat/lon
     * @return true if the feature should be included
     */
    private boolean filterArea(GeospatialData feature,
            Geometry featureAreaToConsider, boolean localCRS) {
        Geometry geom = localCRS ? (Geometry) feature.attributes
                .get(GeospatialDataList.LOCAL_GEOM) : feature.geometry;
        double areaOfGeom = (Double) feature.attributes.get(AREA);

        return filterCheck(featureAreaToConsider, geom, areaOfGeom);
    }

    private boolean filterAreaSecondChance(GeospatialData feature,
            Geometry featureAreaToConsider, boolean localCRS) {
        Geometry geom = localCRS ? (Geometry) feature.attributes
                .get(GeospatialDataList.LOCAL_GEOM) : feature.geometry;
        double areaOfGeom = (Double) feature.attributes.get(AREA);

        if (state.getOldWarningArea() != null) {
            /*
             * Second chance: If the county slipped by the filter in the initial
             * warning, allow it now as long as the hatched area is (nearly) the
             * same as the hatched area in the initial warning.
             * 
             * This test assumes that the followup filter is not more permissive
             * that the initial warning filter. OTOH, if the followup filter is
             * more permissive, this test is not really necessary.
             */
            Geometry oldWarningArea = state.getOldWarningArea();
            if (localCRS)
                oldWarningArea = latLonToLocal(oldWarningArea);
            List<Geometry> geoms = new ArrayList<Geometry>();
            GeometryUtil.buildGeometryList(geoms, oldWarningArea);
            Geometry oldSelectedArea = null;
            String prefix = GeometryUtil.getPrefix(feature.geometry
                    .getUserData());
            for (Geometry g : geoms) {
                if (g.getUserData() != null) {
                    if (prefix.equals(GeometryUtil.getPrefix(g.getUserData()))) {
                        oldSelectedArea = oldSelectedArea == null ? g
                                : GeometryUtil.union(oldSelectedArea, g);
                    }
                }
            }
            if (oldSelectedArea != null) {
                double ratioOfOldArea = featureAreaToConsider.getArea()
                        / oldSelectedArea.getArea();
                /*
                 * Ideally, we would only allow the exact same area, but due to
                 * possible loss of precision in all of the calculations, we
                 * allow >= 0.999.
                 */
                return ratioOfOldArea >= .999
                        && !filterCheck(oldSelectedArea, geom, areaOfGeom);
            }
        }
        return false;
    }

    private boolean isOldAreaOutsidePolygon(GeospatialData f) {
        Set<String> fipsOutsidePolygon = state.getFipsOutsidePolygon();
        if (fipsOutsidePolygon != null)
            return fipsOutsidePolygon.contains(getFips(f));
        return false;
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

        updateWarnedAreas(true);
    }

    public void redrawBoxFromTrack() throws VizException {

        if (displayState.mode == Mode.DRAG_ME) {
            return;
        }
        if (warningAction == null || warningAction == WarningAction.NEW) {
            if ((configuration.isTrackEnabled() == false || configuration
                    .getPathcastConfig() == null)
                    && !this.displayState.isNonstationary()
                    && this.displayState.displayType != DisplayType.POLY) {
                createSquare();
                return;
            } else if (descriptor.getFramesInfo().getFrameCount() == 1) {
                createSquare();
                return;
            }
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

            updateWarnedAreas(true);

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

            updateWarnedAreas(true);
        }
        if (dialog.box.getSelection()) {
            displayState.editable = false;
        }
        dialog.setInstructions();
    }

    /**
     * @return true if the box has been redraw successfully
     */
    public boolean redrawBoxFromHatched() {
        boolean result = true;
        if (state.snappedToArea == false) {
            if (state.getWarningArea() == null
                    || state.getWarningArea().isEmpty()) {
                return true;
            }

            try {
                long t0 = System.currentTimeMillis();
                Polygon hatched = state.getWarningPolygon();
                Geometry hatchedArea = state.getWarningArea();
                if (areaHatcher != null) {
                    Geometry[] areas = areaHatcher.getHatchedAreas();
                    if (areas == null) {
                        // Somehow, the hatcher has not been run.  Try it now.
                        warningAreaChanged();
                        areas = areaHatcher.getHatchedAreas();
                        // If still null, give up.
                        if (areas == null)
                            return false;
                    }
                    hatched = (Polygon) areas[0];
                    hatchedArea = areas[1];
                }

                if (hatched != null) {
                    state.setWarningPolygon(hatched);
                    updateWarnedAreaState(hatchedArea, true);
                    issueRefresh();
                    // End of DR 15559
                    state.snappedToArea = true;
                } else {
                    /*
                     * If redraw failed, do not allow this polygon to be used to
                     * generate a warning.
                     *
                     * Note that this duplicates code from updateWarnedAreaState.
                     */
                    state.strings.clear();
                    state.setWarningArea(null);
                    state.geometryChanged = true;
                    if (dialog != null) {
                        dialog.getDisplay().asyncExec(new Runnable() {
                            @Override
                            public void run() {
                                dialog.setInstructions();
                            }
                        });
                    }
                    state.resetMarked();
                    state.geometryChanged = true;
                    issueRefresh();
                    statusHandler.handle(Priority.PROBLEM,
                            "Could not redraw box from warned area");
                    result = false;
                }
                System.out.println("Time to createWarningPolygon: "
                        + (System.currentTimeMillis() - t0) + "ms");
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error hatching polygon", e);
                result = false;
            }
            issueRefresh();
        }
        return result;
    }

    public void createDamThreatArea(Coordinate[] coordinates) {
        GeometryFactory gf = new GeometryFactory();
        LinearRing lr = gf.createLinearRing(coordinates);
        Coordinate pt = lr.getCentroid().getCoordinate();

        try {
            state.setWarningPolygon(gf.createPolygon(lr, null));
            state.rightClickSelected = false;
            updateWarnedAreas(true);
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
        if (motdir != null && motspd != null
                && (motspd != 0 || configuration.isTrackEnabled())) {
            displayState.setInitiallyMotionless(false);
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
        } else {
            displayState.setInitiallyMotionless(true);
            displayState.angle = 0;
            displayState.speed = 0;
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
            int hour = Integer.parseInt(m.group(1));
            int minute = Integer.parseInt(m.group(2));
            frameTime = TimeUtil.timeOfDayToAbsoluteTime(hour
                    * TimeUtil.SECONDS_PER_HOUR + minute
                    * TimeUtil.SECONDS_PER_MINUTE, warnRecord.getIssueTime());
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
        if (currentFrame == frameCount - 1 || !displayState.isNonstationary()) {
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
            if (warningAction == null || warningAction == WarningAction.NEW
                    || warningAction == WarningAction.CON
                    || warningAction == WarningAction.CAN) {
                Coordinate coord = new Coordinate(
                        stormTrackState.dragMePoint.getCoordinate());
                DataTime currentDataTime = new DataTime(SimulatedTime
                        .getSystemTime().getTime());
                if (stormTrackState.compuateCurrentStormCenter(coord,
                        currentDataTime))
                    cc = new Coordinate[] { coord };
            }
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

            // TODO: Should this even be null when there is no hatching?
            Geometry warningArea = state.getWarningArea();
            if (warningArea == null) {
                warningArea = new GeometryFactory()
                        .createGeometryCollection(new Geometry[0]);
            }

            GeometryFactory gf = new GeometryFactory();
            Point point = gf.createPoint(coord);
            // potentially adding or removing a county, figure out county
            for (GeospatialData f : geoData.features) {
                Geometry geom = f.geometry;
                if (f.prepGeom.contains(point)) {
                    Geometry newWarningArea;
                    if (GeometryUtil.contains(warningArea, point)) {
                        // remove county
                        Geometry tmp = removeCounty(warningArea, getFips(f));
                        if (tmp.isEmpty()) {
                            String fip = getFips(f);
                            if (fip != null && uniqueFip != null
                                    && fip.equals(uniqueFip)) {
                                updateWarnedAreas(true);
                            }
                            break;
                        }

                        newWarningArea = tmp;
                    } else {
                        // add county
                        String featureFips = getFips(f);
                        Collection<GeospatialData> dataWithFips = getDataWithFips(featureFips);
                        if (oldWarningArea != null) {
                            // for a CON, prevents extra areas to be added
                            Set<String> fipsIds = getAllFipsInArea(oldWarningArea);
                            if (fipsIds.contains(featureFips) == false ||
                                    ! (oldWarningPolygon.contains(point) == true
                                    || isOldAreaOutsidePolygon(f))) {
                                break;
                            }
                        }

                        // Get intersecting parts for each geom with
                        // matching fips
                        List<Geometry> fipsParts = new ArrayList<Geometry>(
                                dataWithFips.size());
                        for (GeospatialData gd : dataWithFips) {
                            Geometry g = gd.geometry;
                            if (oldWarningArea != null) {
                                g = GeometryUtil.intersection(oldWarningArea, g);
                            }
                            fipsParts.add(g);
                        }
                        // Create a collection of each part
                        geom = GeometryUtil.union(fipsParts
                                .toArray(new Geometry[fipsParts.size()]));
                        if (warningPolygon.contains(point)) {
                            // If inside warning polygon, intersect
                            geom = GeometryUtil.intersection(
                                    warningPolygon, geom);
                        }
                        newWarningArea = GeometryUtil.union(
                                removeCounty(warningArea, featureFips),
                                geom);
                    }
                    state.setWarningArea(filterWarningArea(newWarningArea));
                    setUniqueFip();
                    warningAreaChanged();
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

    private Geometry filterWarningArea(Geometry warningArea) {
        // TODO: Duplicates logic in createWarnedArea
        if (warningArea == null)
            return null;
        /*
         * Note: Currently does not determine if warningArea is valid (i.e., in
         * contained in CWA, old warning area, etc.) or has overlapping geometries.
         */
        Geometry newHatchedArea = null;
        Geometry newUnfilteredArea = null;
        boolean useFilteredArea = false;
        boolean useFallback = getConfiguration().getHatchedAreaSource().isInclusionFallback();

        for (GeospatialData f : geoData.features) {
            String gid = GeometryUtil.getPrefix(f.geometry.getUserData());
            Geometry warningAreaForFeature = getWarningAreaForGids(Arrays.asList(gid), warningArea);
            boolean passed = filterArea(f, warningAreaForFeature, false);
            useFilteredArea = useFilteredArea || passed;
            if (passed || filterAreaSecondChance(f, warningAreaForFeature, false))
                newHatchedArea = union(newHatchedArea, warningAreaForFeature);
            newUnfilteredArea = union(newUnfilteredArea, warningAreaForFeature);
        }

        newHatchedArea = useFilteredArea && newHatchedArea != null ? newHatchedArea :
                useFallback ? newUnfilteredArea : null;

        return newHatchedArea != null ? newHatchedArea : new GeometryFactory()
                .createGeometryCollection(new Geometry[0]);
    }

    private String getFips(GeospatialData data) {
        return geoAccessor.getFips(data);
    }

    private String getFips(Geometry g) {
        return geoAccessor.getFips(g);
    }

    private void warningAreaChanged() {
        state.snappedToArea = false;
        if (areaHatcher != null) {
            Polygon polygon = state.getWarningPolygon();
            polygon = tryToIntersectWithOriginalPolygon(polygon);
            areaHatcher.hatchArea(polygon, state.getWarningArea(),
                    state.getOldWarningPolygon());
        }
    }

    /**
     * Try to determine the intersection of the given polygon with the original
     * warning polygon. If there is no original polygon, if the result of the
     * intersection is not a single polygon, or if a problem occurs, just return
     * the original polygon. The purpose of this is to pass the polygon that
     * best represents the user's intent to the polygon redrawing algorithm.
     */
    private Polygon tryToIntersectWithOriginalPolygon(Polygon polygon) {
        if (state.getOldWarningPolygon() != null) {
            try {
                Geometry g = polygon.intersection(state.getOldWarningPolygon());
                Polygon newPolygon = null;
                if (g instanceof Polygon) {
                    newPolygon = (Polygon) g;
                } else if (g instanceof GeometryCollection
                        && g.getNumGeometries() == 1
                        && g.getGeometryN(0) instanceof Polygon) {
                    newPolygon = (Polygon) g.getGeometryN(0);
                }
                if (newPolygon != null && newPolygon.isValid()) {
                    polygon = newPolygon;
                }
            } catch (TopologyException e) {
                // ignore
            }
        }
        return polygon;
    }

    private Collection<GeospatialData> getDataWithFips(String fips) {
        List<GeospatialData> data = new ArrayList<GeospatialData>();
        for (GeospatialData d : geoData.features) {
            if (fips.equals(getFips(d))) {
                data.add(d);
            }
        }
        return data;
    }

    private Set<String> getAllFipsInArea(Geometry warningArea) {
        return geoAccessor.getAllFipsInArea(warningArea);
    }

    private Geometry removeCounty(Geometry warningArea, String fipsToRemove) {
        Set<String> set = new HashSet<String>();
        set.add(fipsToRemove);
        return removeCounties(warningArea, set);
    }

    private Geometry removeCounties(Geometry warningArea,
            Set<String> fipsToRemove) {
        if (fipsToRemove == null || fipsToRemove.isEmpty())
            return warningArea;
        List<Geometry> toKeep = new ArrayList<Geometry>(
                warningArea.getNumGeometries());
        for (int n = 0; n < warningArea.getNumGeometries(); ++n) {
            Geometry area = warningArea.getGeometryN(n);
            String areaFips = getFips(area);
            if (fipsToRemove.contains(areaFips) == false) {
                toKeep.add(area);
            }
        }
        return warningArea.getFactory().createGeometryCollection(
                toKeep.toArray(new Geometry[0]));
    }

    /**
     * Populate the W strings with the included counties
     */
    private void populateStrings() {
        state.strings.clear();
        Set<String> prefixes = new HashSet<String>(Arrays.asList(GeometryUtil
                .getGID(state.getWarningArea())));

        prefixes = removeDuplicateGid(prefixes);

        for (GeospatialData f : geoData.features) {
            Geometry geom = f.geometry;
            Geometry geom2 = null;
            Geometry warningAreaN = null;
            Coordinate populatePt = null;
            Geometry populatePtGeom;
            boolean contained = false, closeTo = false;
            double shift = 1.E-8, distance, minDistance = 10.0;
            int loop, maxLoop = 10;
            Geometry warningArea = state.getWarningArea();
            String prefix = GeometryUtil.getPrefix(geom.getUserData());
            if (prefixes.contains(prefix)) {
                loop = 0;
                warningAreaN = findLargestGeometry(GeometryUtil.intersection(geom, warningArea));
                do {
                    if (!warningAreaN.isEmpty()) {
                        populatePt = GisUtil.d2dCoordinate(warningAreaN.getCentroid()
                                .getCoordinate());
                        for (GeospatialData f2 : geoData.features) {
                            geom2 = f2.getGeometry();
                            if (!GeometryUtil.getPrefix(geom2.getUserData()).equals(prefix)) {
                                contained = false;
                                closeTo = false;
                                populatePtGeom = PolygonUtil.createPolygonByPoints(populatePt,
                                        new Coordinate(populatePt.x + shift, populatePt.y + shift));
                                if (GeometryUtil.contains(geom2, populatePtGeom)) {
                                    // populatePt is in another county/zone.
                                    warningAreaN = findLargestQuadrant(warningAreaN, geom);
                                    contained = true;
                                    break;
                                } else {
                                    distance = populatePtGeom.distance(geom2);
                                    if (distance < minDistance) {
                                        // populatePt is very close to the boundary of another county/zone.
                                        warningAreaN = findLargestQuadrant(warningAreaN, geom);
                                        closeTo = true;
                                        break;
                                    }
                                }
                            }
                        }
                    } else {
                        // use the existing populatePt
                        break;
                    }
                    loop += 1;
                } while ((contained || closeTo) && loop <= maxLoop);
                state.strings.put(populatePt, "W");
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
                dialog.realizeEditableState();
                final WarngenDialog dlg = dialog;
                dialog.getDisplay().asyncExec(new Runnable() {
                    @Override
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
    static public <T> T convertGeom(T geom, MathTransform transform) {
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

    /**
     * Some counties/forecast zones have two GIDs, one is for the large portion
     * of that county and the other is for the small portion, e.g., inlets of a
     * bay. Prince William County, Virginia is such an example. As WarnGen needs
     * to mark a hatched county with only one W, one of the GIDs needs to be
     * filtered out. The approach in the method is to remove the GID for the
     * area of smaller size.
     */
    private Set<String> removeDuplicateGid(Set<String> prefixes) {

        if (prefixes.size() < 2)
            return prefixes;

        Map<String, Double> fipsSize = new HashMap<String, Double>();
        Map<String, String> namePrefix = new HashMap<String, String>();
        Iterator<String> iter = prefixes.iterator();
        String fips = null;
        String prefix = null;
        while (iter.hasNext()) {
            prefix = iter.next();
            double size = 0.0d;
            for (GeospatialData f : geoData.features) {
                fips = getFips(f);
                Geometry geom = f.geometry;
                if (prefix.equals(GeometryUtil.getPrefix(geom.getUserData()))) {
                    size = geom.getArea();
                    if (fipsSize.containsKey(fips)) {
                        if (fipsSize.get(fips) < size) {
                            fipsSize.put(fips, size);
                            namePrefix.put(fips, prefix);
                            break;
                        }
                    } else {
                        fipsSize.put(fips, size);
                        namePrefix.put(fips, prefix);
                    }
                }
            }
        }
        return new HashSet<String>(namePrefix.values());
    }

    public void setUniqueFip() {
        Geometry g = state.getWarningArea();
        uniqueFip = null;
        if (g != null) {
            if (getAllFipsInArea(g).size() == 1) {
                Set<String> fips = getAllFipsInArea(g);
                Iterator<String> iter = fips.iterator();
                uniqueFip = iter.next();
            }
        }
    }

    /** 
     * If g is a GeometryCollection, find the largest Geomery in it; otherwise (i.e., g is Geometry), return g.
     * 
     * @param g
     *     A Geometry or a GeometryCollection.
     * @return Geometry
     */
    private Geometry findLargestGeometry(Geometry g) {
        int size = g.getNumGeometries();
        if (size == 1)
            return g;
        double area, maxArea = -1.0;
        int index = 0;
        for (int i = 0; i < size; i++) {
            area = g.getGeometryN(i).getArea();
            if (area > maxArea) {
                maxArea = area;
                index = i;
            }
        }
        return g.getGeometryN(index);
    }

    /** 
     * Split the hatched area into four quadrants, and return the largest one.
     * 
     * @param hatchedArea
     *     The initial hatched area or its a sub area.
     * @param geom
     *     The geometry of a county/zone.
     * @return Geometry
     *     The geometey of largest quadrant among the four, which are the result of 
     *     splitting of hatchedArea.
     */
    private Geometry findLargestQuadrant(Geometry hatchedArea, Geometry geom) {
        Geometry envelope = hatchedArea.getEnvelope();
        Coordinate centroidCoord = GisUtil.d2dCoordinate(envelope.getCentroid()
                .getCoordinate());
        Coordinate[] envCoords = envelope.getCoordinates();
        int size = 4;
        Geometry quadrants[] = new Geometry[size];
        Geometry intersections[] = new Geometry[size];
        double largestArea = -1.0, area = -1.0;
        int index = -1;
        for (int i = 0; i < size; i++) {
            quadrants[i] = PolygonUtil.createPolygonByPoints(envCoords[i], centroidCoord);
            intersections[i] = GeometryUtil.intersection(quadrants[i], hatchedArea);
            area = intersections[i].getArea();
            if (area > largestArea) {
                largestArea = area;
                index = i;
            }
        }
        if (intersections[index].isValid())
            return intersections[index];
        else {
            // "intersections[" + index + "] is invalid"
            return hatchedArea;
        }
    }
}
