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
package com.raytheon.viz.radar.rsc.graphic;

import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.awt.image.RenderedImage;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.measure.UnitConverter;

import org.eclipse.swt.graphics.RGB;
import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.referencing.GeodeticCalculator;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.LineString;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.radar.RadarDataPoint;
import com.raytheon.uf.common.dataplugin.radar.RadarDataPoint.RadarProductType;
import com.raytheon.uf.common.dataplugin.radar.level3.DMDPacket.DMDAttributeIDs;
import com.raytheon.uf.common.dataplugin.radar.level3.GFMPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.HdaHailPacket.HdaHailPoint;
import com.raytheon.uf.common.dataplugin.radar.level3.LinkedVector;
import com.raytheon.uf.common.dataplugin.radar.level3.MBAPacket.MBAAttributeIDs;
import com.raytheon.uf.common.dataplugin.radar.level3.MesocyclonePacket.MesocyclonePoint;
import com.raytheon.uf.common.dataplugin.radar.level3.SCITDataPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.SCITDataPacket.SCITDataCell;
import com.raytheon.uf.common.dataplugin.radar.level3.SpecialGraphicSymbolPacket.SpecialGraphicPoint;
import com.raytheon.uf.common.dataplugin.radar.level3.StormIDPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.StormIDPacket.StormIDPoint;
import com.raytheon.uf.common.dataplugin.radar.level3.SymbologyPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.SymbologyPoint;
import com.raytheon.uf.common.dataplugin.radar.level3.TVSPacket.TVSPoint;
import com.raytheon.uf.common.dataplugin.radar.level3.TextSymbolPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.UnlinkedContourVectorPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.UnlinkedVector;
import com.raytheon.uf.common.dataplugin.radar.level3.UnlinkedVectorPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.generic.AreaComponent;
import com.raytheon.uf.common.dataplugin.radar.level3.generic.GenericDataComponent;
import com.raytheon.uf.common.dataplugin.radar.util.RadarRecordUtil;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.geospatial.ReferencedGeometry;
import com.raytheon.uf.common.geospatial.ReferencedObject.Type;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.DrawableCircle;
import com.raytheon.uf.viz.core.DrawableLine;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.data.IRenderedImageCallback;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.JTSCompiler;
import com.raytheon.uf.viz.core.drawables.JTSCompiler.JTSGeometryData;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ext.ICanvasRenderingExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.point.display.PointWindDisplay;
import com.raytheon.uf.viz.core.point.display.PointWindDisplay.DisplayType;
import com.raytheon.uf.viz.core.point.drawables.ext.IPointImageExtension;
import com.raytheon.uf.viz.core.point.drawables.ext.IPointImageExtension.PointImage;
import com.raytheon.viz.radar.RadarHelper;
import com.raytheon.viz.radar.rsc.graphic.RadarGraphicFunctions.MesocycloneType;
import com.raytheon.viz.radar.rsc.graphic.RadarGraphicFunctions.PlotObject;
import com.raytheon.viz.radar.ui.RadarDisplayControls;
import com.raytheon.viz.radar.ui.RadarDisplayManager;
import com.raytheon.viz.radar.ui.RadarDisplayManager.TrackTypes;
import com.raytheon.viz.radar.util.DmdModifier;

import si.uom.SI;
import systems.uom.common.USCustomary;

/**
 * {@link IRenderable} for displaying a single "page" of radar symbology data.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jan 08, 2009           chammack    Initial creation
 * Mar 05, 2013  DCS51    zwang       Handle GFM product
 * Jun 24, 2013  16162    zwang       Remove "wind behind"
 * Nov 20, 2013  2488     randerso    Removed use of VeraMono font file
 * Jun 04, 2014  3232     bsteffen    Cleanup.
 * Aug 11, 2014  3504     mapeters    Replaced deprecated IODataPreparer
 *                                    instances with IRenderedImageCallback.
 * Sep 03, 2014  3574     njensen     Properly dispose objects
 * Nov 06, 2014  16776    zwang       Handle AMDA product MBA
 * Feb 08, 2016  5318     randerso    Font should not be preference based because the
 *                                    size must match the table outline
 * Feb 16, 2016  12021    wkwock      Fix mesocyclone zooming issue
 * Mar 08, 2016  5318     randerso    Fix hard coded scaling factors
 * Aug 31, 2016  2671     tgurney     Factor out some logic to RadarRecordUtil
 * Sep 14, 2016  3241     bsteffen    Update deprecated JTSCompiler method calls
 *
 * </pre>
 *
 * @author chammack
 */
public class RadarGraphicsPage implements IRenderable {
    private static final int ORIGINAL_FONT_HEIGHT = 10;

    private static final int ORIGINAL_FONT_WIDTH = 7;

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RadarGraphicsPage.class);

    public static enum CoordinateSystem {
        LOCAL, SCREEN
    }

    private IDescriptor descriptor;

    private GeneralGridGeometry gridGeometry;

    private GeometryFactory geomFactory;

    private JTSCompiler compiler;

    private IWireframeShape wireframeShape;

    private IWireframeShape gfmFcstWireframeShape;

    /** Map of ascii strings in local coordinate system */
    private Map<Coordinate, String> localStringMap;

    /** Map of ascii strings in screen coordinate system */
    private Map<Coordinate, String> screenStringMap;

    private List<Geometry> screenGeometries;

    private RGB color;

    private double magnification = 1.0;

    private IWireframeShape screenWireframeShape;

    private IFont font;

    private IGraphicsTarget target;

    private List<PlotObject> plotObjects;

    public int page;

    private static int stiStormsDisplayed;

    // The default x position of the table for Generic Data
    private int tableX = 0;

    // The default y position of the table for Generic Data
    private int tableY = 2 * ORIGINAL_FONT_HEIGHT;

    // The x position of the table for Generic Data
    private int startTableX = 0;

    // The y position of the table for Generic Data
    private int startTableY = 2 * ORIGINAL_FONT_HEIGHT;

    // User preferences for table positioning.
    private DmdModifier tableModifier;

    private boolean drawBorder = false;

    private int recordsPerPage = 5;

    private static final int MIN_RADIUS_IN_PIXEL = 7;

    private static final int SPIKE_SIZE = 8;

    // for GFM product, add gfmFcstWireframeShape
    public RadarGraphicsPage(IDescriptor descriptor, GeneralGridGeometry gg,
            IWireframeShape shape, IWireframeShape gfmShape,
            IGraphicsTarget target, RGB color) {
        this(descriptor, gg, shape, target, color);
        this.gfmFcstWireframeShape = gfmShape;
    }

    public RadarGraphicsPage(IDescriptor descriptor, GeneralGridGeometry gg,
            IWireframeShape shape, IGraphicsTarget target, RGB color,
            DmdModifier tableModifier) {
        this(descriptor, gg, shape, target, color);
        if (tableModifier != null) {
            setTablePreferences(tableModifier);
        }
    }

    public RadarGraphicsPage(IDescriptor descriptor, GeneralGridGeometry gg,
            IWireframeShape shape, IGraphicsTarget target, RGB color) {
        this.descriptor = descriptor;
        this.gridGeometry = gg;
        this.geomFactory = new GeometryFactory();
        this.compiler = new JTSCompiler(null, shape, descriptor);
        this.wireframeShape = shape;
        this.color = color;
        this.localStringMap = new HashMap<>();
        this.screenGeometries = new ArrayList<>();
        this.screenStringMap = new HashMap<>();
        this.target = target;
        this.plotObjects = new ArrayList<>();
        stiStormsDisplayed = 0;
    }

    // Display some No data message in the upper left
    public void addNullLegend(String legend) {
        this.screenStringMap.put(new Coordinate(76, 0), legend);
    }

    /**
     * This method should only be used to display the Graphic Alphanumeric (the
     * data table) data
     *
     * @param sp
     * @param cs
     * @throws VizException
     */
    public void addSymbologyPacket(SymbologyPacket sp, CoordinateSystem cs)
            throws VizException {
        List<ReferencedGeometry> localGeometries = new ArrayList<>();
        if (sp instanceof UnlinkedVectorPacket
                || sp instanceof UnlinkedContourVectorPacket) {
            List<UnlinkedVector> vectors = (List<UnlinkedVector>) RadarHelper
                    .getItems(sp);

            for (UnlinkedVector vector : vectors) {
                Coordinate coord1 = null;
                Coordinate coord2 = null;
                if (cs == CoordinateSystem.LOCAL) {
                    coord1 = RadarRecordUtil.rectifyCoordinate(
                            new Coordinate(vector.i1, vector.j1));
                    coord2 = RadarRecordUtil.rectifyCoordinate(
                            new Coordinate(vector.i2, vector.j2));
                } else if (cs == CoordinateSystem.SCREEN) {
                    coord1 = new Coordinate(vector.i1, vector.j1);
                    coord2 = new Coordinate(vector.i2, vector.j2);
                }

                LineString ls = this.geomFactory
                        .createLineString(new Coordinate[] { coord1, coord2 });

                if (cs == CoordinateSystem.LOCAL) {
                    ReferencedGeometry rg = new ReferencedGeometry(ls,
                            this.gridGeometry, Type.GRID_CENTER);
                    localGeometries.add(rg);
                } else if (cs == CoordinateSystem.SCREEN) {
                    this.screenGeometries.add(ls);
                }
            }
        } else if (sp instanceof TextSymbolPacket) {
            TextSymbolPacket tsp = (TextSymbolPacket) sp;
            if (cs == CoordinateSystem.LOCAL) {
                ReferencedCoordinate rc = new ReferencedCoordinate(
                        RadarRecordUtil.rectifyCoordinate(
                                new Coordinate(tsp.getI(), tsp.getJ())),
                        this.gridGeometry, Type.GRID_CENTER);

                Coordinate pixelCoord = null;
                try {
                    pixelCoord = rc.asPixel(this.descriptor.getGridGeometry());
                } catch (Exception e) {
                    throw new VizException("Error transforming coordinate", e);
                }
                this.localStringMap.put(pixelCoord, tsp.getTheText());
            } else if (cs == CoordinateSystem.SCREEN) {
                this.screenStringMap.put(new Coordinate(tsp.getI(), tsp.getJ()),
                        tsp.getTheText());
            }
        }

        JTSGeometryData jtsData = this.compiler.createGeometryData();
        jtsData.setGeometryColor(color);
        for (ReferencedGeometry rg : localGeometries) {
            this.compiler.handle(rg, jtsData);
        }

    }

    /**
     * This is the main method for adding Radar data to the page.
     *
     * @param stormData
     *            The radar data for a specific lat/lon. The RadarRecord is
     *            responsible for grouping all related data based on geographic
     *            location.
     * @param cs
     * @throws VizException
     */
    public void addImages(RadarDataPoint stormData, CoordinateSystem cs)
            throws VizException {
        if (stormData.getDataType().equals(RadarProductType.STI)) {
            if (RadarDisplayManager.getInstance().getCurrentSettings()
                    .getStiNumStorms() > stiStormsDisplayed) {
                addSTIImages(stormData);
                stiStormsDisplayed++;
            }
        } else {
            List<PlotObject> images = new ArrayList<>();
            Map<Coordinate, String> localMap = new HashMap<>();
            Map<Coordinate, String> screenMap = new HashMap<>();
            List<PlotObject> temp;

            boolean displayStormID = true;

            PlotObject pObject;
            // Get an image for all Point based data
            POINT: for (Integer type : stormData.getDisplayPointData()
                    .keySet()) {
                for (SymbologyPoint currPoint : stormData.getDisplayPointData()
                        .get(type).values()) {
                    pObject = null;
                    if (currPoint instanceof HdaHailPoint) {
                        pObject = getImage((HdaHailPoint) currPoint);

                        if (pObject == null) {
                            displayStormID = false;
                        } else {
                            images.add(pObject);
                        }
                    } else if (currPoint instanceof MesocyclonePoint) {
                        MesocyclonePoint mp = (MesocyclonePoint) currPoint;
                        try {
                            pObject = RadarGraphicFunctions.setMesocycloneInfo(
                                    mp.i, mp.j, mp.getMesoCycloneRadius(),
                                    target, this.gridGeometry, this.descriptor,
                                    RadarGraphicFunctions.MesocycloneType.MESOCYCLONE,
                                    color, LineStyle.SOLID);
                        } catch (TransformException e) {
                            statusHandler.error(e.getLocalizedMessage(), e);
                        } catch (FactoryException e) {
                            statusHandler.error(e.getLocalizedMessage(), e);
                        }

                        if (pObject == null) {
                            displayStormID = false;
                        } else {
                            images.add(pObject);
                        }
                    } else if (currPoint instanceof SpecialGraphicPoint) {
                        temp = getImage((SpecialGraphicPoint) currPoint);
                        if (temp != null) {
                            images.addAll(temp);
                        } else {
                            displayStormID = false;
                            break POINT;
                        }
                    } else if (currPoint instanceof StormIDPoint) {
                        getImage((StormIDPoint) currPoint, localMap);
                    } else if (currPoint instanceof TVSPoint) {
                        TVSPoint tvsPoint = (TVSPoint) currPoint;
                        pObject = getImage(tvsPoint);

                        if (pObject == null) {
                            displayStormID = false;
                        } else {
                            images.add(pObject);
                            // Use label to label instead of localMap so the
                            // label sticks to the graphic
                            localMap.clear();
                            pObject.label = stormData.getStormID();
                            break POINT;
                        }
                    }
                }
            }
            boolean stiFound = false;

            // Get an image for all Packet based data
            for (Integer type : stormData.getDisplayPacketData().keySet()) {
                for (SymbologyPacket currPacket : stormData
                        .getDisplayPacketData().get(type).values()) {
                    if (currPacket instanceof TextSymbolPacket) {
                        TextSymbolPacket tsp = (TextSymbolPacket) currPacket;
                        if ("\" ".equals(tsp.getTheText())) {
                            if (RadarDisplayManager.getInstance()
                                    .getCurrentSettings()
                                    .getStiNumStorms() > stiStormsDisplayed) {
                                stiFound = true;
                                addSTIImages(stormData);
                            } else {
                                // The local Strings are handled within
                                // addSTIImages so we filter them correctly
                                localMap.clear();
                            }
                        } else {
                            getImage((TextSymbolPacket) currPacket, cs,
                                    localMap, screenMap);
                        }
                    }
                }
            }
            if (stiFound) {
                stiStormsDisplayed++;

            }

            for (Integer type : stormData.getDisplayGenericPointData()
                    .keySet()) {

                // DMD
                if (type == 149) {
                    // Handle each Feature in the DMD Packet
                    for (GenericDataComponent currComponent : stormData
                            .getDisplayGenericPointData().get(type).values()) {
                        // Handle Graphic portion
                        pObject = getImage(currComponent, 149);

                        if (pObject == null) {
                            displayStormID = false;
                            stormData.setVisible(false);
                        } else {
                            images.add(pObject);
                            stormData.setVisible(true);
                        }
                    }
                }
                // GFM
                else if (type == 140) {
                    List<PlotObject> gfmImages = new ArrayList<>();

                    // Handle each Feature in the GFM Packet
                    for (GenericDataComponent currComponent : stormData
                            .getDisplayGenericPointData().get(type).values()) {
                        // Handle Graphic portion
                        gfmImages = getGfmImage(currComponent);

                        if (gfmImages.isEmpty()) {
                            displayStormID = false;
                            stormData.setVisible(false);
                        } else {
                            images.addAll(gfmImages);
                            stormData.setVisible(true);
                        }
                    }
                }
                // MBA
                else if (type == 196) {
                    // Handle each Feature in the MBA Packet
                    for (GenericDataComponent currComponent : stormData
                            .getDisplayGenericPointData().get(type).values()) {
                        // Handle Graphic portion
                        drawMbaImage(currComponent);
                    }
                }
            }

            // Only display storm id if other information is being displayed
            if (displayStormID) {
                // Add images
                this.plotObjects.addAll(images);

                // Add Text
                this.localStringMap.putAll(localMap);

                this.screenStringMap.putAll(screenMap);
            }
        }
    }

    /**
     * STI point will have a text packet for the current location, unlinked
     * vector for the full path, and SCIT packets for the past and forecast data
     *
     * @param packet
     * @throws VizException
     */
    private void addSTIImages(RadarDataPoint stiPoint) throws VizException {
        List<PlotObject> images = new ArrayList<>();
        Map<Coordinate, String> localMap = new HashMap<>();

        SCITDataPacket forecstData = null;
        SCITDataPacket pastData = null;
        TextSymbolPacket currLocation = null;
        StormIDPoint stormID = null;

        // TextSymbolPacket - Text: "" "
        // StormIDPacket - i/j matches TextSymbolPacket
        // SCITDataPacket - contains past (!)
        // SCITDataPacket - contains forecast (#)

        // Get the past and forecast data
        HashMap<Integer, HashMap<Integer, SymbologyPacket>> displayPacketData = stiPoint
                .getDisplayPacketData();

        // Get the packet data
        for (Integer type : displayPacketData.keySet()) {
            PACKET: for (SymbologyPacket currPkt : displayPacketData.get(type)
                    .values()) {
                if (currPkt instanceof SCITDataPacket) {
                    for (SCITDataCell currCell : ((SCITDataPacket) currPkt)
                            .getPoints()) {
                        if (currCell.getText().contains("!")) {
                            // STI past
                            pastData = (SCITDataPacket) currPkt;
                            continue PACKET;
                        } else if (currCell.getText().contains("#")) {
                            // STI Forecast
                            forecstData = (SCITDataPacket) currPkt;
                            continue PACKET;
                        }
                    }
                } else if (currPkt instanceof TextSymbolPacket) {
                    // check for current location
                    if (((TextSymbolPacket) currPkt).getTheText()
                            .contains("\"")) {
                        currLocation = (TextSymbolPacket) currPkt;
                    }
                } else if (currPkt instanceof StormIDPacket) {
                    for (StormIDPoint stormPt : ((StormIDPacket) currPkt)
                            .getPoints()) {
                        // Should only have one point
                        stormID = stormPt;
                    }
                }
            }
        }

        // Get a hold of the current display settings
        RadarDisplayControls displaySettings = RadarDisplayManager.getInstance()
                .getCurrentSettings();

        // Process current Location
        if (currLocation != null) {
            images.addAll(RadarGraphicFunctions.createSCITDataCell(currLocation,
                    this.target, this.gridGeometry, this.descriptor, color));
        }

        // Process Past
        if (pastData != null) {
            if (displaySettings.getStiTrackType().equals(TrackTypes.PAST)
                    || displaySettings.getStiTrackType()
                            .equals(TrackTypes.PAST_AND_FORECAST)) {
                // Display past locations
                images.addAll(RadarGraphicFunctions.createSCITDataCell(pastData,
                        this.target, this.gridGeometry, this.descriptor,
                        color));

                // Connect the dots
                List<ReferencedGeometry> localGeometries = new ArrayList<>();

                for (SCITDataCell currCell : pastData.getPoints()) {
                    drawVectors(currCell.getVectors(), currLocation,
                            CoordinateSystem.LOCAL, localGeometries);
                }
                JTSGeometryData jtsData = this.compiler.createGeometryData();
                jtsData.setGeometryColor(color);
                for (ReferencedGeometry rg : localGeometries) {
                    this.compiler.handle(rg, jtsData);
                }
            }
        }

        // Process Forecast
        if (forecstData != null) {
            if (displaySettings.getStiTrackType().equals(TrackTypes.FORECAST)
                    || displaySettings.getStiTrackType()
                            .equals(TrackTypes.PAST_AND_FORECAST)) {
                // Display forecast locations
                images.addAll(RadarGraphicFunctions.createSCITDataCell(
                        forecstData, this.target, this.gridGeometry,
                        this.descriptor, color));

                // Connect the dots
                List<ReferencedGeometry> localGeometries = new ArrayList<>();

                for (SCITDataCell currCell : forecstData.getPoints()) {
                    drawVectors(currCell.getVectors(), currLocation,
                            CoordinateSystem.LOCAL, localGeometries);
                }
                JTSGeometryData jtsData = this.compiler.createGeometryData();
                jtsData.setGeometryColor(color);
                for (ReferencedGeometry rg : localGeometries) {
                    this.compiler.handle(rg, jtsData);
                }
            }
        }

        // Process storm id
        if (stormID != null) {
            getImage(stormID, localMap, 5, 5);
        }

        // Add images
        this.plotObjects.addAll(images);

        // Add Text
        this.localStringMap.putAll(localMap);
    }

    private void getImage(StormIDPoint id, Map<Coordinate, String> localMap)
            throws VizException {
        getImage(id, localMap, 5, 5);
    }

    private void getImage(StormIDPoint id, Map<Coordinate, String> localMap,
            int xOffset, int yOffset) throws VizException {
        ReferencedCoordinate rc = new ReferencedCoordinate(
                RadarRecordUtil.rectifyCoordinate(
                        new Coordinate(id.getI(), id.getJ())),
                this.gridGeometry, Type.GRID_CENTER);

        Coordinate pixelCoord = null;
        try {
            pixelCoord = rc.asPixel(this.descriptor.getGridGeometry());
        } catch (Exception e) {
            throw new VizException("Error transforming coordinate", e);
        }

        // Handle offsets
        pixelCoord.x = pixelCoord.x + xOffset;
        pixelCoord.y = pixelCoord.y + yOffset;

        localMap.put(pixelCoord, id.getStormID());
    }

    private void getImage(TextSymbolPacket tsp, CoordinateSystem cs,
            Map<Coordinate, String> localMap, Map<Coordinate, String> screenMap)
                    throws VizException {
        if (cs == CoordinateSystem.LOCAL) {
            ReferencedCoordinate rc = new ReferencedCoordinate(
                    RadarRecordUtil.rectifyCoordinate(
                            new Coordinate(tsp.getI(), tsp.getJ())),
                    this.gridGeometry, Type.GRID_CENTER);

            Coordinate pixelCoord = null;
            try {
                pixelCoord = rc.asPixel(this.descriptor.getGridGeometry());
            } catch (Exception e) {
                throw new VizException("Error transforming coordinate", e);
            }
            localMap.put(pixelCoord, tsp.getTheText());
        } else if (cs == CoordinateSystem.SCREEN) {
            screenMap.put(new Coordinate(tsp.getI(), tsp.getJ()),
                    tsp.getTheText());
        }
    }

    private PlotObject getImage(GenericDataComponent currPt, int type)
            throws VizException {
        PlotObject image = null;

        if (type == 149) {
            // Determine if the feature should be rendered
            RadarDisplayControls currentSettings = RadarDisplayManager
                    .getInstance().getCurrentSettings();

            AreaComponent currFeature = (AreaComponent) currPt;

            String overlaps = currFeature.getValue(
                    DMDAttributeIDs.OVERLAPS_LOWER_FEATURE.toString());

            String rank = currFeature
                    .getValue(DMDAttributeIDs.STRENGTH_RANK.getName());
            String rankType = currFeature
                    .getValue(DMDAttributeIDs.STRENGTH_RANK_TYPE.getName());
            int strengthRank = rank.equals("") ? 0 : Integer.parseInt(rank);
            if (rankType != null) {
                rank += rankType;
            }
            String detectStatus = currFeature
                    .getValue(DMDAttributeIDs.DETECTION_STATUS.toString());
            boolean extrapolated = detectStatus.equalsIgnoreCase("EXT");

            if ((currentSettings.isDmdShowOverlapping()
                    || !overlaps.equalsIgnoreCase("Y"))
                    && strengthRank >= currentSettings
                            .getDmdMinFeatureStrength()
                    && (currentSettings.isDmdMdTvsShowExtrapolated()
                            || !extrapolated)) {
                float i = currFeature.getPoints().get(0).getCoordinate2();
                float j = currFeature.getPoints().get(0).getCoordinate1();

                Coordinate coord = new Coordinate(i, j);

                ReferencedCoordinate rc = new ReferencedCoordinate(coord);

                // Determine the type of symbol:
                // 3d Correlated Shear - Strength Rank < 5
                // Meso - Strength Rank >= 5
                // Meso w/ Spikes - Strength Rank >= 5 && (base on lowest
                // elevation
                // || base height <= 1km)
                MesocycloneType iconType;

                if (strengthRank >= 5) {
                    String onLowestElev = currFeature.getValue(
                            DMDAttributeIDs.BASE_ON_LOWEST_ELEV.toString());
                    String baseHeight = currFeature
                            .getValue(DMDAttributeIDs.BASE_HEIGHT.toString());

                    if (!onLowestElev.equals("")
                            && onLowestElev.equalsIgnoreCase("Y")
                            || !baseHeight.equals("")
                                    && Double.parseDouble(baseHeight) <= 1) {
                        iconType = MesocycloneType.MESOCYCLONE_WITH_SPIKES;
                    } else {
                        iconType = MesocycloneType.MESOCYCLONE;
                    }
                } else if (extrapolated) {
                    iconType = MesocycloneType.CORRELATED_SHEAR_EXTRAPOLATED;
                } else {
                    iconType = MesocycloneType.CORRELATED_SHEAR;
                }

                // Handle STI portion
                List<PlotObject> images = new ArrayList<>();
                int numFcstPos = currFeature.getValueAsInt(
                        DMDAttributeIDs.NUM_FCST_POSITIONS.toString());
                int numPastPos = currFeature.getValueAsInt(
                        DMDAttributeIDs.NUM_PAST_POSITIONS.toString());

                List<Coordinate> coords = new ArrayList<>();

                Coordinate currLoc = new Coordinate(
                        currFeature.getPoints().get(0).getCoordinate2(),
                        currFeature.getPoints().get(0).getCoordinate1());

                float lat;
                float lon;
                float prevLat;
                float prevLon;
                // Forecast Positions
                if (numFcstPos > 0 && (currentSettings.getDmdTrackType()
                        .equals(TrackTypes.FORECAST)
                        || currentSettings.getDmdTrackType()
                                .equals(TrackTypes.PAST_AND_FORECAST))) {
                    String fcstPosLat = currFeature
                            .getValue(DMDAttributeIDs.FCST_LAT.toString());
                    String fcstPosLon = currFeature
                            .getValue(DMDAttributeIDs.FCST_LON.toString());

                    // Parse the lat/lon values
                    String[] latParts = fcstPosLat.split(",");
                    String[] lonParts = fcstPosLon.split(",");

                    for (int k = 0; k < numFcstPos; k++) {
                        lat = Float.parseFloat(latParts[k]);
                        lon = Float.parseFloat(lonParts[k]);

                        Coordinate stiCoord = new Coordinate(lon, lat);

                        ReferencedCoordinate stiRC = new ReferencedCoordinate(
                                stiCoord);

                        images.add(RadarGraphicFunctions.createDMDSTIImage(
                                stiRC, RadarGraphicFunctions.DMD_FCST_SYM,
                                this.target, this.gridGeometry, this.descriptor,
                                color));

                        // Connect the dots
                        if (k != 0) {
                            prevLat = Float.parseFloat(latParts[k - 1]);
                            prevLon = Float.parseFloat(lonParts[k - 1]);

                            this.wireframeShape
                                    .addLineSegment(new Coordinate[] {
                                            new Coordinate(prevLon, prevLat),
                                            new Coordinate(lon, lat) });
                        } else {
                            this.wireframeShape
                                    .addLineSegment(new Coordinate[] { currLoc,
                                            new Coordinate(lon, lat) });
                        }
                    }
                }

                // Past Positions
                if (numPastPos > 0 && (currentSettings.getDmdTrackType()
                        .equals(TrackTypes.PAST)
                        || currentSettings.getDmdTrackType()
                                .equals(TrackTypes.PAST_AND_FORECAST))) {
                    String pastPosLat = currFeature
                            .getValue(DMDAttributeIDs.PAST_LAT.toString());
                    String pastPosLon = currFeature
                            .getValue(DMDAttributeIDs.PAST_LON.toString());

                    // Parse the lat/lon values
                    String[] latParts = pastPosLat.split(",");
                    String[] lonParts = pastPosLon.split(",");

                    coords.clear();
                    for (int k = 0; k < numPastPos; k++) {
                        lat = Float.parseFloat(latParts[k]);
                        lon = Float.parseFloat(lonParts[k]);

                        Coordinate stiCoord = new Coordinate(lon, lat);

                        ReferencedCoordinate stiRC = new ReferencedCoordinate(
                                stiCoord);

                        images.add(RadarGraphicFunctions.createDMDSTIImage(
                                stiRC, RadarGraphicFunctions.DMD_PAST_SYM,
                                this.target, this.gridGeometry, this.descriptor,
                                color));

                        // Connect the dots
                        if (k != 0) {
                            prevLat = Float.parseFloat(latParts[k - 1]);
                            prevLon = Float.parseFloat(lonParts[k - 1]);

                            this.wireframeShape
                                    .addLineSegment(new Coordinate[] {
                                            new Coordinate(prevLon, prevLat),
                                            new Coordinate(lon, lat) });
                        } else {
                            this.wireframeShape
                                    .addLineSegment(new Coordinate[] { currLoc,
                                            new Coordinate(lon, lat) });
                        }
                    }
                }

                // Add images
                this.plotObjects.addAll(images);

                try {
                    double radius = currFeature.getValueAsDouble(
                            DMDAttributeIDs.BASE_DIAMETER.toString()) / 2;
                    LineStyle lineStyle = LineStyle.SOLID;
                    if (currFeature
                            .getValue(
                                    DMDAttributeIDs.DETECTION_STATUS.toString())
                            .equals("EXT")) {
                        lineStyle = LineStyle.DASHED;
                    }

                    image = RadarGraphicFunctions.setMesocycloneInfo(rc, radius,
                            descriptor, iconType, color, lineStyle);
                    double radiusInPixels = RadarGraphicFunctions
                            .getRadiusInPixels(radius, rc.asLatLon().x,
                                    rc.asLatLon().y, descriptor, target);

                    Coordinate coordID = rc
                            .asPixel(this.descriptor.getGridGeometry());
                    coordID.x = coordID.x - radiusInPixels;
                    coordID.y = coordID.y - radiusInPixels;

                    String mesoID = currFeature
                            .getValue(DMDAttributeIDs.MESO_ID.toString());

                    this.localStringMap.put(coordID, mesoID);
                } catch (NumberFormatException e) {
                    statusHandler.error(e.getLocalizedMessage(), e);
                } catch (TransformException e) {
                    statusHandler.error(e.getLocalizedMessage(), e);
                } catch (FactoryException e) {
                    statusHandler.error(e.getLocalizedMessage(), e);
                }
            }
        }

        return image;
    }

    // Handle GFM product
    private List<PlotObject> getGfmImage(GenericDataComponent currPt)
            throws VizException {
        List<PlotObject> images = new ArrayList<>();

        UnitConverter metersPerSecondToKnots = SI.METRE_PER_SECOND
                .getConverterTo(USCustomary.KNOT);

        boolean isFcst = false;
        double x, y;
        Coordinate pos1, pos2;

        int imgSize = 64;

        AreaComponent currFeature = (AreaComponent) currPt;

        int numPoints = currFeature.getPoints().size();
        int numParam = currFeature.getParameters().size();

        String propU, propV, windX, windY;
        double pU = 0.0;
        double pV = 0.0;
        double wX = 0.0;
        double wY = 0.0;

        // if the component only has dectect ID and DeltaT
        if (numParam == 2) {
            isFcst = true;
        }
        // 11 parameters
        else {
            propU = currFeature
                    .getValue(GFMPacket.GFMAttributeIDs.PROPU.getName());
            if (propU != null && propU.length() > 0) {
                pU = metersPerSecondToKnots.convert(Double.parseDouble(propU));
            }
            propV = currFeature
                    .getValue(GFMPacket.GFMAttributeIDs.PROPV.getName());
            if (propV != null && propV.length() > 0) {
                pV = metersPerSecondToKnots.convert(Double.parseDouble(propV));
            }
            windX = currFeature
                    .getValue(GFMPacket.GFMAttributeIDs.WINDBEHINDX.getName());
            if (windX != null && windX.length() > 0) {
                wX = Float.parseFloat(windX);
            }
            windY = currFeature
                    .getValue(GFMPacket.GFMAttributeIDs.WINDBEHINDY.getName());
            if (windY != null && windY.length() > 0) {
                wY = Float.parseFloat(windY);
            }

            // Get the nearest point on the MIGFA front to the wind behind point
            // Plot front movement arrow at this point
            Coordinate windBehind = new Coordinate(wX, wY);
            Coordinate plotPoint = getPlotPoint(currFeature, windBehind);
            wX = plotPoint.x;
            wY = plotPoint.y;

            // Prop wind arrow
            PlotObject poWind = new PlotObject();
            PointWindDisplay barb = new PointWindDisplay(imgSize * 0.4, 0.5, 2,
                    0);
            barb.setImageParameters(imgSize, imgSize, 255, 255, 255, 1);
            barb.setColor(this.color);

            // plot the wind arrow in the same length as 50 kts
            double spd = Math.sqrt(pU * pU + pV * pV);
            if (spd > 0) {
                pU *= 50.0 / spd;
                pV *= 50.0 / spd;
            }

            barb.setWind(pU, pV, false);
            final BufferedImage imgBuf = barb.getWindImage(false,
                    DisplayType.ARROW, 0.2);
            poWind.image = target
                    .initializeRaster(new IRenderedImageCallback() {
                        @Override
                        public RenderedImage getImage() throws VizException {
                            return imgBuf;
                        }
                    });

            ReferencedCoordinate rc = referencedGfmCoord(wX, wY);
            try {
                poWind.coord = rc.asPixel(this.descriptor.getGridGeometry());
                poWind.pixelOffset = new int[] { 0, 0 };
                images.add(poWind);
            } catch (Exception e) {
                throw new VizException("Unable to transform coordinates", e);
            }
        }

        // Draw GFM fronts
        x = currFeature.getPoints().get(0).getCoordinate1();
        y = currFeature.getPoints().get(0).getCoordinate2();
        try {
            pos1 = referencedGfmCoord(x, y).asLatLon();

            for (int k = 1; k < numPoints; k++) {
                x = currFeature.getPoints().get(k).getCoordinate1();
                y = currFeature.getPoints().get(k).getCoordinate2();
                // convert xy to latlon
                pos2 = referencedGfmCoord(x, y).asLatLon();

                // Connect the dots
                if (isFcst) {
                    gfmFcstWireframeShape
                            .addLineSegment(new Coordinate[] { pos1, pos2 });
                } else {
                    wireframeShape
                            .addLineSegment(new Coordinate[] { pos1, pos2 });
                }
                pos1 = pos2;
            }
        } catch (TransformException e) {
            throw new VizException(e);
        } catch (FactoryException e) {
            throw new VizException(e);
        }
        return images;
    }

    // Handle MBA product
    private void drawMbaImage(GenericDataComponent currPt) throws VizException {

        double x, y;
        Coordinate point;

        // Determine if the feature should be rendered
        RadarDisplayControls currentSettings = RadarDisplayManager.getInstance()
                .getCurrentSettings();

        AreaComponent currFeature = (AreaComponent) currPt;
        String cat = currFeature.getValue(MBAAttributeIDs.CATEGORY.getName());
        int catValue = cat.equals("") ? 0 : Integer.parseInt(cat);

        // By default, do not show MBA Wind Shear
        int minCat = 1;
        if (currentSettings.isMbaShowWindShear()) {
            minCat = 0;
        }

        if (catValue >= minCat) {

            int numPoints = currFeature.getPoints().size();
            Coordinate[] points = new Coordinate[numPoints];

            // Draw Microburst cell
            try {
                for (int k = 0; k < numPoints; k++) {
                    x = currFeature.getPoints().get(k).getCoordinate1();
                    y = currFeature.getPoints().get(k).getCoordinate2();
                    // convert xy to latlon
                    point = referencedGfmCoord(x, y).asLatLon();
                    points[k] = point;
                }
                wireframeShape.addLineSegment(points);
            } catch (TransformException e) {
                throw new VizException(e);
            } catch (FactoryException e) {
                throw new VizException(e);
            }
        }
    }

    private PlotObject getImage(HdaHailPoint currPt) throws VizException {
        PlotObject image = null;

        image = RadarGraphicFunctions.createHailImage(currPt, target,
                this.gridGeometry, this.descriptor, color);

        return image;
    }

    private PlotObject getImage(TVSPoint currPt) throws VizException {
        PlotObject image = null;

        image = RadarGraphicFunctions.createTVS(currPt, target,
                this.gridGeometry, this.descriptor, color);

        return image;
    }

    private List<PlotObject> getImage(SpecialGraphicPoint currPt)
            throws VizException {
        List<PlotObject> images = null;

        try {
            switch (currPt.getPointFeatureType()) {
            case 1:
                // Mesocyclone - Extrapolated
                if (RadarDisplayManager.getInstance().getCurrentSettings()
                        .isDmdMdTvsShowExtrapolated()) {

                    images = RadarGraphicFunctions.createMesocycloneImage(
                            currPt, target, this.gridGeometry, this.descriptor,
                            RadarGraphicFunctions.MesocycloneType.MESOCYCLONE,
                            color);

                }
                break;
            case 2:
                // 3D Correlated Shear - Extrapolated
                if (RadarDisplayManager.getInstance().getCurrentSettings()
                        .isDmdMdTvsShowExtrapolated()) {
                    images = RadarGraphicFunctions.createMesocycloneImage(
                            currPt, target, this.gridGeometry, this.descriptor,
                            RadarGraphicFunctions.MesocycloneType.CORRELATED_SHEAR_EXTRAPOLATED,
                            color);
                }
                break;
            case 3:
                // Mesocyclone - Persistent, New, or Increasing
                images = RadarGraphicFunctions.createMesocycloneImage(currPt,
                        target, this.gridGeometry, this.descriptor,
                        RadarGraphicFunctions.MesocycloneType.MESOCYCLONE,
                        color);
                break;
            case 4:
                // 3D Correalated Shear - Persistent, New, or Increasing
                images = RadarGraphicFunctions.createMesocycloneImage(currPt,
                        target, this.gridGeometry, this.descriptor,
                        RadarGraphicFunctions.MesocycloneType.CORRELATED_SHEAR,
                        color);
                break;
            case 5:
                // TVS - Extrapolated
                if (RadarDisplayManager.getInstance().getCurrentSettings()
                        .isDmdMdTvsShowExtrapolated()) {
                    images = RadarGraphicFunctions.createTVS(currPt, false,
                            target, this.gridGeometry, this.descriptor, color,
                            true);
                }
                break;
            case 6:
                // ETVS - Extrapolated
                if (RadarDisplayManager.getInstance().getCurrentSettings()
                        .isDmdMdTvsShowExtrapolated()) {
                    images = RadarGraphicFunctions.createTVS(currPt, true,
                            target, this.gridGeometry, this.descriptor, color,
                            true);
                }
                break;
            case 7:
                // TVS - Persistent, New, or Increasing
                images = RadarGraphicFunctions.createTVS(currPt, false, target,
                        this.gridGeometry, this.descriptor, color, false);
                break;
            case 8:
                // ETVS - Persistent, New, or Increasing
                images = RadarGraphicFunctions.createTVS(currPt, true, target,
                        this.gridGeometry, this.descriptor, color, false);
                break;
            // 9,10,11 - MDA Data
            case 9:
                // Strength >= 5 AND Base Height <= 1km Above Ground Level
                // OR Base on lowest Elevation
                // Display: like Leg. Mesocyclone plus outward spikes
                images = RadarGraphicFunctions.createMesocycloneImage(currPt,
                        target, this.gridGeometry, this.descriptor,
                        RadarGraphicFunctions.MesocycloneType.MESOCYCLONE_WITH_SPIKES,
                        color);
                break;
            case 10:
                // Strength >= 5 AND Base Height > 1km Above Ground Level
                // AND Base NOT on lowest Elevation
                // Display: like Leg. Mesocyclone
                images = RadarGraphicFunctions.createMesocycloneImage(currPt,
                        target, this.gridGeometry, this.descriptor,
                        RadarGraphicFunctions.MesocycloneType.MESOCYCLONE,
                        color);
                break;
            case 11:
                // Strength < 5
                // Display: like a 3D Correlated Shear
                images = RadarGraphicFunctions.createMesocycloneImage(currPt,
                        target, this.gridGeometry, this.descriptor,
                        RadarGraphicFunctions.MesocycloneType.CORRELATED_SHEAR,
                        color);
                break;
            default:
                break;
            }
        } catch (TransformException e) {
            statusHandler.error(e.getLocalizedMessage(), e);
        } catch (FactoryException e) {
            statusHandler.error(e.getLocalizedMessage(), e);
        }

        return images;
    }

    /**
     * @return the magnification
     */
    public double getMagnification() {
        return magnification;
    }

    /**
     * @param magnification
     *            the magnification to set
     */
    public void setMagnification(double magnification) {
        if (this.magnification != magnification && this.font != null) {
            font.setMagnification((float) magnification, false);
        }

        this.magnification = magnification;
    }

    /**
     * @return the color
     */
    public RGB getColor() {
        return color;
    }

    /**
     * @param color
     *            the color to set
     */
    public void setColor(RGB color) {
        this.color = color;
    }

    @Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
        if (font == null) {
            this.font = target.initializeFont("Monospace", 10, null);
            this.font.setMagnification((float) magnification, false);
            this.font.setSmoothing(true);
        }

        // Paint map-relative vectors
        // GFM forecast positions should be dashed thick lines
        if (this.gfmFcstWireframeShape != null) {
            target.drawWireframeShape(this.wireframeShape, this.color, 3.0f);
            target.drawWireframeShape(this.gfmFcstWireframeShape, this.color,
                    3.0f, LineStyle.DASHED);
        } else if (this.wireframeShape != null) {
            target.drawWireframeShape(this.wireframeShape, this.color, 1.0f);
        }

        // Paint map-relative text
        for (Coordinate c : this.localStringMap.keySet()) {
            String str = this.localStringMap.get(c);
            DrawableString string = new DrawableString(str, this.color);
            string.font = this.font;
            string.setCoordinates(c.x, c.y);
            target.drawStrings(string);
        }

        // Paint screen-relative text
        // Find scale factor based on width of screen
        target.clearClippingPlane();

        // Draw a border around the Generic table data
        if (drawBorder) {
            drawTableBorder();
        }

        double yOffset = 24.0;
        double xOffset = 24.0;
        double xScale = 1.0;
        double yScale = 1.0;
        double minx = Double.MAX_VALUE;
        double maxx = Double.MIN_VALUE;
        if (this.screenGeometries != null && !this.screenGeometries.isEmpty()) {
            for (Geometry g : this.screenGeometries) {
                for (Coordinate co : g.getCoordinates()) {
                    minx = Math.min(minx, co.x);
                    maxx = Math.max(maxx, co.x);
                }
            }

            // the target may have messes with our magnification value,
            // especially in smaller panes.
            DrawableString testString = new DrawableString("X", this.color);
            testString.font = this.font;
            Rectangle2D bounds = target.getStringsBounds(testString);
            xScale = bounds.getWidth() / ORIGINAL_FONT_WIDTH;
            yScale = bounds.getHeight() / ORIGINAL_FONT_HEIGHT;
            double width = (maxx - minx) * xScale;
            // If the table wider than our 90 % of canvas width then shrink it
            double maxWidth = paintProps.getCanvasBounds().width * 0.9;
            if (width > maxWidth) {
                double shrink = maxWidth / width;
                width *= shrink;
                yScale *= shrink;
                xScale *= shrink;
                font.setMagnification((float) (magnification * shrink), false);
            }
            xOffset = (paintProps.getCanvasBounds().width - width) / 2;
            List<DrawableLine> lines = new ArrayList<>();
            for (Geometry g : this.screenGeometries) {
                Coordinate[] coords = g.getCoordinates();

                // offsets to make the table fit the screen
                double x1 = (coords[0].x - minx) * xScale + xOffset;
                double y1 = (coords[0].y + 0.25) * yScale + yOffset;
                double x2 = (coords[1].x - minx) * xScale + xOffset;
                double y2 = (coords[1].y + 0.25) * yScale + yOffset;
                DrawableLine line = new DrawableLine();
                line.addPoint(x1, y1);
                line.addPoint(x2, y2);
                line.basics.color = this.color;
                lines.add(line);
            }
            target.getExtension(ICanvasRenderingExtension.class)
                    .drawLines(paintProps, lines.toArray(new DrawableLine[0]));
        }

        // Only paint data table text if no configuration is specified or
        // if configuration says to show table.
        if (tableModifier == null
                || tableModifier != null && tableModifier.isShowTable()) {
            // Paint the table data text.
            if (this.screenStringMap != null
                    && !this.screenStringMap.isEmpty()) {
                for (Coordinate c : this.screenStringMap.keySet()) {
                    String str = this.screenStringMap.get(c);

                    double x = (c.x - minx) * xScale + xOffset;
                    double y = c.y * yScale + yOffset;
                    DrawableString string = new DrawableString(str, this.color);
                    string.font = this.font;
                    string.setCoordinates(x, y);
                    string.verticallAlignment = VerticalAlignment.TOP;
                    target.getExtension(ICanvasRenderingExtension.class)
                            .drawStrings(paintProps, string);
                }

            }
            if (xScale != this.magnification) {
                font.setMagnification((float) this.magnification, false);
            }
        }

        target.setupClippingPlane(paintProps.getClippingPane());

        // paint symbols on screen
        double ratio = paintProps.getView().getExtent().getWidth()
                / paintProps.getCanvasBounds().width;
        double pixels = 90 * magnification;

        for (PlotObject po : this.plotObjects) {
            Coordinate adjustedCoord = (Coordinate) po.coord.clone();

            adjustedCoord.x += po.pixelOffset[0] * ratio * magnification;
            adjustedCoord.y += po.pixelOffset[1] * ratio * magnification;
            PointImage image = new PointImage(po.image, adjustedCoord);
            image.setHeight(pixels);
            image.setWidth(pixels);

            if (po.label != null) {
                image.setSiteId(po.label);
                // Place the label next to the image
                adjustedCoord.x += pixels / 20;
                DrawableString string = new DrawableString(po.label,
                        this.color);
                string.font = this.font;
                string.setCoordinates(adjustedCoord.x, adjustedCoord.y);
                target.drawStrings(string);

            }

            if (po.image != null) {
                target.getExtension(IPointImageExtension.class)
                        .drawPointImages(paintProps, image);
            } else {
                drawMesocyclone(target, paintProps, po);
            }
        }
    }

    /**
     * Convert the radius from kilometer to radius relative to pixel
     *
     * @param lat
     *            The latitude in decimal degrees between -90 and +90°
     * @param lon
     *            The longitude in decimal degrees between -180 and +180°
     * @param radius
     *            in kilometer
     * @return radius relative to pixel
     */
    private double convertRadius(double lat, double lon, double radius) {
        GeodeticCalculator gc = new GeodeticCalculator(descriptor.getCRS());
        gc.setStartingGeographicPoint(lat, lon);
        gc.setDirection(90, radius * 1000); // convert to meter

        double[] p1 = descriptor.worldToPixel(
                new double[] { gc.getStartingGeographicPoint().getX(),
                        gc.getStartingGeographicPoint().getY() });
        double[] p2 = descriptor.worldToPixel(
                new double[] { gc.getDestinationGeographicPoint().getX(),
                        gc.getDestinationGeographicPoint().getY() });
        return Math.abs(p1[0] - p2[0]);
    }

    /**
     * Draw mesocyclone symbols
     *
     * @param target
     * @param paintProps
     * @param po
     * @throws VizException
     */
    private void drawMesocyclone(IGraphicsTarget target,
            PaintProperties paintProps, PlotObject po) throws VizException {
        float width = 4;
        if (po.type.equals(MesocycloneType.CORRELATED_SHEAR) || po.type
                .equals(MesocycloneType.CORRELATED_SHEAR_EXTRAPOLATED)) {
            width = 1;
        }

        double radius = Math.abs(po.mesoRadius);
        if (radius == 0) {// In case, it's 0 for line
                          // radius=MIN_RADIUS_IN_PIXEL/radiusInPixel*radius;
            radius = 0.01;
        }

        radius = convertRadius(po.lat, po.lon, radius);

        double[] centerXy = this.descriptor.getRenderableDisplay().gridToScreen(
                new double[] { po.coord.x, po.coord.y, 0.0 }, target);
        double[] rightXy = this.descriptor.getRenderableDisplay().gridToScreen(
                new double[] { po.coord.x + radius, po.coord.y, 0.0 }, target);
        double radiusInPixel = rightXy[0] - centerXy[0];

        if (radiusInPixel < MIN_RADIUS_IN_PIXEL) {
            radius = MIN_RADIUS_IN_PIXEL / radiusInPixel * radius;
            radiusInPixel = MIN_RADIUS_IN_PIXEL;
        }

        DrawableCircle circle = new DrawableCircle();
        circle = new DrawableCircle();
        circle.setCoordinates(po.coord.x, po.coord.y);
        circle.radius = radius;
        circle.basics.color = po.color;
        circle.lineWidth = width;
        circle.lineStyle = po.lineStyle;
        target.drawCircle(circle);

        if (po.type.equals(MesocycloneType.MESOCYCLONE_WITH_SPIKES)) {
            List<DrawableLine> lines = new ArrayList<>();

            DrawableLine topSpike = new DrawableLine();
            topSpike.addPoint(centerXy[0], centerXy[1] - radiusInPixel);
            topSpike.addPoint(centerXy[0],
                    centerXy[1] - radiusInPixel - SPIKE_SIZE);
            topSpike.basics.color = po.color;
            topSpike.width = width;
            lines.add(topSpike);

            DrawableLine bottomSpike = new DrawableLine();
            bottomSpike.addPoint(centerXy[0], centerXy[1] + radiusInPixel);
            bottomSpike.addPoint(centerXy[0],
                    centerXy[1] + radiusInPixel + SPIKE_SIZE);
            bottomSpike.basics.color = po.color;
            bottomSpike.width = width;
            lines.add(bottomSpike);

            DrawableLine leftSpike = new DrawableLine();
            leftSpike.addPoint(centerXy[0] - radiusInPixel, centerXy[1]);
            leftSpike.addPoint(centerXy[0] - radiusInPixel - SPIKE_SIZE,
                    centerXy[1]);
            leftSpike.basics.color = po.color;
            leftSpike.width = width;
            lines.add(leftSpike);

            DrawableLine rightSpike = new DrawableLine();
            rightSpike.addPoint(centerXy[0] + radiusInPixel, centerXy[1]);
            rightSpike.addPoint(centerXy[0] + radiusInPixel + SPIKE_SIZE,
                    centerXy[1]);
            rightSpike.basics.color = po.color;
            rightSpike.width = width;
            lines.add(rightSpike);
            target.getExtension(ICanvasRenderingExtension.class)
                    .drawLines(paintProps, lines.toArray(new DrawableLine[0]));

        }
    }

    /**
     * Draws the table border for the tabular data that doesn't come in via the
     * Graphic Alphanumeric Block (i.e. DMD).
     */
    private void drawTableBorder() {
        this.drawBorder = false;

        int[] columns = new int[] { 0, 42, 84, 165, 231, 291, 336, 388, 448,
                511, 574, 636 };

        int numberOfRows = this.screenStringMap.size();

        int tableLeft = startTableX - 3;
        int tableTop = startTableY - 1;

        // this.screenGeometries.clear();
        List<Coordinate> coords = new ArrayList<>();
        LineString ls = null;

        int tableRight = tableLeft + columns[columns.length - 1];

        int tableBottom = ORIGINAL_FONT_HEIGHT * numberOfRows + tableTop;

        // Rows
        for (int i = tableTop; i <= tableBottom; i += ORIGINAL_FONT_HEIGHT) {
            coords.clear();
            coords.add(new Coordinate(tableLeft, i));
            coords.add(new Coordinate(tableRight, i));
            ls = this.geomFactory.createLineString(
                    coords.toArray(new Coordinate[coords.size()]));
            this.screenGeometries.add(ls);
        }

        // Columns
        for (int x : columns) {
            coords.clear();
            coords.add(new Coordinate(tableLeft + x, tableTop));
            coords.add(new Coordinate(tableLeft + x, tableBottom));
            ls = this.geomFactory.createLineString(
                    coords.toArray(new Coordinate[coords.size()]));
            this.screenGeometries.add(ls);
        }
    }

    /**
     * Draw vectors on the screen
     *
     * @param vectors
     * @param sp
     * @param cs
     * @param localGeometries
     */
    public void drawVectors(List<LinkedVector> vectors,
            TextSymbolPacket currStormLocation, CoordinateSystem cs,
            List<ReferencedGeometry> localGeometries) {
        List<Coordinate> coords = new ArrayList<>();
        boolean addFirst = true;
        for (LinkedVector vector : vectors) {
            Coordinate coord1 = null;
            Coordinate coord2 = null;
            if (cs == CoordinateSystem.LOCAL) {
                double i = 0;
                double j = 0;
                if (addFirst == true) {
                    i = currStormLocation.getI();
                    j = currStormLocation.getJ();
                    addFirst = false;
                } else {
                    i = vector.i1;
                    j = vector.j1;
                }
                coord1 = RadarRecordUtil
                        .rectifyCoordinate(new Coordinate(i, j));
                coord2 = RadarRecordUtil.rectifyCoordinate(
                        new Coordinate(vector.i2, vector.j2));
            } else if (cs == CoordinateSystem.SCREEN) {
                coord1 = new Coordinate(vector.i1, vector.j1);
                coord2 = new Coordinate(vector.i2, vector.j2);
            }
            coords.add(coord1);
            coords.add(coord2);
        }

        LineString ls = this.geomFactory.createLineString(
                coords.toArray(new Coordinate[coords.size()]));
        if (cs == CoordinateSystem.LOCAL) {
            ReferencedGeometry rg = new ReferencedGeometry(ls,
                    this.gridGeometry, Type.GRID_CENTER);
            localGeometries.add(rg);
        } else if (cs == CoordinateSystem.SCREEN) {
            this.screenGeometries.add(ls);
        }
    }

    public void dispose() {
        if (this.screenWireframeShape != null) {
            this.screenWireframeShape.dispose();
            this.screenWireframeShape = null;
        }

        if (this.wireframeShape != null) {
            this.wireframeShape.dispose();
            this.wireframeShape = null;
        }

        if (this.gfmFcstWireframeShape != null) {
            this.gfmFcstWireframeShape.dispose();
            this.gfmFcstWireframeShape = null;
        }

        if (plotObjects != null) {
            for (PlotObject po : plotObjects) {
                if (po != null && po.image != null) {
                    po.image.dispose();
                    po.image = null;
                }
            }
            plotObjects.clear();
        }

        if (this.font != null) {
            this.font.dispose();
        }
    }

    public void addTableRow(String headings) {
        this.screenStringMap.put(new Coordinate(tableX, tableY), headings);
        tableY += ORIGINAL_FONT_HEIGHT;
    }

    public void drawTableBorder(boolean draw) {
        this.drawBorder = draw;
    }

    public int getRecordsPerPage() {
        return this.recordsPerPage;
    }

    /**
     * Sets the DMD table preferences from XML configuration.
     */
    private void setTablePreferences(DmdModifier tableModifier) {

        this.tableModifier = tableModifier;
        this.startTableX = this.tableModifier.getTablePosition().getX();
        this.startTableY = this.tableModifier.getTablePosition().getY();
        this.tableX = this.startTableX;
        this.tableY = this.startTableY;
    }

    /**
     * Need to convert x/y to lon/lat for GFM product
     */
    public ReferencedCoordinate referencedGfmCoord(double i, double j) {
        return new ReferencedCoordinate(
                RadarRecordUtil.rectifyCoordinate(new Coordinate(i * 4, j * 4)),
                this.gridGeometry, Type.GRID_CENTER);
    }

    /**
     * Gets the nearest point from GFM front to wind behind point to plot front
     * movement arrow
     *
     * @param AreaComponent
     * @param Coordinate
     * @return Coordinate
     *
     */
    private Coordinate getPlotPoint(AreaComponent currFeature,
            Coordinate windBehind) {
        Coordinate point = new Coordinate();
        double minDist = Double.MAX_VALUE;

        int numPoints = currFeature.getPoints().size();
        double x1 = windBehind.x;
        double y1 = windBehind.y;
        double x2 = 0.0;
        double y2 = 0.0;
        double dist = 0.0;

        for (int k = 0; k < numPoints; k++) {
            x2 = currFeature.getPoints().get(k).getCoordinate1();
            y2 = currFeature.getPoints().get(k).getCoordinate2();
            dist = (x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1);
            if (dist < minDist) {
                point.x = x2;
                point.y = y2;
                minDist = dist;
            }
        }

        return point;
    }

}
