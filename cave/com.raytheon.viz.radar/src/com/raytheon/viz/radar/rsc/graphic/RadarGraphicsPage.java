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

import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import org.eclipse.swt.graphics.RGB;
import org.geotools.coverage.grid.GeneralGridGeometry;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.radar.RadarDataPoint;
import com.raytheon.uf.common.dataplugin.radar.RadarDataPoint.RadarProductType;
import com.raytheon.uf.common.dataplugin.radar.level3.CellTrendDataPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.CellTrendVolumeScanPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.CorrelatedShearPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.DMDPacket.DMDAttributeIDs;
import com.raytheon.uf.common.dataplugin.radar.level3.ETVSPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.GFMPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.HailPositivePacket;
import com.raytheon.uf.common.dataplugin.radar.level3.HailProbablePacket;
import com.raytheon.uf.common.dataplugin.radar.level3.HdaHailPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.HdaHailPacket.HdaHailPoint;
import com.raytheon.uf.common.dataplugin.radar.level3.LinkedContourVectorPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.LinkedVector;
import com.raytheon.uf.common.dataplugin.radar.level3.LinkedVectorPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.MesocyclonePacket;
import com.raytheon.uf.common.dataplugin.radar.level3.MesocyclonePacket.MesocyclonePoint;
import com.raytheon.uf.common.dataplugin.radar.level3.PrecipDataPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.SCITDataPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.SCITDataPacket.SCITDataCell;
import com.raytheon.uf.common.dataplugin.radar.level3.STICirclePacket;
import com.raytheon.uf.common.dataplugin.radar.level3.SpecialGraphicSymbolPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.SpecialGraphicSymbolPacket.SpecialGraphicPoint;
import com.raytheon.uf.common.dataplugin.radar.level3.StormIDPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.StormIDPacket.StormIDPoint;
import com.raytheon.uf.common.dataplugin.radar.level3.SuperObWindDataPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.SymbologyPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.SymbologyPoint;
import com.raytheon.uf.common.dataplugin.radar.level3.TVSPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.TVSPacket.TVSPoint;
import com.raytheon.uf.common.dataplugin.radar.level3.TextSymbolPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.UnlinkedContourVectorPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.UnlinkedVector;
import com.raytheon.uf.common.dataplugin.radar.level3.UnlinkedVectorPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.VectorArrowPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.WindBarbPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.WindBarbPacket.WindBarbPoint;
import com.raytheon.uf.common.dataplugin.radar.level3.generic.AreaComponent;
import com.raytheon.uf.common.dataplugin.radar.level3.generic.GenericDataComponent;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.geospatial.ReferencedGeometry;
import com.raytheon.uf.common.geospatial.ReferencedObject.Type;
import com.raytheon.uf.viz.core.DrawableLine;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.data.prep.IODataPreparer;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ext.ICanvasRenderingExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.rsc.jts.JTSCompiler;
import com.raytheon.viz.pointdata.PointWindDisplay;
import com.raytheon.viz.pointdata.PointWindDisplay.DisplayType;
import com.raytheon.viz.pointdata.drawables.IPointImageExtension;
import com.raytheon.viz.pointdata.drawables.IPointImageExtension.PointImage;
import com.raytheon.viz.radar.RadarHelper;
import com.raytheon.viz.radar.rsc.graphic.RadarGraphicFunctions.MesocycloneType;
import com.raytheon.viz.radar.rsc.graphic.RadarGraphicFunctions.PlotObject;
import com.raytheon.viz.radar.ui.RadarDisplayControls;
import com.raytheon.viz.radar.ui.RadarDisplayManager;
import com.raytheon.viz.radar.ui.RadarDisplayManager.TrackTypes;
import com.raytheon.viz.radar.util.DmdModifier;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 8, 2009            chammack     Initial creation
 * 03/05/2013   DCS51     zwang        Handle GFM product 
 * 06/24/2013   DR16162   zwang        Remove "wind behind"
 * 11/20/2013   2488      randerso     Removed use of VeraMono font file
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class RadarGraphicsPage implements IRenderable {

    public static enum CoordinateSystem {
        LOCAL, SCREEN
    };

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
    private int tableY = 20;

    // The x position of the table for Generic Data
    private int startTableX = 0;

    // The y position of the table for Generic Data
    private int startTableY = 20;

    private int tableRowSpacing = 10;

    // User preferences for table positioning.
    private DmdModifier tableModifier;

    private boolean drawBorder = false;

    private int recordsPerPage = 5;

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
        this.localStringMap = new HashMap<Coordinate, String>();
        this.screenGeometries = new ArrayList<Geometry>();
        this.screenStringMap = new HashMap<Coordinate, String>();
        this.target = target;
        this.plotObjects = new ArrayList<PlotObject>();
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
        List<ReferencedGeometry> localGeometries = new ArrayList<ReferencedGeometry>();
        // System.out.println("Page : " + page + " Current Packet: "
        // + sp.getClass());
        if (sp instanceof UnlinkedVectorPacket
                || sp instanceof UnlinkedContourVectorPacket) {
            List<UnlinkedVector> vectors = (List<UnlinkedVector>) RadarHelper
                    .getItems(sp);

            for (UnlinkedVector vector : vectors) {
                Coordinate coord1 = null;
                Coordinate coord2 = null;
                if (cs == CoordinateSystem.LOCAL) {
                    coord1 = rectifyCoordinate(new Coordinate(vector.i1,
                            vector.j1));
                    coord2 = rectifyCoordinate(new Coordinate(vector.i2,
                            vector.j2));
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
                        rectifyCoordinate(new Coordinate(tsp.getI(), tsp.getJ())),
                        this.gridGeometry, Type.GRID_CENTER);

                Coordinate pixelCoord = null;
                try {
                    pixelCoord = rc.asPixel(this.descriptor.getGridGeometry());
                } catch (Exception e) {
                    throw new VizException("Error transforming coordinate", e);
                }
                this.localStringMap.put(pixelCoord, tsp.getTheText());
            } else if (cs == CoordinateSystem.SCREEN) {
                this.screenStringMap.put(
                        new Coordinate(tsp.getI(), tsp.getJ()),
                        tsp.getTheText());
            }
        }

        for (ReferencedGeometry rg : localGeometries) {
            this.compiler.handle(rg, color);
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
            List<PlotObject> images = new ArrayList<PlotObject>();
            Map<Coordinate, String> localMap = new HashMap<Coordinate, String>();
            Map<Coordinate, String> screenMap = new HashMap<Coordinate, String>();
            List<PlotObject> temp;

            boolean displayStormID = true;

            PlotObject pObject;
            // Get an image for all Point based data
            POINT: for (Integer type : stormData.getDisplayPointData().keySet()) {
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
                            pObject = RadarGraphicFunctions
                                    .createMesocycloneImage(
                                            mp.i,
                                            mp.j,
                                            mp.getMesoCycloneRadius(),
                                            target,
                                            this.gridGeometry,
                                            this.descriptor,
                                            RadarGraphicFunctions.MesocycloneType.MESOCYCLONE,
                                            color);
                        } catch (TransformException e) {
                            e.printStackTrace();
                        } catch (FactoryException e) {
                            e.printStackTrace();
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
                    } else if (currPoint instanceof WindBarbPoint) {

                    } else {
                        System.out.println("Need to display point: "
                                + currPoint.getClass());
                    }
                }
            }
            boolean stiFound = false;

            // Get an image for all Packet based data
            for (Integer type : stormData.getDisplayPacketData().keySet()) {
                for (SymbologyPacket currPacket : stormData
                        .getDisplayPacketData().get(type).values()) {
                    if (currPacket instanceof CellTrendDataPacket) {
                    } else if (currPacket instanceof CellTrendVolumeScanPacket) {
                    } else if (currPacket instanceof HailProbablePacket) {
                    } else if (currPacket instanceof HailPositivePacket) {
                    } else if (currPacket instanceof HdaHailPacket) {
                    } else if (currPacket instanceof LinkedVectorPacket
                            || currPacket instanceof LinkedContourVectorPacket) {
                    } else if (currPacket instanceof CorrelatedShearPacket) {
                    } else if (currPacket instanceof MesocyclonePacket) {
                    } else if (currPacket instanceof PrecipDataPacket) {
                    } else if (currPacket instanceof SCITDataPacket) {
                    } else if (currPacket instanceof SpecialGraphicSymbolPacket) {
                    } else if (currPacket instanceof STICirclePacket) {
                    } else if (currPacket instanceof StormIDPacket) {
                    } else if (currPacket instanceof SuperObWindDataPacket) {
                    } else if (currPacket instanceof TextSymbolPacket) {
                        TextSymbolPacket tsp = (TextSymbolPacket) currPacket;
                        if ("\" ".equals(tsp.getTheText())) {
                            if (RadarDisplayManager.getInstance()
                                    .getCurrentSettings().getStiNumStorms() > stiStormsDisplayed) {
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
                    } else if (currPacket instanceof ETVSPacket) {
                    } else if (currPacket instanceof TVSPacket) {
                    } else if (currPacket instanceof UnlinkedVectorPacket
                            || currPacket instanceof UnlinkedContourVectorPacket) {
                    } else if (currPacket instanceof VectorArrowPacket) {
                    } else if (currPacket instanceof WindBarbPacket) {
                    } else {
                        System.out.println("Need to display packet: "
                                + currPacket.getClass());
                    }
                }
            }
            if (stiFound) {
                stiStormsDisplayed++;

            }

            for (Integer type : stormData.getDisplayGenericPointData().keySet()) {

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
                    List<PlotObject> gfmImages = new ArrayList<PlotObject>();

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
        List<PlotObject> images = new ArrayList<PlotObject>();
        Map<Coordinate, String> localMap = new HashMap<Coordinate, String>();

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
        RadarDisplayControls displaySettings = RadarDisplayManager
                .getInstance().getCurrentSettings();

        // Process current Location
        if (currLocation != null) {
            images.addAll(RadarGraphicFunctions.createSCITDataCell(
                    currLocation, this.target, this.gridGeometry,
                    this.descriptor, color));
        }

        // Process Past
        if (pastData != null) {
            if (displaySettings.getStiTrackType().equals(TrackTypes.PAST)
                    || displaySettings.getStiTrackType().equals(
                            TrackTypes.PAST_AND_FORECAST)) {
                // Display past locations
                images.addAll(RadarGraphicFunctions.createSCITDataCell(
                        pastData, this.target, this.gridGeometry,
                        this.descriptor, color));

                // Connect the dots
                List<ReferencedGeometry> localGeometries = new ArrayList<ReferencedGeometry>();

                for (SCITDataCell currCell : pastData.getPoints()) {
                    drawVectors(currCell.getVectors(), currLocation,
                            CoordinateSystem.LOCAL, localGeometries);
                }

                for (ReferencedGeometry rg : localGeometries) {
                    this.compiler.handle(rg, color);
                }
            }
        }

        // Process Forecast
        if (forecstData != null) {
            if (displaySettings.getStiTrackType().equals(TrackTypes.FORECAST)
                    || displaySettings.getStiTrackType().equals(
                            TrackTypes.PAST_AND_FORECAST)) {
                // Display forecast locations
                images.addAll(RadarGraphicFunctions.createSCITDataCell(
                        forecstData, this.target, this.gridGeometry,
                        this.descriptor, color));

                // Connect the dots
                List<ReferencedGeometry> localGeometries = new ArrayList<ReferencedGeometry>();

                for (SCITDataCell currCell : forecstData.getPoints()) {
                    drawVectors(currCell.getVectors(), currLocation,
                            CoordinateSystem.LOCAL, localGeometries);
                }

                for (ReferencedGeometry rg : localGeometries) {
                    this.compiler.handle(rg, color);
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
                rectifyCoordinate(new Coordinate(id.getI(), id.getJ())),
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
                    rectifyCoordinate(new Coordinate(tsp.getI(), tsp.getJ())),
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

            String overlaps = currFeature
                    .getValue(DMDAttributeIDs.OVERLAPS_LOWER_FEATURE.toString());

            String rank = currFeature.getValue(DMDAttributeIDs.STRENGTH_RANK
                    .getName());
            String rankType = currFeature
                    .getValue(DMDAttributeIDs.STRENGTH_RANK_TYPE.getName());
            int strengthRank = rank.equals("") ? 0 : Integer.parseInt(rank);
            if (rankType != null) {
                rank += rankType;
            }
            String detectStatus = currFeature
                    .getValue(DMDAttributeIDs.DETECTION_STATUS.toString());
            boolean extrapolated = detectStatus.equalsIgnoreCase("EXT");

            if ((currentSettings.isDmdShowOverlapping() || !overlaps
                    .equalsIgnoreCase("Y"))
                    && (strengthRank >= currentSettings
                            .getDmdMinFeatureStrength())
                    && (currentSettings.isDmdMdTvsShowExtrapolated() || !extrapolated)) {
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
                    String onLowestElev = currFeature
                            .getValue(DMDAttributeIDs.BASE_ON_LOWEST_ELEV
                                    .toString());
                    String baseHeight = currFeature
                            .getValue(DMDAttributeIDs.BASE_HEIGHT.toString());

                    if ((!onLowestElev.equals("") && onLowestElev
                            .equalsIgnoreCase("Y"))
                            || (!baseHeight.equals("") && Double
                                    .parseDouble(baseHeight) <= 1)) {
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
                List<PlotObject> images = new ArrayList<PlotObject>();
                int numFcstPos = currFeature
                        .getValueAsInt(DMDAttributeIDs.NUM_FCST_POSITIONS
                                .toString());
                int numPastPos = currFeature
                        .getValueAsInt(DMDAttributeIDs.NUM_PAST_POSITIONS
                                .toString());

                List<Coordinate> coords = new ArrayList<Coordinate>();

                Coordinate currLoc = new Coordinate(currFeature.getPoints()
                        .get(0).getCoordinate2(), currFeature.getPoints()
                        .get(0).getCoordinate1());

                float lat;
                float lon;
                float prevLat;
                float prevLon;
                // Forecast Positions
                if (numFcstPos > 0
                        && (currentSettings.getDmdTrackType().equals(
                                TrackTypes.FORECAST) || currentSettings
                                .getDmdTrackType().equals(
                                        TrackTypes.PAST_AND_FORECAST))) {
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
                                this.target, this.gridGeometry,
                                this.descriptor, color));

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
                if (numPastPos > 0
                        && (currentSettings.getDmdTrackType().equals(
                                TrackTypes.PAST) || currentSettings
                                .getDmdTrackType().equals(
                                        TrackTypes.PAST_AND_FORECAST))) {
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
                                this.target, this.gridGeometry,
                                this.descriptor, color));

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
                    double radius = currFeature
                            .getValueAsDouble(DMDAttributeIDs.BASE_DIAMETER
                                    .toString()) / 2;

                    image = RadarGraphicFunctions.createMesocycloneImage(rc,
                            radius, target, this.descriptor, iconType, color);
                    double radiusInPixels = RadarGraphicFunctions
                            .getRadiusInPixels(radius, rc.asLatLon().x,
                                    rc.asLatLon().y, descriptor, target);

                    Coordinate coordID = rc.asPixel(this.descriptor
                            .getGridGeometry());
                    coordID.x = coordID.x - radiusInPixels;
                    coordID.y = coordID.y - radiusInPixels;

                    String mesoID = currFeature
                            .getValue(DMDAttributeIDs.MESO_ID.toString());

                    this.localStringMap.put(coordID, mesoID);
                } catch (NumberFormatException e) {
                    e.printStackTrace();
                } catch (TransformException e) {
                    e.printStackTrace();
                } catch (FactoryException e) {
                    e.printStackTrace();
                }
            }
        }

        return image;
    }

    // Handle GFM product
    private List<PlotObject> getGfmImage(GenericDataComponent currPt)
            throws VizException {
        List<PlotObject> images = new ArrayList<PlotObject>();

        UnitConverter metersPerSecondToKnots = SI.METERS_PER_SECOND
                .getConverterTo(NonSI.KNOT);

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
            propU = currFeature.getValue(GFMPacket.GFMAttributeIDs.PROPU
                    .getName());
            if ((propU != null) && (propU.length() > 0)) {
                pU = metersPerSecondToKnots.convert(new Double(propU));
            }
            propV = currFeature.getValue(GFMPacket.GFMAttributeIDs.PROPV
                    .getName());
            if ((propV != null) && (propV.length() > 0)) {
                pV = metersPerSecondToKnots.convert(new Double(propV));
            }
            windX = currFeature.getValue(GFMPacket.GFMAttributeIDs.WINDBEHINDX
                    .getName());
            if ((windX != null) && (windX.length() > 0)) {
                wX = Float.parseFloat(windX);
            }
            windY = currFeature.getValue(GFMPacket.GFMAttributeIDs.WINDBEHINDY
                    .getName());
            if ((windY != null) && (windY.length() > 0)) {
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
            BufferedImage imgBuf = barb.getWindImage(false, DisplayType.ARROW,
                    0.2);
            IImage img = this.target.initializeRaster(new IODataPreparer(
                    imgBuf, UUID.randomUUID().toString(), 0), null);
            poWind.image = img;

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
                    gfmFcstWireframeShape.addLineSegment(new Coordinate[] {
                            pos1, pos2 });
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
                    images = RadarGraphicFunctions
                            .createMesocycloneImage(
                                    currPt,
                                    target,
                                    this.gridGeometry,
                                    this.descriptor,
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
                images = RadarGraphicFunctions
                        .createMesocycloneImage(
                                currPt,
                                target,
                                this.gridGeometry,
                                this.descriptor,
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
            e.printStackTrace();
        } catch (FactoryException e) {
            e.printStackTrace();
        }

        return images;
    }

    public static Coordinate rectifyCoordinate(Coordinate c) {
        c.x += RadarGraphicsDisplay.X_OFFSET;
        c.y += RadarGraphicsDisplay.Y_OFFSET;
        // Flip y to match geotools nomenclature
        c.y = 4096 - c.y;

        return c;
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

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.drawables.IRenderable#paint(com.raytheon.viz.core
     * .IGraphicsTarget, com.raytheon.viz.core.drawables.PaintProperties)
     */
    @Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
        if (font == null) {
            this.font = target.initializeFont(getClass().getName());
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
        double magnification = this.magnification;
        if (this.screenGeometries != null && !this.screenGeometries.isEmpty()) {
            double minx = Double.MAX_VALUE;
            double maxx = Double.MIN_VALUE;
            for (Geometry g : this.screenGeometries) {
                for (Coordinate co : g.getCoordinates()) {
                    minx = Math.min(minx, co.x);
                    maxx = Math.max(maxx, co.x);
                }
            }

            // the target may have messes with our magnification value,
            // especially in smaller panes.
            DrawableString testString = new DrawableString("hy", this.color);
            testString.font = this.font;
            magnification = target.getStringsBounds(testString).getHeight() / 14;
            double width = (maxx - minx) * magnification;
            // If the table wider than our canvas then shrink it
            if (width > paintProps.getCanvasBounds().width) {
                magnification = this.magnification
                        * paintProps.getCanvasBounds().width
                        / (width + xOffset * 2);
                font.setMagnification((float) magnification, false);
                magnification = target.getStringsBounds(testString).getHeight() / 14;
                width = (maxx - minx) * magnification;
            }
            xOffset = (paintProps.getCanvasBounds().width - width) / 2;
            List<DrawableLine> lines = new ArrayList<DrawableLine>();
            for (Geometry g : this.screenGeometries) {
                Coordinate[] coords = g.getCoordinates();

                // offsets to make the table fit the screen
                double x1 = coords[0].x * magnification + xOffset;
                double y1 = (coords[0].y + 0.25) * magnification * 1.3
                        + yOffset;
                double x2 = coords[1].x * magnification + xOffset;
                double y2 = (coords[1].y + 0.25) * magnification * 1.3
                        + yOffset;
                DrawableLine line = new DrawableLine();
                line.addPoint(x1, y1);
                line.addPoint(x2, y2);
                line.basics.color = this.color;
                lines.add(line);
            }
            target.getExtension(ICanvasRenderingExtension.class).drawLines(
                    paintProps, lines.toArray(new DrawableLine[0]));
        }

        // Only paint data table text if no configuration is specified or
        // if configuration says to show table.
        if (tableModifier == null
                || (tableModifier != null && tableModifier.isShowTable())) {
            // Paint the table data text.
            if (this.screenStringMap != null && !this.screenStringMap.isEmpty()) {
                for (Coordinate c : this.screenStringMap.keySet()) {
                    String str = this.screenStringMap.get(c);

                    double x = c.x * magnification + xOffset;
                    double y = c.y * magnification * 1.3 + yOffset;
                    // if (x < 0.1) {
                    // x = 0;
                    // }
                    DrawableString string = new DrawableString(str, this.color);
                    string.font = this.font;
                    string.setCoordinates(x, y);
                    string.verticallAlignment = VerticalAlignment.TOP;
                    target.getExtension(ICanvasRenderingExtension.class)
                            .drawStrings(paintProps, string);
                }

            }
            target.setupClippingPlane(paintProps.getClippingPane());
            if (magnification != this.magnification) {
                font.setMagnification((float) this.magnification, false);
            }
        }

        // paint symbols on screen
        double ratio = (paintProps.getView().getExtent().getWidth() / paintProps
                .getCanvasBounds().width);
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
                DrawableString string = new DrawableString(po.label, this.color);
                string.font = this.font;
                string.setCoordinates(adjustedCoord.x, adjustedCoord.y);
                target.drawStrings(string);

            }
            target.getExtension(IPointImageExtension.class).drawPointImages(
                    paintProps, image);

        }

    }

    /**
     * Draws the table border for the tabular data that doesn't come in via the
     * Graphic Alphanumeric Block (i.e. DMD).
     */
    private void drawTableBorder() {
        this.drawBorder = false;

        int rowHeight = 10;
        int[] columnWidths = new int[] { 41, 42, 81, 70, 56, 45, 49, 63, 63,
                63, 57 };

        int numberOfRows = this.screenStringMap.size();

        int tableLeft = startTableX - 3;
        int tableTop = startTableY - 1;

        // this.screenGeometries.clear();
        List<Coordinate> coords = new ArrayList<Coordinate>();
        LineString ls = null;

        int tableRight = tableLeft;

        for (Integer width : columnWidths) {
            tableRight += width;
        }

        int tableBottom = rowHeight * numberOfRows + tableTop;

        // Rows
        for (int i = tableTop; i <= tableBottom; i += rowHeight) {
            coords.clear();
            coords.add(new Coordinate(tableLeft, i));
            coords.add(new Coordinate(tableRight, i));
            ls = this.geomFactory.createLineString(coords
                    .toArray(new Coordinate[coords.size()]));
            this.screenGeometries.add(ls);
        }

        // Columns
        int k = 0;
        for (int i = tableLeft; i <= tableRight;) {
            coords.clear();
            coords.add(new Coordinate(i, tableTop));
            coords.add(new Coordinate(i, tableBottom));
            ls = this.geomFactory.createLineString(coords
                    .toArray(new Coordinate[coords.size()]));
            this.screenGeometries.add(ls);

            i += columnWidths[k];
            if (k < 10) {
                k++;
            }
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
        List<Coordinate> coords = new ArrayList<Coordinate>();
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
                coord1 = rectifyCoordinate(new Coordinate(i, j));
                coord2 = rectifyCoordinate(new Coordinate(vector.i2, vector.j2));
            } else if (cs == CoordinateSystem.SCREEN) {
                coord1 = new Coordinate(vector.i1, vector.j1);
                coord2 = new Coordinate(vector.i2, vector.j2);
            }
            coords.add(coord1);
            coords.add(coord2);
        }

        LineString ls = this.geomFactory.createLineString(coords
                .toArray(new Coordinate[coords.size()]));
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

        if (this.font != null) {
            this.font.dispose();
        }
    }

    public void addTableRow(String headings) {
        this.screenStringMap.put(new Coordinate(tableX, tableY), headings);
        tableY += tableRowSpacing;
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
        return new ReferencedCoordinate(rectifyCoordinate(new Coordinate(i * 4,
                j * 4)), this.gridGeometry, Type.GRID_CENTER);
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
