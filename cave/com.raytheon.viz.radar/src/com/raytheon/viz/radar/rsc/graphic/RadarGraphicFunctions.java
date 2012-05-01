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

import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.awt.image.IndexColorModel;
import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.apache.batik.bridge.BridgeContext;
import org.apache.batik.bridge.GVTBuilder;
import org.apache.batik.bridge.UserAgentAdapter;
import org.apache.batik.dom.svg.SAXSVGDocumentFactory;
import org.apache.batik.gvt.GraphicsNode;
import org.apache.batik.util.XMLResourceDescriptor;
import org.eclipse.swt.graphics.RGB;
import org.geotools.coverage.grid.GeneralGridGeometry;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.raytheon.uf.common.dataplugin.radar.level3.HdaHailPacket.HdaHailPoint;
import com.raytheon.uf.common.dataplugin.radar.level3.MesocyclonePacket.MesocyclonePoint;
import com.raytheon.uf.common.dataplugin.radar.level3.SCITDataPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.SCITDataPacket.SCITDataCell;
import com.raytheon.uf.common.dataplugin.radar.level3.STICirclePacket.STICirclePoint;
import com.raytheon.uf.common.dataplugin.radar.level3.SpecialGraphicSymbolPacket.SpecialGraphicPoint;
import com.raytheon.uf.common.dataplugin.radar.level3.SymbologyPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.TVSPacket.TVSPoint;
import com.raytheon.uf.common.dataplugin.radar.level3.TextSymbolPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.WindBarbPacket.WindBarbPoint;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.geospatial.ReferencedObject.Type;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.data.prep.IODataPreparer;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.pointdata.PointWindDisplay;
import com.raytheon.viz.pointdata.PointWindDisplay.DisplayType;
import com.raytheon.viz.radar.RadarHelper;
import com.raytheon.viz.radar.ui.RadarDisplayControls;
import com.raytheon.viz.radar.ui.RadarDisplayManager;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * Describes the functions of each packet and how to paint them based on logic
 * and weather values
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 13, 2009            chammack     Initial creation
 * Jan 20, 2010 DR 4059    Zihou Wang   Display more GSM status and
 *                                      correct display features.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
public class RadarGraphicFunctions {

    private static final String RADAR_PLOTMODEL_DIR = "radar";

    /** ID in the svg for sti current symbols */
    private static final String STI_CURR_SYMBOL = "STICurrentSymbol";

    /** ID in the svg for sti past symbols */
    private static final String STI_PAST_SYMBOL = "STIPastSymbol";

    /** ID in the svg for sti forecast symbols */
    private static final String STI_FORE_SYMBOL = "STIForecastSymbol";

    /** ID in the svg for sti hexagon symbols */
    private static final String STI_HEX_SYMBOL = "STIHexagonSymbol";

    /** Index of the font for the past symbol */
    private static final String PAST_SYM = "056";

    /** Index of the font for the current symbol */
    private static final String CURR_SYM = "057";

    /** Index of the font for the forecast symbol */
    private static final String FORE_SYM = "058";

    /** Index of the font for the DMD past symbol */
    public static final String DMD_PAST_SYM = "059";

    /** Index of the font for the DMD fcst symbol */
    public static final String DMD_FCST_SYM = "060";

    /** ID in the svg for hail symbols */
    private static final String HAIL_SYMBOL = "hailSymbol";

    /** ID in the svg for large font hail symbols */
    private static final String LG_HAIL_SYMBOL = "largeHailSymbol";

    private static final String NO_SYMBOL = " ";

    /** ID in the svg for large font hail size text */
    private static final String HAIL_SIZE = "hailSize";

    /** Index of the font for the hi hail symbol */
    private static final String HAIL_HI = "055";

    /** Index of the font for the lo hail symbol */
    private static final String HAIL_LO = "054";

    /** Index of the font for the ETVS */
    private static final String ETVS = "054";

    /** Index of the font for the TVS */
    private static final String TVS = "055";

    /** Index of the font for the extrapolated TVS */
    private static final String EXTRAP_TVS = "059";

    /** ID in the svg for tvs */
    private static final String TVS_ATTRIB = "tvs";

    static BufferedImage colorMapImage = null;

    private static UserAgentAdapter userAgentAdapter = new UserAgentAdapter();

    private static BridgeContext bridgeContext = new BridgeContext(
            userAgentAdapter);

    private static GVTBuilder builder = new GVTBuilder();

    private static Map<String, Document> docMap = new HashMap<String, Document>();

    public static enum MesocycloneType {
        CORRELATED_SHEAR, CORRELATED_SHEAR_EXTRAPOLATED, MESOCYCLONE, MESOCYCLONE_WITH_SPIKES
    }

    /**
     * 
     * Container object for returning plot objects
     * 
     * @author chammack
     * @version 1.0
     */
    public static class PlotObject {
        /** The plot image */
        public IImage image;

        /** The location on the map in world coords */
        public Coordinate coord;

        /** The offset in pixel space */
        public int[] pixelOffset;

        public String label;

    }

    /**
     * Create a set of plot objects from the HdaHailPacket
     * 
     * @param packet
     * @param target
     * @param gridGeometry
     * @param descriptor
     * @return
     * @throws VizException
     */
    public static List<PlotObject> createHailImage(SymbologyPacket packet,
            IGraphicsTarget target, GeneralGridGeometry gridGeometry,
            IDescriptor descriptor, RGB color) throws VizException {

        List<PlotObject> images = new ArrayList<PlotObject>();
        List<HdaHailPoint> points = (List<HdaHailPoint>) RadarHelper
                .getItems(packet);

        PlotObject pObject;
        for (HdaHailPoint point : points) {
            pObject = createHailImage(point, target, gridGeometry, descriptor,
                    color);
            if (pObject != null) {
                images.add(pObject);
            }
        }

        return images;
    }

    public static PlotObject createHailImage(HdaHailPoint point,
            IGraphicsTarget target, GeneralGridGeometry gridGeometry,
            IDescriptor descriptor, RGB color) throws VizException {

        RadarDisplayControls _instance = RadarDisplayManager.getInstance()
                .getCurrentSettings();

        if ((((point.getProbHail() >= _instance.getHiPOHLow() || (point
                .getProbSevereHail() >= _instance.getHiPOHLow())
                && !_instance.isShowAll()) || _instance.isShowAll()))) {
            double severeProb = point.getProbSevereHail();
            double hailProb = point.getProbHail();
            double maxSize = point.getMaxHailSize();

            if (severeProb <= 0 && hailProb <= 0 || severeProb > 100
                    && hailProb > 100) {
                // Skip erroneous and zero-probability cells
                return null;
            }

            Map<String, Object> objArr = new HashMap<String, Object>();

            // to determine size and fill of triangle
            if (severeProb >= _instance.getHiPOSHHigh()) {
                // Large filled triangle
                objArr.put(LG_HAIL_SYMBOL, HAIL_HI);
                objArr.put(HAIL_SYMBOL, NO_SYMBOL);
            } else if (severeProb >= _instance.getHiPOSHLow()) {
                // Large open triangle
                objArr.put(LG_HAIL_SYMBOL, HAIL_LO);
                objArr.put(HAIL_SYMBOL, NO_SYMBOL);
                // == 0) {
            } else if (hailProb >= _instance.getHiPOHHigh()) {
                // Small filled triangle
                objArr.put(HAIL_SYMBOL, HAIL_HI);
                objArr.put(LG_HAIL_SYMBOL, NO_SYMBOL);
            } else if (hailProb >= _instance.getHiPOHLow()) {
                // Small open triangle
                objArr.put(HAIL_SYMBOL, HAIL_LO);
                objArr.put(LG_HAIL_SYMBOL, NO_SYMBOL);
            } else {
                return null;
            }

            String size = "";
            if (maxSize == 0) {
                size = "*";
            } else {
                size = "" + (int) maxSize;
            }

            objArr.put(HAIL_SIZE, size);

            BufferedImage img = createImage(objArr, "HailIndex.svg", color);
            IImage image = convertBufferedImage(target, img, UUID.randomUUID()
                    .toString());
            PlotObject po = new PlotObject();
            po.image = image;
            ReferencedCoordinate rc = new ReferencedCoordinate(
                    RadarGraphicsPage.rectifyCoordinate(new Coordinate(point
                            .getI(), point.getJ())), gridGeometry,
                    Type.GRID_CENTER);
            try {
                po.coord = rc.asPixel(descriptor.getGridGeometry());
                // adjust y coord per original code
                po.pixelOffset = new int[] { 0, 8 };
            } catch (Exception e) {
                throw new VizException("Unable to transform coordinates", e);
            }

            return po;
        }

        return null;
    }

    /**
     * Create a set of plot objects from the TVSPacket and ETVSPacket
     * 
     * @param packet
     * @param target
     * @param gridGeometry
     * @param descriptor
     * @return
     * @throws VizException
     */
    public static List<PlotObject> createTVS(SymbologyPacket packet,
            IGraphicsTarget target, GeneralGridGeometry gridGeometry,
            IDescriptor descriptor, RGB color) throws VizException {
        List<PlotObject> images = new ArrayList<PlotObject>();
        TVSPoint[] points = (TVSPoint[]) RadarHelper.getItems(packet).toArray();
        if (points == null)
            return images;

        for (TVSPoint point : points) {
            images.add(createTVS(point, target, gridGeometry, descriptor, color));
        }

        return images;
    }

    public static List<PlotObject> createTVS(SpecialGraphicPoint currPt,
            boolean isElevated, IGraphicsTarget target,
            GeneralGridGeometry gridGeometry, IDescriptor descriptor,
            RGB color, boolean isExtrapolated) throws VizException {
        List<PlotObject> images = new ArrayList<PlotObject>();

        images.add(createTVS(currPt.getI(), currPt.getJ(), isElevated,
                isExtrapolated, target, gridGeometry, descriptor, color));

        return images;
    }

    public static PlotObject createTVS(TVSPoint point, IGraphicsTarget target,
            GeneralGridGeometry gridGeometry, IDescriptor descriptor, RGB color)
            throws VizException {
        return createTVS(point.getI(), point.getJ(), point.isElevated(), false,
                target, gridGeometry, descriptor, color);
    }

    public static PlotObject createTVS(int i, int j, boolean isElevated,
            boolean isExtrapolated, IGraphicsTarget target,
            GeneralGridGeometry gridGeometry, IDescriptor descriptor, RGB color)
            throws VizException {
        Map<String, Object> objArr = new HashMap<String, Object>();

        if (isElevated
                && RadarDisplayManager.getInstance().getCurrentSettings()
                        .isTvsShowElevated()) {
            objArr.put(TVS_ATTRIB, ETVS);
        } else if (isExtrapolated) {
            objArr.put(TVS_ATTRIB, EXTRAP_TVS);
        } else {
            objArr.put(TVS_ATTRIB, TVS);
        }

        BufferedImage img = createImage(objArr, "TVS.svg", color);
        IImage image = convertBufferedImage(target, img, UUID.randomUUID()
                .toString());
        PlotObject po = new PlotObject();
        po.image = image;
        ReferencedCoordinate rc = new ReferencedCoordinate(
                RadarGraphicsPage.rectifyCoordinate(new Coordinate(i, j)),
                gridGeometry, Type.GRID_CENTER);
        try {
            po.coord = rc.asPixel(descriptor.getGridGeometry());
            // adjust y coord per original code
            po.pixelOffset = new int[] { 0, 8 };
        } catch (Exception e) {
            throw new VizException("Unable to transform coordinates", e);
        }

        return po;
    }

    public static PlotObject createMesocycloneImage(double i, double j,
            int mesoRadius, IGraphicsTarget target,
            GeneralGridGeometry gridGeometry, IDescriptor descriptor,
            MesocycloneType type, RGB color) throws VizException,
            TransformException, FactoryException {
        ReferencedCoordinate rc = new ReferencedCoordinate(
                RadarGraphicsPage.rectifyCoordinate(new Coordinate(i, j)),
                gridGeometry, Type.GRID_CENTER);

        return createMesocycloneImage(rc, mesoRadius, target, descriptor, type,
                color);
    }

    /**
     * Returns the desired radius [km] in pixels
     * 
     * @param km
     *            The desired radius in km
     * @param lon
     *            The longitude of the origin
     * @param lat
     *            The latitude of the origin
     * @param descriptor
     * @return
     */
    public static double getRadiusInPixels(double km, double lon, double lat,
            IDescriptor descriptor, IGraphicsTarget target) {
        double radius = 0;

        try {
            double[] point1 = target.getPointOnCircle(lon, lat, 0, km, 90);
            double[] point2 = target.getPointOnCircle(lon, lat, 0, km, 270);

            radius = point1[1] - point2[1];

            if (radius < 0) {
                radius *= -1;
            }
        } catch (VizException e) {
            e.printStackTrace();
        }

        if (radius < RadarHelper.DMD_MIN_RADIUS) {
            radius = RadarHelper.DMD_MIN_RADIUS;
        }

        return radius;
    }

    public static PlotObject createMesocycloneImage(ReferencedCoordinate rc,
            double mesoRadius, IGraphicsTarget target, IDescriptor descriptor,
            MesocycloneType type, RGB color) throws VizException,
            TransformException, FactoryException {
        int spikeSize = 8;

        String svgToGet;
        if (type.equals(MesocycloneType.MESOCYCLONE_WITH_SPIKES)) {
            svgToGet = "MesocycloneWithSpikes.svg";
        } else {
            svgToGet = "Mesocyclone.svg";
        }

        // Get the SVG
        Document document = loadSVG(svgToGet);

        // Load the values into the SVG
        // Set radius
        Element radius = document.getElementById("mesocyclone");
        double mesoRadiusInPixels = getRadiusInPixels(mesoRadius,
                rc.asLatLon().x, rc.asLatLon().y, descriptor, target);
        radius.setAttribute("r", String.valueOf(mesoRadiusInPixels));

        // Draw spikes if needed
        if (type.equals(MesocycloneType.MESOCYCLONE_WITH_SPIKES)) {
            // Set spikes
            Element topSpike = document.getElementById("topSpike");
            topSpike.setAttribute("x1", "0");
            topSpike.setAttribute("y1", String.valueOf(mesoRadiusInPixels));
            topSpike.setAttribute("x2", "0");
            topSpike.setAttribute("y2",
                    String.valueOf(mesoRadiusInPixels + spikeSize));

            // <line id="rightSpike" x1="8" y1="0" x2="16" y2="0"/>
            Element rightSpike = document.getElementById("rightSpike");
            rightSpike.setAttribute("y1", "0");
            rightSpike.setAttribute("x1", String.valueOf(mesoRadiusInPixels));
            rightSpike.setAttribute("y2", "0");
            rightSpike.setAttribute("x2",
                    String.valueOf(mesoRadiusInPixels + spikeSize));

            // <line id="bottomSpike" x1="0" y1="-8" x2="0" y2="-16"/>
            Element bottomSpike = document.getElementById("bottomSpike");
            bottomSpike.setAttribute("x1", "0");
            bottomSpike.setAttribute("y1",
                    String.valueOf(-1 * mesoRadiusInPixels));
            bottomSpike.setAttribute("x2", "0");
            bottomSpike.setAttribute("y2",
                    String.valueOf(-1 * (mesoRadiusInPixels + spikeSize)));

            // <line id="leftSpike" x1="-8" y1="0" x2="-16" y2="0"/>
            Element leftSpike = document.getElementById("leftSpike");
            leftSpike.setAttribute("y1", "0");
            leftSpike.setAttribute("x1",
                    String.valueOf(-1 * mesoRadiusInPixels));
            leftSpike.setAttribute("y2", "0");
            leftSpike.setAttribute("x2",
                    String.valueOf(-1 * (mesoRadiusInPixels + spikeSize)));
        }

        if (type.equals(MesocycloneType.CORRELATED_SHEAR)) {
            radius.setAttribute("style", "");
            radius.setAttribute("stroke-width", "1px");
        } else if (type.equals(MesocycloneType.CORRELATED_SHEAR_EXTRAPOLATED)) {
            radius.setAttribute("style", "stroke-dasharray: 5, 3");
            radius.setAttribute("stroke-width", "1px");
        } else {
            radius.setAttribute("style", "");
            radius.setAttribute("stroke-width", "4px");
        }

        // Get the buffered image from SVG
        BufferedImage img = getBufferedImage(document, color);

        // Plot
        IImage image = convertBufferedImage(target, img, UUID.randomUUID()
                .toString());
        PlotObject po = new PlotObject();
        po.image = image;

        try {
            po.coord = rc.asPixel(descriptor.getGridGeometry());
            // adjust y coord per original code
            po.pixelOffset = new int[] { 0, 0 };
        } catch (Exception e) {
            throw new VizException("Unable to transform coordinates", e);
        }

        // Meso images are supposed to reflect the actual area covered
        // po.constantSize = false;

        return po;
    }

    public static List<PlotObject> createMesocycloneImage(
            SpecialGraphicPoint point, IGraphicsTarget target,
            GeneralGridGeometry gridGeometry, IDescriptor descriptor,
            MesocycloneType type, RGB color) throws VizException,
            TransformException, FactoryException {
        List<PlotObject> images = new ArrayList<PlotObject>();
        if (point != null) {
            PlotObject po = createMesocycloneImage(point.getI(), point.getJ(),
                    point.getPointFeatureAttr(), target, gridGeometry,
                    descriptor, type, color);
            images.add(po);
        }

        return images;
    }

    public static List<PlotObject> createMesocycloneImage(
            SymbologyPacket packet, IGraphicsTarget target,
            GeneralGridGeometry gridGeometry, IDescriptor descriptor,
            MesocycloneType type, RGB color) throws VizException,
            TransformException, FactoryException {
        List<PlotObject> images = new ArrayList<PlotObject>();
        MesocyclonePoint[] points = (MesocyclonePoint[]) RadarHelper.getItems(
                packet).toArray();

        if (points == null)
            return images;

        for (MesocyclonePoint point : points) {
            PlotObject po = createMesocycloneImage(point.getI(), point.getJ(),
                    point.getMesoCycloneRadius(), target, gridGeometry,
                    descriptor, type, color);

            images.add(po);
        }

        return images;
    }

    public static List<PlotObject> createMesocycloneImage(
            SymbologyPacket packet, IGraphicsTarget target,
            GeneralGridGeometry gridGeometry, IDescriptor descriptor, RGB color)
            throws VizException, TransformException, FactoryException {
        return createMesocycloneImage(packet, target, gridGeometry, descriptor,
                MesocycloneType.MESOCYCLONE, color);
    }

    /**
     * Create a set of plot objects from a WindBarbPacket, making a table
     * 
     * @param packet
     * @param target
     * @param gridGeometry
     * @param descriptor
     * @return
     * @throws VizException
     */
    public static List<PlotObject> createWindBarb(SymbologyPacket packet,
            IGraphicsTarget target, IDescriptor descriptor) throws VizException {

        List<PlotObject> images = new ArrayList<PlotObject>();
        WindBarbPoint[] points = (WindBarbPoint[]) RadarHelper.getItems(packet)
                .toArray();
        if (points == null)
            return images;

        int imgSize = 64;
        for (WindBarbPoint point : points) {
            PointWindDisplay barb = new PointWindDisplay(imgSize * 0.4, 1, 0, 0);
            RGB color;
            barb.setImageParameters(imgSize, imgSize, 255, 255, 255, 1);

            switch (point.getColorValue()) {
            case 1:
                color = RadarHelper.GREEN;
                break;
            case 2:
                color = RadarHelper.YELLOW;
                break;
            case 3:
                color = RadarHelper.BRIGHT_RED;
                break;
            case 4:
                color = RadarHelper.LIGHT_BLUE;
                break;
            case 5:
                color = RadarHelper.MED_PURPLE;
                break;
            default:
                color = RadarHelper.WHITE;
                break;
            }
            barb.setColor(color);
            barb.setWind(point.getWindBarbDir(), point.getWindBarbSpd(), true);
            BufferedImage img = barb.getWindImage(false, DisplayType.BARB, 1);
            IImage image = convertBufferedImage(target, img, UUID.randomUUID()
                    .toString());
            PlotObject po = new PlotObject();
            po.image = image;
            ReferencedCoordinate rc = new ReferencedCoordinate(
                    RadarGraphicsPage.rectifyCoordinate(new Coordinate(point
                            .getI(), point.getJ())),
                    descriptor.getGridGeometry(), Type.GRID_CENTER);
            try {
                // po.coord = rc.asPixel(descriptor);
                po.coord = new Coordinate(point.i, point.j);
                // adjust y coord per original code
                // po.pixelOffset = new int[] { 0, 8 };
                po.pixelOffset = new int[] { 0, 0 };
            } catch (Exception e) {
                throw new VizException("Unable to transform coordinates", e);
            }
            images.add(po);
        }

        return images;
    }

    public static List<PlotObject> createStormTrackIndexCircle(
            SymbologyPacket packet, IGraphicsTarget target,
            GeneralGridGeometry gridGeometry, IDescriptor descriptor,
            int hexOrCircle, RGB color) throws VizException {
        List<PlotObject> images = new ArrayList<PlotObject>();
        STICirclePoint[] points = (STICirclePoint[]) RadarHelper.getItems(
                packet).toArray();
        if (points == null)
            return images;

        for (STICirclePoint point : points) {
            Map<String, Object> objArr = new HashMap<String, Object>();

            // TODO no type of product used as of 2/17/09
            BufferedImage img = createImage(objArr, "TVS.svg", color);
            IImage image = convertBufferedImage(target, img, UUID.randomUUID()
                    .toString());
            PlotObject po = new PlotObject();
            po.image = image;
            ReferencedCoordinate rc = new ReferencedCoordinate(
                    RadarGraphicsPage.rectifyCoordinate(new Coordinate(point
                            .getI(), point.getJ())), gridGeometry,
                    Type.GRID_CENTER);
            try {
                po.coord = rc.asPixel(descriptor.getGridGeometry());
                // adjust y coord per original code
                po.pixelOffset = new int[] { 0, 8 };
            } catch (Exception e) {
                throw new VizException("Unable to transform coordinates", e);
            }

            images.add(po);

        }

        return images;
    }

    /**
     * 
     * @param packet
     * @param target
     * @param gridGeometry
     * @param descriptor
     * @return
     * @throws VizException
     */
    public static List<PlotObject> createSCITDataCell(SymbologyPacket packet,
            IGraphicsTarget target, GeneralGridGeometry gridGeometry,
            IDescriptor descriptor, RGB color) throws VizException {
        List<PlotObject> images = new ArrayList<PlotObject>();
        List<SCITDataCell> cells;
        if (packet instanceof TextSymbolPacket) {
            TextSymbolPacket pkt = (TextSymbolPacket) packet;
            cells = new ArrayList<SCITDataCell>();
            cells.add(new SCITDataCell());
            cells.get(0).setText(pkt.getTheText());
            cells.get(0).setI(pkt.getI());
            cells.get(0).setJ(pkt.getJ());
        } else {
            SCITDataPacket pkt = (SCITDataPacket) packet;
            cells = pkt.getPoints();
        }
        if (cells == null)
            return images;

        for (SCITDataCell point : cells) {
            Map<String, Object> objArr = new HashMap<String, Object>();
            if ("! ".equals(point.getText())) {
                objArr.put(STI_PAST_SYMBOL, PAST_SYM);
                objArr.put(STI_CURR_SYMBOL, NO_SYMBOL);
                objArr.put(STI_FORE_SYMBOL, NO_SYMBOL);
            } else if ("\" ".equals(point.getText())) {
                objArr.put(STI_CURR_SYMBOL, CURR_SYM);
                objArr.put(STI_PAST_SYMBOL, NO_SYMBOL);
                objArr.put(STI_FORE_SYMBOL, NO_SYMBOL);
            } else if ("# ".equals(point.getText())) {
                objArr.put(STI_FORE_SYMBOL, FORE_SYM);
                objArr.put(STI_CURR_SYMBOL, NO_SYMBOL);
                objArr.put(STI_PAST_SYMBOL, NO_SYMBOL);
            } else {
                break;
            }
            BufferedImage img = createImage(objArr, "STI.svg", color);
            IImage image = convertBufferedImage(target, img, UUID.randomUUID()
                    .toString());
            PlotObject po = new PlotObject();
            po.image = image;
            ReferencedCoordinate rc = new ReferencedCoordinate(
                    RadarGraphicsPage.rectifyCoordinate(new Coordinate(point
                            .getI(), point.getJ())), gridGeometry,
                    Type.GRID_CENTER);
            try {
                po.coord = rc.asPixel(descriptor.getGridGeometry());
                // adjust y coord per original code
                po.pixelOffset = new int[] { 7, 0 };
            } catch (Exception e) {
                throw new VizException("Unable to transform coordinates", e);
            }

            images.add(po);

        }
        return images;
    }

    public static PlotObject createDMDSTIImage(ReferencedCoordinate rc,
            String type, IGraphicsTarget target,
            GeneralGridGeometry gridGeometry, IDescriptor descriptor, RGB color)
            throws VizException {
        PlotObject po = new PlotObject();

        Map<String, Object> objArr = new HashMap<String, Object>();
        if (type.equals(DMD_PAST_SYM)) {
            objArr.put(STI_PAST_SYMBOL, DMD_PAST_SYM);
            objArr.put(STI_CURR_SYMBOL, NO_SYMBOL);
            objArr.put(STI_FORE_SYMBOL, NO_SYMBOL);
        } else if (type.equals(DMD_FCST_SYM)) {
            objArr.put(STI_FORE_SYMBOL, DMD_FCST_SYM);
            objArr.put(STI_CURR_SYMBOL, NO_SYMBOL);
            objArr.put(STI_PAST_SYMBOL, NO_SYMBOL);
        } else {
            objArr.put(STI_FORE_SYMBOL, NO_SYMBOL);
            objArr.put(STI_CURR_SYMBOL, NO_SYMBOL);
            objArr.put(STI_PAST_SYMBOL, NO_SYMBOL);
        }

        BufferedImage img = createImage(objArr, "STI.svg", color);
        IImage image = convertBufferedImage(target, img, UUID.randomUUID()
                .toString());

        po.image = image;

        try {
            po.coord = rc.asPixel(descriptor.getGridGeometry());
            // adjust y coord per original code
            po.pixelOffset = new int[] { 7, 0 };
        } catch (Exception e) {
            throw new VizException("Unable to transform coordinates", e);
        }

        return po;
    }

    /**
     * Generic method for creating an image using a radar graphics plot model
     * using a map of metadata.
     * 
     * This map is used to map elements in the svg (identified using the id
     * attribute) and set the tag values to the values in the map.
     * 
     * @param objs
     * @param plotModelFile
     * @return
     * @throws VizException
     */
    private static synchronized BufferedImage createImage(
            Map<String, Object> objs, String plotModelFile, RGB rgb)
            throws VizException {
        // Get the SVG
        Document document = loadSVG(plotModelFile);
        // Load the values into the SVG
        setSVGValues(objs, document);

        // Get the buffered image from SVG
        BufferedImage bufferedImage = getBufferedImage(document, rgb);

        return bufferedImage;
    }

    private static BufferedImage getBufferedImage(Document document, RGB rgb) {
        Element svgRoot = document.getDocumentElement();
        int originalPlotModelWidth = Integer.parseInt(svgRoot.getAttributeNS(
                null, "width"));
        int originalPlotModelHeight = Integer.parseInt(svgRoot.getAttributeNS(
                null, "height"));

        byte[] red = new byte[] { 0, (byte) rgb.red };
        byte[] green = new byte[] { 0, (byte) rgb.green };
        byte[] blue = new byte[] { 0, (byte) rgb.blue };

        IndexColorModel tm = new IndexColorModel(8, 2, red, green, blue, 0);

        BufferedImage bufferedImage = new BufferedImage(originalPlotModelWidth,
                originalPlotModelHeight, BufferedImage.TYPE_BYTE_INDEXED, tm);
        Graphics2D g2d = bufferedImage.createGraphics();

        GraphicsNode theGraphicsNode = builder.build(bridgeContext, document);
        theGraphicsNode.primitivePaint(g2d);
        theGraphicsNode.paint(g2d);
        // Cleanup and return image
        g2d.dispose();

        return bufferedImage;
    }

    private static void setSVGValues(Map<String, Object> objs, Document document) {
        Element plot = document.getElementById("plotData");
        NodeList plotElements = plot.getChildNodes();
        for (int i = 0; i < plotElements.getLength(); i++) {
            if (Node.ELEMENT_NODE == plotElements.item(i).getNodeType()) {
                Element plotElement = (Element) plotElements.item(i);
                String id = plotElement.getAttribute("id");

                if (id != null && objs.containsKey(id)) {
                    Object value = objs.get(id);
                    plotElement.getChildNodes().item(0)
                            .setNodeValue(value.toString());
                }
            }
        }

    }

    private static Document loadSVG(String plotModelFile) throws VizException {
        Document document = null;
        // document = docMap.get(plotModelFile);
        if (document == null) {

            String parser = XMLResourceDescriptor.getXMLParserClassName();
            SAXSVGDocumentFactory f = new SAXSVGDocumentFactory(parser);

            try {
                document = f.createDocument(PathManagerFactory
                        .getPathManager()
                        .getStaticFile(
                                RADAR_PLOTMODEL_DIR + File.separator
                                        + plotModelFile).toURI().toString());
            } catch (Exception e1) {
                throw new VizException("Unable to set up radar graphics model",
                        e1);
            }
            docMap.put(plotModelFile, document);
        }

        return document;
    }

    /**
     * Converting the buffered image to a graphics card image
     * 
     * @param target
     * @param img
     * @param name
     * @return
     */
    public static IImage convertBufferedImage(IGraphicsTarget target,
            BufferedImage img, String name) throws VizException {
        return target.initializeRaster(new IODataPreparer(img, name, 0), null);
    }

    /**
     * Drawing the legend for the color bars on the top left
     * 
     * @param colors
     * @return
     */
    public static BufferedImage drawColorLegend(IGraphicsTarget target,
            IDescriptor descriptor, RGB[] colors, int startNum, int endNum,
            int offset) {
        BufferedImage img = new BufferedImage(15, 15,
                BufferedImage.TYPE_BYTE_INDEXED);

        return img;
    }
}
