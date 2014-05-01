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

package com.raytheon.viz.pointdata;

import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.awt.image.IndexColorModel;
import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.text.ParseException;
import java.text.ParsePosition;
import java.util.ArrayList;
import java.util.Formatter;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;

import org.apache.batik.bridge.BridgeContext;
import org.apache.batik.bridge.GVTBuilder;
import org.apache.batik.bridge.UserAgentAdapter;
import org.apache.batik.dom.svg.SAXSVGDocumentFactory;
import org.apache.batik.gvt.GraphicsNode;
import org.apache.batik.util.XMLResourceDescriptor;
import org.eclipse.swt.graphics.RGB;
import org.geotools.referencing.GeodeticCalculator;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.dataplugin.IDecoderGettable.Amount;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;

/**
 * A singleton that will create a plot model texture based on a passed in
 * MetarRecord object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 11/20/2006              brockwoo    Initial creation.
 * 03/16/2009              jsanchez    Added processAvailDirective.
 * 05/15/2009       2338   jsanchez    Updated processTextDirective.
 * 
 * </pre>
 * 
 * @author BRock97
 * @version 1.0
 */
public class PlotModelFactory {

    private static final String plotmodelDir = "plotModels";

    private static final String DM_ATTRIBUTE = "plotMode";

    private static final String P_ATTRIBUTE = "plotParam";

    private static final String FMT_ATTRIBUTE = "plotFormat";

    private static final String UNIT_ATTRIBUTE = "plotUnit";

    private static final String TRIM_ATTRIBUTE = "plotTrim";

    private static final String PFT_ATTRIBUTE = "plotFunctionTable";

    private static final String PLT_ATTRIBUTE = "plotLookupTable";

    // Need to include attribute and code to allow for String2String lookups and
    // String2Number lookups
    // to support clouds and present weather

    private Document document;

    private GraphicsNode theGraphicsNode;

    private final GVTBuilder builder;

    private final BridgeContext bridgeContext;

    private int plotModelWidth;

    private int plotModelHeight;

    private int width = 1;

    private LineStyle lineStyle = LineStyle.DEFAULT;

    private String currentStyleStr;

    private final int originalPlotModelWidth;

    private final int originalPlotModelHeight;

    private final Element svgRoot;

    private final GeodeticCalculator gc;

    private final IMapDescriptor mapDescriptor;

    private IndexColorModel tm;

    private final ArrayList<PlotModelElement> plotFields;

    public static enum DisplayMode {
        TEXT, BARB, TABLE, AVAIL, RANGE
    }

    public class PlotModelElement {
        DisplayMode mode = DisplayMode.TEXT;

        String format = null;

        String parameter = null;

        String unit = null;

        int trim = 0;

        UnitConverter converter = null;

        Element plotElement = null;

        Node plotNode = null;

        S2N ranking = null;

        StringLookup lookup = null;

        PlotWindElement winds = null;
    }

    public class PlotWindElement {
        Node barbNode = null;

        Element barbElement = null;

        Node arrowNode = null;

        Element arrowElement = null;

        Node gustNode = null;

        Element gustElement = null;

        String gustX = null;

        String gustY = null;
    }

    public PlotModelFactory(IMapDescriptor mapDescriptor, String plotModelFile) {
        byte full = (byte) 255;
        byte zero = (byte) 0;
        byte[] red = { 0, zero };
        byte[] blue = { 0, full };
        byte[] green = { 0, zero };

        this.plotFields = new ArrayList<PlotModelElement>();

        tm = new IndexColorModel(8, 2, red, blue, green, 0);
        this.gc = new GeodeticCalculator(mapDescriptor.getCRS());
        this.mapDescriptor = mapDescriptor;
        String parser = XMLResourceDescriptor.getXMLParserClassName();
        SAXSVGDocumentFactory f = new SAXSVGDocumentFactory(parser);
        try {
            document = f.createDocument(PathManagerFactory
                    .getPathManager()
                    .getStaticFile(
                            plotmodelDir + File.separator + plotModelFile)
                    .toURI().toString());
        } catch (MalformedURLException e1) {
            // TODO Auto-generated catch block
            e1.printStackTrace();
        } catch (IOException e1) {
            // TODO Auto-generated catch block
            e1.printStackTrace();
        }
        this.svgRoot = document.getDocumentElement();
        this.originalPlotModelWidth = Integer.parseInt(svgRoot.getAttributeNS(
                null, "width"));
        this.originalPlotModelHeight = Integer.parseInt(svgRoot.getAttributeNS(
                null, "height"));
        this.plotModelWidth = this.originalPlotModelWidth;
        this.plotModelHeight = this.originalPlotModelHeight;

        Element plot = document.getElementById("plotData");
        NodeList plotElements = plot.getChildNodes();

        for (int i = 0; i < plotElements.getLength(); i++) {
            if (Node.ELEMENT_NODE == plotElements.item(i).getNodeType()) {
                Element plotElement = (Element) plotElements.item(i);
                if (plotElement.hasAttribute(DM_ATTRIBUTE)) {
                    PlotModelElement thisElement = new PlotModelElement();
                    thisElement.plotElement = plotElement;
                    thisElement.plotNode = plotElement.getChildNodes().item(0);
                    if (plotElement.getAttribute(DM_ATTRIBUTE).equals("text")) {
                        thisElement.mode = DisplayMode.TEXT;
                        plotElement.setAttribute("class", "text");
                    } else if (plotElement.getAttribute(DM_ATTRIBUTE).equals(
                            "barb")) {
                        thisElement.mode = DisplayMode.BARB;
                        thisElement.winds = new PlotWindElement();
                        NodeList windElements = plotElement.getChildNodes();
                        for (int j = 0; j < windElements.getLength(); j++) {
                            if (Node.ELEMENT_NODE == windElements.item(j)
                                    .getNodeType()) {
                                Element windElement = (Element) windElements
                                        .item(j);
                                if (windElement.getAttribute("class").matches(
                                        "arrow")) {
                                    thisElement.winds.arrowElement = windElement;
                                    thisElement.winds.arrowNode = windElement
                                            .getChildNodes().item(0);
                                } else if (windElement.getAttribute("class")
                                        .matches("barb")) {
                                    thisElement.winds.barbElement = windElement;
                                    thisElement.winds.barbNode = windElement
                                            .getChildNodes().item(0);
                                } else if (windElement.getAttribute("class")
                                        .matches("text")) {
                                    thisElement.winds.gustElement = windElement;
                                    thisElement.winds.gustNode = windElement
                                            .getChildNodes().item(0);
                                    thisElement.winds.gustX = windElement
                                            .getAttribute("x");
                                    thisElement.winds.gustY = windElement
                                            .getAttribute("y");
                                }
                            }
                        }
                    } else if (plotElement.getAttribute(DM_ATTRIBUTE).equals(
                            "table")) {
                        thisElement.mode = DisplayMode.TABLE;
                        if (plotElement.hasAttribute(PFT_ATTRIBUTE)) {
                            thisElement.ranking = S2N.readS2NFile(plotElement
                                    .getAttribute(PFT_ATTRIBUTE));
                        }
                        if (plotElement.hasAttribute(PLT_ATTRIBUTE)) {
                            thisElement.lookup = StringLookup
                                    .readS2SFile(plotElement
                                            .getAttribute(PLT_ATTRIBUTE));
                        }
                    } else if (plotElement.getAttribute(DM_ATTRIBUTE).equals(
                            "range")) {
                        thisElement.mode = DisplayMode.RANGE;
                        if (plotElement.hasAttribute(PLT_ATTRIBUTE)) {
                            thisElement.lookup = StringLookup
                                    .readR2SFile(plotElement
                                            .getAttribute(PLT_ATTRIBUTE));
                        }
                    } else if (plotElement.getAttribute(DM_ATTRIBUTE).equals(
                            "arrow")) {
                        plotElement.setAttribute("class", "text");
                        thisElement.mode = DisplayMode.BARB;
                    } else if (plotElement.getAttribute(DM_ATTRIBUTE).equals(
                            "available")) {
                        // for plotMode="available"
                        thisElement.mode = DisplayMode.AVAIL;
                        plotElement.setAttribute("class", "text");
                    }
                    thisElement.parameter = plotElement
                            .getAttribute(P_ATTRIBUTE);
                    if (plotElement.hasAttribute(FMT_ATTRIBUTE)) {
                        thisElement.format = plotElement
                                .getAttribute(FMT_ATTRIBUTE);
                    }
                    if (plotElement.hasAttribute(UNIT_ATTRIBUTE)) {
                        thisElement.unit = plotElement
                                .getAttribute(UNIT_ATTRIBUTE);
                    }
                    if (plotElement.hasAttribute(TRIM_ATTRIBUTE)) {
                        thisElement.trim = Integer.parseInt(plotElement
                                .getAttribute(TRIM_ATTRIBUTE));
                    }
                    this.plotFields.add(thisElement);
                }
            }
        }

        UserAgentAdapter userAgentAdapter = new UserAgentAdapter();
        this.bridgeContext = new BridgeContext(userAgentAdapter);
        this.builder = new GVTBuilder();
    }

    public void setColor(RGB color) {
        byte fullr = (byte) color.red;
        byte fullg = (byte) color.green;
        byte fullb = (byte) color.blue;
        String style = "stroke: rgb(" + color.red + "," + color.green + ","
                + color.blue + ");";
        // this.svgRoot.setAttribute("style", style);
        // System.out.println(style);
        byte[] red = { 0, fullr };
        byte[] blue = { 0, fullb };
        byte[] green = { 0, fullg };
        tm = new IndexColorModel(8, 2, red, green, blue, 0);
        System.out.println(style);
    }

    public void setLineWidth(int width) {
        this.width = width;
        regenerateStyle();
    }

    public void setLineStyle(LineStyle style) {
        this.lineStyle = style;
        regenerateStyle();
    }

    private void regenerateStyle() {
        String strokeStart = "stroke-dasharray: ";
        switch (lineStyle) {
        case DASH_DOTTED: {
            strokeStart += "3, 2, 1, 2";
            break;
        }
        case DASHED: {
            strokeStart += "4, 4";
            break;
        }
        case DASHED_LARGE: {
            strokeStart += "12, 4";
            break;
        }
        case DOTTED: {
            strokeStart += "1, 1";
            break;
        }
        default: {
            strokeStart += "1, 0";
            break;
        }
        }

        strokeStart += "; stroke-width: " + width + ";";

        currentStyleStr = strokeStart;
    }

    public void setPlotDimensions(long x, long y) {
        this.svgRoot.setAttributeNS(null, "width", Long.toString(x));
        this.svgRoot.setAttributeNS(null, "height", Long.toString(y));
        this.plotModelWidth = (int) x;
        this.plotModelHeight = (int) y;
    }

    public int getDefinedPlotModelWidth() {
        return this.originalPlotModelWidth;
    }

    public int getDefinedPlotModelHeight() {
        return this.originalPlotModelHeight;
    }

    /**
     * Takes the station name and its MetarRecord object and produces a buffered
     * image.
     * 
     * @param station
     *            The station name
     * @param stationData
     *            A metar record for that station
     * @return A buffered image representing the station data
     */
    public BufferedImage getStationPlot(PluginDataObject stationData)
            throws VizException {
        try {
            if (stationData != null
                    && (stationData instanceof IDecoderGettable)) {

                IDecoderGettable observation = (IDecoderGettable) stationData;

                double[] stationLocation = {
                        observation.getValue("NLON").doubleValue(),
                        observation.getValue("NLAT").doubleValue() };
                double[] stationPixelLocation = this.mapDescriptor
                        .worldToPixel(stationLocation);
                if (stationPixelLocation != null) {
                    stationPixelLocation[1]--;
                    double[] newWorldLocation = this.mapDescriptor
                            .pixelToWorld(stationPixelLocation);
                    this.gc.setStartingGeographicPoint(stationLocation[0],
                            stationLocation[1]);
                    this.gc.setDestinationGeographicPoint(newWorldLocation[0],
                            newWorldLocation[1]);
                }

                for (PlotModelElement element : this.plotFields) {
                    switch (element.mode) {
                    case TEXT:
                        this.processTextDirective(observation, element);
                        break;
                    case BARB:
                        element.plotElement.setAttribute("style",
                                currentStyleStr);
                        this.processBarbDirective(observation, element);
                        break;
                    case TABLE:
                        this.processTableDirective(observation, element);
                        break;
                    case RANGE:
                        this.processRangeDirective(observation, element);
                        break;
                    case AVAIL:
                        this.processAvailDirective(observation, element);
                        break;
                    }

                }
                BufferedImage bufferedImage = new BufferedImage(
                        this.plotModelWidth, this.plotModelHeight,
                        BufferedImage.TYPE_BYTE_INDEXED, tm);
                Graphics2D g2d = bufferedImage.createGraphics();

                this.theGraphicsNode = builder.build(this.bridgeContext,
                        this.document);
                this.theGraphicsNode.primitivePaint(g2d);
                // Cleanup and return image
                g2d.dispose();

                return bufferedImage;
            }
        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        return null;
    }

    private void processTextDirective(IDecoderGettable ob,
            PlotModelElement element) throws VizException {
        Amount value = ob.getValue(element.parameter);
        String sValue = null;
        if (value != null && value.doubleValue() != -9999.0) {
            double displayValue = 0.0;
            if (element.unit != null) {
                try {
                    Unit<?> unit = UnitFormat
                            .getUCUMInstance()
                            .parseSingleUnit(element.unit, new ParsePosition(0));
                    element.converter = value.getUnit().getConverterTo(unit);
                } catch (ParseException e) {
                    throw new VizException("Unable parse units ", e);
                }
                displayValue = element.converter.convert(value.doubleValue());
            } else {
                displayValue = value.doubleValue();
            }
            if (element.format != null) {
                StringBuilder sb = new StringBuilder();
                Formatter testing = new Formatter(sb);
                testing.format(element.format, displayValue);
                sValue = sb.toString();
            } else {
                sValue = Double.toString(displayValue);
            }
            element.plotNode.setNodeValue(sValue.substring(element.trim));
        } else if ((sValue = ob.getString(element.parameter)) != null) {
            element.plotNode.setNodeValue(ob.getString(element.parameter));
        } else {
            element.plotNode.setNodeValue(" ");
        }
    }

    private void processBarbDirective(IDecoderGettable ob,
            PlotModelElement element) throws VizException {
        String[] windParams = element.parameter.split(",");
        Amount windSpeed = ob.getValue(windParams[0]);
        Amount windDir = ob.getValue(windParams[1]);
        Amount windGust = null;
        if (windParams.length == 3) {
            windGust = ob.getValue(windParams[2]);
        }
        double dWindDir = -9999.0;
        double cWindSpeed = -9999.0;
        double cWindGust = -9999.0;
        if (element.unit != null && windSpeed != null) {
            if (element.converter == null) {
                try {
                    Unit<?> unit = UnitFormat
                            .getUCUMInstance()
                            .parseSingleUnit(element.unit, new ParsePosition(0));
                    element.converter = windSpeed.getUnit()
                            .getConverterTo(unit);
                } catch (ParseException e) {
                    throw new VizException("Unable parse units ", e);
                }
            }
            if (windSpeed != null && windSpeed.doubleValue() != -9999.0) {
                cWindSpeed = element.converter.convert(windSpeed.doubleValue());
            }
            if (windGust != null && windGust.doubleValue() != -9999.0) {
                cWindGust = element.converter.convert(windGust.doubleValue());
            }
        } else if (windSpeed != null) {
            if (windSpeed != null) {
                cWindSpeed = windSpeed.doubleValue();
            }
            if (windGust != null) {
                cWindGust = windGust.doubleValue();
            }
        }
        if (windDir != null) {
            dWindDir = windDir.doubleValue();
        }
        // Element eWindSpeed =
        // (Element)element.plotElement.getChildNodes().item(0)
        if (element.winds.barbElement != null) {
            if (cWindSpeed >= 0 && cWindSpeed < 3.0) {
                element.winds.barbElement.removeAttribute("transform");
                element.winds.barbNode.setNodeValue("0");
            } else if (cWindSpeed >= 3.0 && dWindDir != -9999.0) {
                dWindDir -= this.gc.getAzimuth();
                int iWindSpeed = this.windNormalizer(cWindSpeed);
                element.winds.barbElement.setAttribute("transform", "rotate("
                        + dWindDir + ",0,0)");
                element.winds.barbNode.setNodeValue(Integer
                        .toString(iWindSpeed));
            } else {

                element.winds.barbElement.removeAttribute("transform");
                element.winds.barbNode.setNodeValue(" ");
            }
        }
        if (element.winds.arrowElement != null) {
            if (dWindDir != -9999.0 && cWindGust != -9999.0) {
                dWindDir -= this.gc.getAzimuth();

                element.winds.arrowElement.setAttribute("transform", "rotate("
                        + dWindDir + ",0,0)");
                element.winds.arrowNode.setNodeValue("arrow");
            } else {
                element.winds.arrowElement.removeAttribute("transform");
                element.winds.arrowNode.setNodeValue(" ");
            }
        }
        if (element.winds.gustElement != null) {
            if (dWindDir != -9999.0 && cWindGust != -9999.0) {
                dWindDir -= this.gc.getAzimuth();
                double rWindDir = Math.toRadians(dWindDir + 180.0);
                long x = Math.round(32.0 * Math.sin(rWindDir));
                long y = Math.round(32.0 * Math.cos(rWindDir)) * -1;
                element.winds.gustElement.setAttribute("x", Long.toString(x));
                element.winds.gustElement.setAttribute("y", Long.toString(y));
                element.winds.gustNode.setNodeValue(Integer
                        .toString(((int) cWindGust)));
            } else {
                element.winds.gustNode.setNodeValue(" ");
            }
        }
    }

    private void processTableDirective(IDecoderGettable ob,
            PlotModelElement element) {
        String[] fields = ob.getStrings(element.parameter);
        String display = null;
        if (element.ranking != null && fields != null) {
            display = element.ranking.getRankedField(fields);
        } else if (fields != null) {
            display = fields[0];
        }
        if (element.lookup != null) {
            display = element.lookup.recursiveTranslation(display);
        }
        if (display != null) {
            element.plotNode.setNodeValue(display);
        } else {
            element.plotNode.setNodeValue(" ");
        }
    }

    private void processRangeDirective(IDecoderGettable ob,
            PlotModelElement element) throws VizException {
        Amount value = ob.getValue(element.parameter);
        String sValue = null;
        if (value != null && value.doubleValue() != -9999.0) {
            double displayValue = 0.0;
            if (element.unit != null) {
                if (element.converter == null) {
                    try {
                        Unit<?> unit = UnitFormat.getUCUMInstance()
                                .parseSingleUnit(element.unit,
                                        new ParsePosition(0));
                        element.converter = value.getUnit()
                                .getConverterTo(unit);
                    } catch (ParseException e) {
                        throw new VizException("Unable parse units ", e);
                    }
                }
                displayValue = element.converter.convert(value.doubleValue());
            } else {
                displayValue = value.doubleValue();
            }
            if (element.format != null) {
                StringBuilder sb = new StringBuilder();
                Formatter testing = new Formatter(sb);
                testing.format(element.format, displayValue);
                sValue = sb.toString();
            } else {
                sValue = Double.toString(displayValue);
            }
        }

        if (element.lookup != null) {
            sValue = element.lookup.determineRange(sValue);
        }
        if (sValue != null) {
            element.plotNode.setNodeValue(sValue.substring(element.trim));
        } else {
            element.plotNode.setNodeValue(" ");
        }
    }

    /**
     * Displays an asterisks (*) to all available plot locations
     * 
     * @param ob
     *            IDecoderGettable object
     * @param element
     *            PlotModelElement object
     */
    private void processAvailDirective(IDecoderGettable ob,
            PlotModelElement element) {
        element.plotNode.setNodeValue("*");
    }

    /**
     * Returns the corresponding windbarb character to the actual speed
     * 
     * @param windSpeed
     * @return The character that corresponds to the nearest 5 knot speed
     */
    private int windNormalizer(double dWindSpeed) {
        int windSpeed = (int) dWindSpeed;
        int major = windSpeed / 5;
        int minor = windSpeed % 5;
        if (minor >= 3) {
            major++;
        }
        return major * 5;
    }
}
