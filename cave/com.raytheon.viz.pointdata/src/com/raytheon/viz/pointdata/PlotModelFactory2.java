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
import java.text.ParseException;
import java.text.ParsePosition;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.Formatter;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;

import jep.JepException;

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
import org.w3c.dom.Text;

import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.viz.pointdata.lookup.IAbstractLookupTable;
import com.raytheon.viz.pointdata.lookup.LookupUtils;
import com.raytheon.viz.pointdata.rsc.PlotResource2;
import com.raytheon.viz.pointdata.rsc.PlotResourceData;

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
 * 06/29/2009       2538   jsanchez    Implemented pointdata.
 * 
 * </pre>
 * 
 * @author BRock97
 * @version 1.0
 */
public class PlotModelFactory2 {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(PlotModelFactory2.class);

    private static final String plotmodelDir = "plotModels";

    private static final String DM_ATTRIBUTE = "plotMode";

    private static final String P_ATTRIBUTE = "plotParam";

    private static final String FMT_ATTRIBUTE = "plotFormat";

    private static final String UNIT_ATTRIBUTE = "plotUnit";

    private static final String SYMBOL_ATTRIBUTE = "plotSymbol";

    private static final String TRIM_ATTRIBUTE = "plotTrim";

    private static final String PFT_ATTRIBUTE = "plotFunctionTable";

    private static final String PLT_ATTRIBUTE = "plotLookupTable";

    private static final String PLT_INDEX = "plotIndex";

    private static final String REQUIRED = "required";

    private final SimpleDateFormat SAMPLE_DATE = new SimpleDateFormat("HHmm");

    // Need to include attribute and code to allow for String2String lookups and
    // String2Number lookups
    // to support clouds and present weather

    private int width = 1;

    private LineStyle lineStyle = LineStyle.DEFAULT;

    private String currentStyleStr;

    private Document document;

    private GraphicsNode theGraphicsNode;

    private final GVTBuilder builder;

    private final BridgeContext bridgeContext;

    private int plotModelWidth;

    private int plotModelHeight;

    private final int originalPlotModelWidth;

    private final int originalPlotModelHeight;

    private final Element svgRoot;

    private final GeodeticCalculator gc;

    private final IMapDescriptor mapDescriptor;

    private IndexColorModel tm;

    private final List<PlotModelElement> plotFields;

    private final List<PlotModelElement> sampleFields;

    private double lowerLimit = -9999.0;

    private double upperLimit = -9999.0;

    private boolean plotMissingData = false;

    private Map<String, BufferedImage> imageCache = null;

    private ScriptInfo scriptInfo;

    private ScriptInfo sampleScriptInfo;

    public static enum DisplayMode {
        TEXT, BARB, TABLE, AVAIL, RANGE, NULL, SAMPLE, ARROW
    }

    public class PlotModelElement {
        DisplayMode mode = DisplayMode.TEXT;

        String format = null;

        String parameter = null;

        String unit = null;

        String symbol = null;

        int trim = 0;

        int index = -1;

        UnitConverter converter = null;

        Element plotElement = null;

        Node plotNode = null;

        S2N ranking = null;

        IAbstractLookupTable lookup = null;

        PlotWindElement winds = null;

        boolean required = false;

        public Node getPlotNode() {
            return plotNode;
        }
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

    public PlotModelFactory2(IMapDescriptor mapDescriptor, String plotModelFile) {
        byte full = (byte) 255;
        byte zero = (byte) 0;
        byte[] red = { 0, zero };
        byte[] blue = { 0, full };
        byte[] green = { 0, zero };
        regenerateStyle();
        this.plotFields = new ArrayList<PlotModelElement>();
        this.sampleFields = new ArrayList<PlotModelElement>();

        tm = new IndexColorModel(8, 2, red, blue, green, 0);
        this.gc = new GeodeticCalculator(mapDescriptor.getCRS());
        this.mapDescriptor = mapDescriptor;
        String parser = XMLResourceDescriptor.getXMLParserClassName();
        SAXSVGDocumentFactory f = new SAXSVGDocumentFactory(parser);
        try {
            document = f.createDocument(PathManagerFactory.getPathManager()
                    .getStaticFile(PlotResourceData.PLOT_DIR + plotModelFile)
                    .toURI().toString());
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, "Error parsing svg file", e);
        }
        this.svgRoot = document.getDocumentElement();
        this.originalPlotModelWidth = Integer.parseInt(svgRoot.getAttributeNS(
                null, "width"));
        this.originalPlotModelHeight = Integer.parseInt(svgRoot.getAttributeNS(
                null, "height"));
        this.plotModelWidth = this.originalPlotModelWidth;
        this.plotModelHeight = this.originalPlotModelHeight;

        int displayedElementCount = 0;

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
                        displayedElementCount++;
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
                        displayedElementCount++;
                    } else if (plotElement.getAttribute(DM_ATTRIBUTE).equals(
                            "arrowuv")) {
                        thisElement.mode = DisplayMode.ARROW;
                        thisElement.winds = new PlotWindElement();
                        NodeList windElements = plotElement.getChildNodes();
                        for (int j = 0; j < windElements.getLength(); j++) {
                            if (Node.ELEMENT_NODE == windElements.item(j)
                                    .getNodeType()) {
                                Element windElement = (Element) windElements
                                        .item(j);
                                String attrClass = windElement
                                        .getAttribute("class");
                                if ("arrow".matches(attrClass)) {
                                    thisElement.winds.arrowElement = windElement;
                                    thisElement.winds.arrowNode = windElement
                                            .getChildNodes().item(0);
                                } else if ("arrow1".matches(attrClass)) {
                                    thisElement.winds.arrowElement = windElement;
                                    thisElement.winds.arrowNode = windElement
                                            .getChildNodes().item(0);
                                    thisElement.winds.arrowElement
                                            .setAttribute("arrowtype",
                                                    attrClass);
                                } else if ("arrow2".matches(attrClass)) {
                                    thisElement.winds.arrowElement = windElement;
                                    thisElement.winds.arrowNode = windElement
                                            .getChildNodes().item(0);
                                    thisElement.winds.arrowElement
                                            .setAttribute("arrowtype",
                                                    attrClass);
                                } else if ("text".matches(attrClass)) {
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
                        displayedElementCount++;
                    } else if (plotElement.getAttribute(DM_ATTRIBUTE).equals(
                            "table")
                            || plotElement.getAttribute(DM_ATTRIBUTE).equals(
                                    "recursive_translation")) {
                        thisElement.mode = DisplayMode.TABLE;
                        if (plotElement.hasAttribute(PFT_ATTRIBUTE)) {
                            thisElement.ranking = S2N.readS2NFile(plotElement
                                    .getAttribute(PFT_ATTRIBUTE));
                        }
                        if (plotElement.hasAttribute(PLT_ATTRIBUTE)) {
                            File table = getTableFile(plotElement
                                    .getAttribute(PLT_ATTRIBUTE));
                            thisElement.lookup = LookupUtils
                                    .buildLookupTable(table);
                            thisElement.lookup.setMode(plotElement
                                    .getAttribute(DM_ATTRIBUTE));
                        }
                        displayedElementCount++;
                    } else if (plotElement.getAttribute(DM_ATTRIBUTE).equals(
                            "arrow")) {
                        plotElement.setAttribute("class", "text");
                        thisElement.mode = DisplayMode.BARB;
                        displayedElementCount++;
                    } else if (plotElement.getAttribute(DM_ATTRIBUTE).equals(
                            "available")) {
                        thisElement.mode = DisplayMode.AVAIL;
                        plotElement.setAttribute("class", "text");
                        displayedElementCount++;
                    } else if (plotElement.getAttribute(DM_ATTRIBUTE).equals(
                            "range")) {
                        thisElement.mode = DisplayMode.RANGE;
                        if (plotElement.hasAttribute(PLT_ATTRIBUTE)) {
                            File table = getTableFile(plotElement
                                    .getAttribute(PLT_ATTRIBUTE));
                            thisElement.lookup = LookupUtils
                                    .buildLookupTable(table);
                            thisElement.lookup.setMode(plotElement
                                    .getAttribute(DM_ATTRIBUTE));
                        }
                        displayedElementCount++;
                    } else if (plotElement.getAttribute(DM_ATTRIBUTE).equals(
                            "null")) {
                        thisElement.mode = DisplayMode.NULL;
                    } else if (plotElement.getAttribute(DM_ATTRIBUTE).equals(
                            "sample")) {
                        thisElement.mode = DisplayMode.SAMPLE;
                        if (plotElement.hasAttribute(PLT_ATTRIBUTE)) {
                            File table = getTableFile(plotElement
                                    .getAttribute(PLT_ATTRIBUTE));
                            thisElement.lookup = LookupUtils
                                    .buildLookupTable(table);
                            thisElement.lookup.setMode(plotElement
                                    .getAttribute(DM_ATTRIBUTE));
                        }
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
                    if (plotElement.hasAttribute(SYMBOL_ATTRIBUTE)) {
                        thisElement.symbol = plotElement
                                .getAttribute(SYMBOL_ATTRIBUTE);
                    }
                    if (plotElement.hasAttribute(TRIM_ATTRIBUTE)) {
                        thisElement.trim = Integer.parseInt(plotElement
                                .getAttribute(TRIM_ATTRIBUTE));
                    }
                    if (plotElement.hasAttribute(PLT_INDEX)) {
                        thisElement.index = Integer.parseInt(plotElement
                                .getAttribute(PLT_INDEX));
                    }
                    if (plotElement.hasAttribute(REQUIRED)) {
                        thisElement.required = Boolean.parseBoolean(plotElement
                                .getAttribute(REQUIRED));
                    }
                    if (thisElement.mode != DisplayMode.SAMPLE) {
                        this.plotFields.add(thisElement);
                    } else {
                        this.sampleFields.add(thisElement);
                        thisElement.plotNode.setNodeValue("");
                    }
                }
            }
        }

        if (displayedElementCount <= 3) {
            // Dont use image caching if more then 3 elements are used, with
            // very few elements the hit rate is good enought to risk keeping
            // the images in memory, but with more elements the hit rate drops
            // and the cache size increases. 3 elements might not be the optimal
            // way of detecting complexity but it is better than nothing.
            imageCache = new HashMap<String, BufferedImage>();
        }
        NodeList scriptNodes = document.getElementsByTagName("script");
        // Only one script node supported
        if (scriptNodes.getLength() > 0) {
            Element scriptNode = (Element) scriptNodes.item(0);
            scriptInfo = new ScriptInfo();
            scriptInfo.plotDelegateName = scriptNode
                    .getAttribute("plotDelegate");
            NodeList childNodes = scriptNode.getChildNodes();
            StringBuilder sb = new StringBuilder();
            for (int i = 0; i < childNodes.getLength(); i++) {
                Node child = childNodes.item(i);
                if (Node.TEXT_NODE == child.getNodeType())
                    sb.append(((Text) child).getData());
            }
            if (sb.length() > 0)
                scriptInfo.scriptText = sb.toString();

            sampleScriptInfo = new ScriptInfo();
            sampleScriptInfo.plotDelegateName = scriptInfo.plotDelegateName;
            sampleScriptInfo.scriptText = scriptInfo.scriptText;

            // remove the scriptNode in memory so time isn't wasted
            // later attempting to render it
            scriptNode.getParentNode().removeChild(scriptNode);
        }

        UserAgentAdapter userAgentAdapter = new UserAgentAdapter();
        this.bridgeContext = new BridgeContext(userAgentAdapter);
        this.builder = new GVTBuilder();
    }

    public void setColor(RGB color) {
        if (imageCache != null) {
            imageCache.clear();
        }
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
        // System.out.println(style);
    }

    public void setLineWidth(int width) {
        if (imageCache != null) {
            imageCache.clear();
        }
        this.width = width;
        regenerateStyle();
    }

    public void setLineStyle(LineStyle style) {
        if (imageCache != null) {
            imageCache.clear();
        }
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
        if (imageCache != null) {
            imageCache.clear();
        }
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
    public synchronized BufferedImage getStationPlot(PlotData stationData,
            double latitude, double longitude) {
        double[] stationLocation = { longitude, latitude };
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
        StringBuffer imageId = new StringBuffer();
        PlotPythonScript script = null;
        try {
            boolean discard = false;

            if (scriptInfo != null) {
                script = scriptInfo.getScript();

                try {
                    Object result = script.executePlotDelegateMethod("isValid",
                            "rec", stationData);
                    if (result instanceof Boolean
                            && !((Boolean) result).booleanValue())
                        return null;
                } catch (JepException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
            }

            for (PlotModelElement element : this.plotFields) {
                boolean valid = true;
                boolean required = element.required;
                switch (element.mode) {
                case TEXT:
                    this.processTextDirective(stationData, element);
                    addToImageId(stationData, element.parameter, imageId);
                    break;
                case BARB:
                    element.plotElement.setAttribute("style", currentStyleStr);
                    valid = this.processBarbDirective(stationData, element);
                    // normalize speed before adding to id so all identical
                    // barbs will share plots
                    String[] windParams = element.parameter.split(",");
                    double speed = stationData.getNumber(windParams[0])
                            .doubleValue();
                    double dir = stationData.getNumber(windParams[1])
                            .doubleValue();
                    if (element.converter != null) {
                        speed = element.converter.convert(speed);
                    }
                    imageId.append(windNormalizer(speed));
                    // Accurate to a fourth a degree.
                    imageId.append((int) (dir - gc.getAzimuth()) * 4);
                    if (windParams.length == 3) {
                        imageId.append(stationData.getNumber(windParams[2]));
                    }
                    addToImageId(stationData,
                            element.parameter.split(",", 2)[1], imageId);
                    break;
                case ARROW:
                    element.plotElement.setAttribute("style", currentStyleStr);
                    addToImageId(stationData, element.parameter, imageId);
                    this.processArrowDirective(stationData, element);
                    break;
                case TABLE:
                    this.processTableDirective(stationData, element);
                    addToImageId(stationData, element.parameter, imageId);
                    break;
                case AVAIL:
                    this.processAvailDirective(element);
                    break;
                case RANGE:
                    valid = this.processRangeDirective(stationData, element);
                    addToImageId(stationData, element.parameter, imageId);
                    break;
                case NULL:
                    this.processNullDirective(element);
                    break;
                }
                if (!valid && required) {
                    discard = true;
                }
            }

            if (discard) {
                return null;
            }

            if (imageCache != null
                    && imageCache.containsKey(imageId.toString())) {
                return imageCache.get(imageId.toString());
            }

            BufferedImage bufferedImage = new BufferedImage(
                    this.plotModelWidth, this.plotModelHeight,
                    BufferedImage.TYPE_BYTE_INDEXED, tm);

            long t0 = System.currentTimeMillis();
            this.theGraphicsNode = builder.build(this.bridgeContext,
                    this.document);
            Graphics2D g2d = null;
            try {
                g2d = bufferedImage.createGraphics();
                this.theGraphicsNode.primitivePaint(g2d);
            } finally {
                if (g2d != null) {
                    g2d.dispose();
                }
            }
            // System.out.println("Time building and creating graphics: "
            // + (System.currentTimeMillis() - t0));
            if (imageCache != null) {
                imageCache.put(imageId.toString(), bufferedImage);
            }
            return bufferedImage;

        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error:" + e.getLocalizedMessage(), e);
        } catch (JepException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error:" + e.getLocalizedMessage(), e);
        }

        return null;
    }

    public synchronized String getStationMessage(PlotData stationData, String dataURI) {
        PlotPythonScript script = null;
        StringBuilder sampleMessage = new StringBuilder();
        try {
            if (sampleScriptInfo != null) {
                script = sampleScriptInfo.getScript();

                Object result = script.executePlotDelegateMethod("isValid",
                        "rec", stationData);
                if (result instanceof Boolean
                        && ((Boolean) result).booleanValue()) {
                    result = script.executePlotDelegateMethod("getSampleText",
                            "rec", stationData);
                    if (result instanceof String) {
                        sampleMessage.append((String) result);
                    }
                }
            } else {
                for (PlotModelElement element : this.sampleFields) {
                    sampleMessage.append(processSampleDirective(stationData,
                            element));
                }
            }

        } catch (Exception e) {
            // TODO
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        } catch (JepException e) {
            // TODO Auto-generated catch block. Please revise as appropriate.
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }

        String message = sampleMessage.toString();
        if (message.length() == 0) {
            message = PlotResource2.NO_DATA;
        }

        return message;
    }

    private void addToImageId(PlotData stationData, String parameters,
            StringBuffer imageId) {
        for (String parameter : parameters.split(",")) {
            switch (stationData.getType(parameter)) {
            case STRING:
                imageId.append(stationData.getString(parameter));
                break;
            default:
                imageId.append(stationData.getNumber(parameter).floatValue());
            }
        }
    }

    private void processTextDirective(PlotData ob, PlotModelElement element)
            throws VizException {
        int dimensions = -1;
        String sValue = null;
        String param = element.parameter;
        Number value = null;
        dimensions = ob.getDimensions(param);
        switch (ob.getType(param)) {
        case FLOAT:
        case DOUBLE:
        case INT:
        case LONG:
            if (dimensions == 1) {
                value = ob.getNumber(param);
            } else if (dimensions == 2) {
                Number[] values = ob.getNumberAllLevels(param);
                if (element.index != -1 && values != null
                        && element.index < values.length) {
                    value = values[element.index];
                }
            }
            if (isValidValue(value)) {
                double displayValue = 0.0;
                if (element.unit != null) {
                    if (element.converter == null) {
                        try {
                            Unit<?> unit = UnitFormat.getUCUMInstance()
                                    .parseProductUnit(element.unit,
                                            new ParsePosition(0));
                            element.converter = ob.getUnit(param)
                                    .getConverterTo(unit);
                        } catch (ParseException e) {
                            throw new VizException("Unable parse units ", e);
                        }
                    }
                    displayValue = element.converter.convert(value
                            .doubleValue());
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
                sValue = sValue.substring(element.trim);
                element.plotNode.setNodeValue(sValue);
            } else if (plotMissingData) {
                element.plotNode.setNodeValue("m");
            } else {
                element.plotNode.setNodeValue("");
            }
            break;
        case STRING:
            element.plotNode.setNodeValue(ob.getString(param));
            break;
        default:
            element.plotNode.setNodeValue(" ");
        }
    }

    private boolean processBarbDirective(PlotData ob, PlotModelElement element)
            throws VizException {
        String[] windParams = element.parameter.split(",");
        Number windSpeed = ob.getNumber(windParams[0]);
        Unit<?> speedUnit = ob.getUnit(windParams[0]);
        Number windDir = ob.getNumber(windParams[1]);
        Number windGust = null;
        if (windParams.length == 3) {
            windGust = ob.getNumber(windParams[2]);
        }
        double dWindDir = -9999.0;
        double cWindSpeed = -9999.0;

        if (element.unit != null && isValidValue(windSpeed)
                && windSpeed.intValue() != -9999) {
            if (element.converter == null) {
                try {
                    Unit<?> unit = UnitFormat.getUCUMInstance()
                            .parseProductUnit(element.unit,
                                    new ParsePosition(0));
                    element.converter = speedUnit.getConverterTo(unit);
                } catch (ParseException e) {
                    throw new VizException("Unable parse units ", e);
                }
            }
            cWindSpeed = element.converter.convert(windSpeed.doubleValue());
        } else if (windSpeed != null) {
            cWindSpeed = windSpeed.doubleValue();
        }
        if (windDir != null) {
            dWindDir = windDir.doubleValue();
        }

        if (isValidValue(cWindSpeed)) {
            // Element eWindSpeed =
            // (Element)element.plotElement.getChildNodes().item(0)
            if (element.winds.barbElement != null) {
                if (cWindSpeed == -9999.0 && plotMissingData) {
                    element.winds.barbElement.removeAttribute("transform");
                    element.winds.barbNode.setNodeValue("m");
                } else if (cWindSpeed >= 0 && cWindSpeed < 2.5) {
                    element.winds.barbElement.removeAttribute("transform");
                    element.winds.barbNode.setNodeValue("0");
                } else if (cWindSpeed >= 2.5 && dWindDir != -9999.0) {
                    dWindDir -= this.gc.getAzimuth();
                    int iWindSpeed = this.windNormalizer(cWindSpeed);
                    element.winds.barbElement.setAttribute("transform",
                            "rotate(" + dWindDir + ",0,0)");
                    element.winds.barbNode.setNodeValue(Integer
                            .toString(iWindSpeed));
                } else {

                    element.winds.barbElement.removeAttribute("transform");
                    element.winds.barbNode.setNodeValue(" ");
                }
            }
            renderArrow(windGust, windDir, speedUnit, element);
        } else {
            if (element.winds.barbElement != null) {
                element.winds.barbNode.setNodeValue(" ");
            }
            return false;
        }
        return true;
    }

    private void processArrowDirective(PlotData ob, PlotModelElement element)
            throws VizException {
        String[] params = element.parameter.split(",");
        Number magnitude = ob.getNumber(params[0]);
        Unit<?> speedUnit = ob.getUnit(params[0]);
        Number direction = ob.getNumber(params[1]);
        renderArrow(magnitude, direction, speedUnit, element);
    }

    private void renderArrow(Number magnitude, Number direction,
            Unit<?> speedUnit, PlotModelElement element) throws VizException {
        double dDir = -9999.0;
        double cMag = -9999.0;
        if (element.unit != null && magnitude != null) {
            if (element.converter == null) {
                try {
                    Unit<?> unit = UnitFormat
                            .getUCUMInstance()
                            .parseSingleUnit(element.unit, new ParsePosition(0));
                    element.converter = speedUnit.getConverterTo(unit);
                } catch (ParseException e) {
                    throw new VizException("Unable parse units ", e);
                }
            }
            if (isValidValue(magnitude)) {
                cMag = element.converter.convert(magnitude.doubleValue());
            }
        } else if (magnitude != null) {
            cMag = magnitude.doubleValue();
        }
        if (direction != null) {
            dDir = direction.doubleValue();
        }

        if (dDir != -9999.0 && cMag != -9999.0) {
            dDir -= this.gc.getAzimuth();

            String arrowType = null;
            if (element.winds.arrowElement != null) {
                element.winds.arrowElement.setAttribute("transform", "rotate("
                        + dDir + ",0,0)");
                arrowType = element.winds.arrowElement
                        .getAttribute("arrowtype");
                if ("arrow1".equals(arrowType) || "arrow2".equals(arrowType)) {
                    element.winds.arrowNode.setNodeValue(arrowType);
                } else {
                    element.winds.arrowNode.setNodeValue("arrow");
                }
            }
            if (element.winds.gustElement != null) {
                double len = 32.0;
                if ("arrow1".equals(arrowType) || "arrow2".equals(arrowType)) {
                    try {
                        len = Double.parseDouble(element.winds.gustY);
                    } catch (NumberFormatException nfe) {
                        len = 32.0;
                    }
                }

                double rWindDir = Math.toRadians(dDir + 180.0);

                long x = Math.round(len * Math.sin(rWindDir));
                long y = Math.round(len * Math.cos(rWindDir)) * -1;

                element.winds.gustElement.setAttribute("x", Long.toString(x));
                element.winds.gustElement.setAttribute("y", Long.toString(y));
                element.winds.gustNode.setNodeValue(Integer
                        .toString(((int) Math.round(cMag))));
            }

        } else {
            if (element.winds.arrowElement != null) {
                element.winds.arrowElement.removeAttribute("transform");
                element.winds.arrowNode.setNodeValue(" ");
            }
            if (element.winds.gustElement != null) {
                element.winds.gustNode.setNodeValue(" ");
            }
        }
    }

    private void processTableDirective(PlotData ob, PlotModelElement element) {

        int dimensions = ob.getDimensions(element.parameter);
        String display = null;
        String[] fields = null;
        switch (ob.getType(element.parameter)) {
        case FLOAT:
        case DOUBLE:
        case INT:
        case LONG:
            if (dimensions == 1) {
                display = String.valueOf((ob.getNumber(element.parameter)));
            } else if (dimensions == 2) {
                Number[] values = ob.getNumberAllLevels(element.parameter);
                fields = numberToStringArray(values);
            }
            break;
        case STRING:
            if (dimensions == 1) {
                display = ob.getString(element.parameter);
            } else if (dimensions == 2) {
                fields = ob.getStringAllLevels(element.parameter);
            }
            break;
        default:
            element.plotNode.setNodeValue(" ");
        }
        if (element.index != -1 && element.index < fields.length) {
            display = fields[element.index];
        } else if (element.ranking != null && fields != null) {
            display = element.ranking.getRankedField(fields);
        } else if (fields != null) {
            display = fields[0];
            for (int i = 1; i < fields.length; i++) {
                if (fields[i].length() > 0) {
                    display = fields[i] + " " + display;
                }
            }
        }

        if (element.lookup != null) {
            display = element.lookup.lookup(display);
        }

        if (display != null) {
            element.plotNode.setNodeValue(display);
        } else {
            element.plotNode.setNodeValue(" ");
        }
    }

    private String processSampleDirective(PlotData ob, PlotModelElement element)
            throws VizException {
        String sValue = null;
        String parameter = element.parameter;
        switch (ob.getType(parameter)) {
        case FLOAT:
        case DOUBLE:
        case INT:
        case LONG:
            Number value = ob.getNumber(parameter);
            if (value != null && value.doubleValue() != -9999.0) {
                double displayValue = 0.0;

                if (element.unit != null) {
                    if (element.converter == null) {
                        try {
                            Unit<?> unit = UnitFormat.getUCUMInstance()
                                    .parseSingleUnit(element.unit,
                                            new ParsePosition(0));
                            element.converter = ob.getUnit(parameter)
                                    .getConverterTo(unit);
                        } catch (ParseException e) {
                            throw new VizException("Unable parse units ", e);
                        }
                    }
                    displayValue = element.converter.convert(value
                            .doubleValue());
                } else {
                    displayValue = value.doubleValue();
                }
                if (element.format != null) {
                    if (element.format.equals("time")) {
                        Date d = new Date(new Double(displayValue).longValue());
                        SAMPLE_DATE.setTimeZone(TimeZone.getTimeZone("UTC"));
                        sValue = SAMPLE_DATE.format(d);
                    } else {
                        StringBuilder sb = new StringBuilder();
                        Formatter testing = new Formatter(sb);
                        testing.format(element.format, displayValue);
                        sValue = sb.toString();
                    }
                } else {
                    sValue = Double.toString(displayValue);
                }
                sValue = sValue.substring(element.trim);
            } else {
                sValue = "?";
            }
            break;
        case STRING:
            sValue = ob.getString(parameter);
            break;
        }

        if (element.lookup != null && sValue != null) {
            String lu = null;
            lu = element.lookup.lookup(sValue);
            if (lu != null) {
                sValue = lu.trim();
            }
        }

        if (sValue != null && element.symbol != null) {
            sValue = sValue + element.symbol;
        }

        return sValue + " ";
    }

    private boolean processRangeDirective(PlotData ob, PlotModelElement element)
            throws VizException {

        String sValue = null;
        switch (ob.getType(element.parameter)) {
        case FLOAT:
        case DOUBLE:
        case INT:
        case LONG:
            Number value = ob.getNumber(element.parameter);
            if (value != null && value.doubleValue() != -9999.0) {
                if (value.doubleValue() >= lowerLimit
                        && value.doubleValue() <= upperLimit) {
                    double displayValue = 0.0;
                    if (element.unit != null) {
                        if (element.converter == null) {
                            try {
                                Unit<?> unit = UnitFormat.getUCUMInstance()
                                        .parseProductUnit(element.unit,
                                                new ParsePosition(0));
                                element.converter = ob.getUnit(
                                        element.parameter).getConverterTo(unit);
                            } catch (ParseException e) {
                                throw new VizException("Unable parse units ", e);
                            }
                        }
                        displayValue = element.converter.convert(value
                                .doubleValue());
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
            }
            break;
        case STRING:
            sValue = ob.getString(element.parameter);
            break;
        default:
            element.plotNode.setNodeValue(" ");
        }
        if (element.lookup != null) {
            sValue = element.lookup.lookup(sValue);
        }
        if (sValue != null) {
            element.plotNode.setNodeValue(sValue.substring(element.trim));
            return !sValue.trim().isEmpty();
        } else if (plotMissingData) {
            element.plotNode.setNodeValue("m");
            return true;
        } else {
            element.plotNode.setNodeValue(" ");
            return false;
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
    private void processAvailDirective(PlotModelElement element) {
        element.plotNode.setNodeValue("*");
    }

    private void processNullDirective(PlotModelElement element) {
        element.plotNode.setNodeValue(" ");
    }

    /**
     * Returns the corresponding windbarb character to the actual speed
     * 
     * @param windSpeed
     * @return The character that corresponds to the nearest 5 knot speed
     */
    private int windNormalizer(double dWindSpeed) {
        int windSpeed = (int) Math.round(dWindSpeed);
        int major = windSpeed / 5;
        int minor = windSpeed % 5;
        if (minor >= 3) {
            major++;
        }
        return major * 5;
    }

    public synchronized List<PlotModelElement> getPlotFields() {
        return this.plotFields;
    }

    private String[] numberToStringArray(Number[] values) {
        ArrayList<String> list = new ArrayList<String>();
        // String[] retVal = new String[values.length];
        for (int i = 0; i < values.length; i++) {
            if (!String.valueOf(values[i]).equals("")) {
                list.add(String.valueOf(values[i]));
            }
        }

        if (list.size() > 0) {
            return list.toArray(new String[list.size()]);
        } else {
            return null;
        }
    }

    public void setLowerLimit(double lowerLimit) {
        this.lowerLimit = lowerLimit;
    }

    public void setUpperLimit(double upperLimit) {
        this.upperLimit = upperLimit;
    }

    private boolean isValidValue(Number value) {
        return value != null && value.doubleValue() > lowerLimit
                && value.doubleValue() <= upperLimit;
    }

    public void setPlotMissingData(boolean b) {
        this.plotMissingData = b;
    }

    public void disposeScript() {
        if (scriptInfo != null) {
            try {
                scriptInfo.disposeScript();
            } catch (JepException e) {
                statusHandler.handle(Priority.ERROR,
                        "Error disposing plot model script", e);
            }
        }
    }

    public void disposeSampleScript() {
        if (sampleScriptInfo != null) {
            try {
                sampleScriptInfo.disposeScript();
            } catch (JepException e) {
                statusHandler.handle(Priority.ERROR,
                        "Error disposing plot model sample script", e);
            }
        }
    }

    private class ScriptInfo {
        public String plotDelegateName;

        public String scriptText;

        private PlotPythonScript script;

        private Thread scriptThread;

        public PlotPythonScript getScript() throws JepException {
            if (script != null) {
                if (Thread.currentThread() == scriptThread)
                    return script;
                else {
                    statusHandler.handle(Priority.ERROR,
                            "Plot model scripting was not properly disposed.");
                    script = null;
                    scriptThread = null;
                }
            }

            script = createScript();
            scriptThread = Thread.currentThread();
            return script;
        }

        public void disposeScript() throws JepException {
            if (script != null) {
                try {
                    if (Thread.currentThread() == scriptThread)
                        script.dispose();
                } finally {
                    script = null;
                    scriptThread = null;
                }
            }
        }

        private PlotPythonScript createScript() throws JepException {
            PlotPythonScript script;

            Map<LocalizationLevel, LocalizationFile> map = PathManagerFactory
                    .getPathManager().getTieredLocalizationFile(
                            LocalizationType.CAVE_STATIC, "plotModels");
            ArrayList<LocalizationLevel> keys = new ArrayList<LocalizationLevel>(
                    map.keySet());
            Collections.sort(keys);
            Collections.reverse(keys);
            StringBuilder includePath = new StringBuilder();
            for (LocalizationLevel ll : keys) {
                LocalizationFile lf = map.get(ll);
                if (includePath.length() > 0)
                    includePath.append(File.pathSeparator);
                includePath.append(lf.getFile().getAbsolutePath());
            }

            File baseFile = PathManagerFactory.getPathManager().getStaticFile(
                    "plotModels/PlotModelInterface.py");
            script = new PlotPythonScript(baseFile.getAbsolutePath(),
                    includePath.toString(), plotDelegateName);
            if (scriptText != null)
                script.evaluate(scriptText);
            script.executePlotDelegateMethod("init", "plotModelFactory",
                    PlotModelFactory2.this);
            return script;
        }
    }

    private static class PlotPythonScript extends PythonScript {

        private static String CHEAT_RUN = "_cheat_run";

        private String plotDelegateName;

        public PlotPythonScript(String filePath, String anIncludePath,
                String plotDelegateName) throws JepException {
            super(filePath, anIncludePath);
            jep.eval("def "
                    + CHEAT_RUN
                    + "(text):\n return eval(compile(text,'string','exec'),globals(),globals())");
            this.plotDelegateName = plotDelegateName;
        }

        public Object evaluate(String script) throws JepException {
            Object result = jep.invoke(CHEAT_RUN, script);
            return result;
        }

        public Object executePlotDelegateMethod(String methodName,
                String argName, Object argValue) throws JepException {
            if (plotDelegateName != null) {
                HashMap<String, Object> map = null;
                if (argName != null) {
                    map = new HashMap<String, Object>();
                    map.put(argName, argValue);
                }
                return execute(methodName, plotDelegateName, map);
            } else
                return null;
        }
    }

    private File getTableFile(String fileName) {
        File rval = PathManagerFactory.getPathManager().getStaticFile(
                plotmodelDir + File.separator + fileName);
        return rval;
    }

    public List<PlotModelElement> getSampleFields() {
        return this.sampleFields;
    }

    public boolean isCachingImages() {
        return imageCache != null;
    }
}
