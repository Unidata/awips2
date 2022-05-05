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

import java.awt.image.BufferedImage;
import java.io.File;
import java.text.ParsePosition;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.Formatter;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.measure.IncommensurableException;
import javax.measure.UnconvertibleException;
import javax.measure.Unit;
import javax.measure.format.ParserException;

import org.eclipse.swt.graphics.RGB;
import org.geotools.referencing.GeodeticCalculator;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.concurrent.PythonJobCoordinator;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.point.svg.SVGImageFactory;
import com.raytheon.viz.pointdata.def.Condition;
import com.raytheon.viz.pointdata.def.PlotParameterDefinition;
import com.raytheon.viz.pointdata.lookup.LookupUtils;
import com.raytheon.viz.pointdata.python.CheckPlotValidityExecutor;
import com.raytheon.viz.pointdata.python.PlotPythonScript;
import com.raytheon.viz.pointdata.python.PlotPythonScriptFactory;
import com.raytheon.viz.pointdata.python.SampleTextExecutor;
import com.raytheon.viz.pointdata.rsc.PlotResource;

import tec.uom.se.format.SimpleUnitFormat;
import tec.uom.se.format.SimpleUnitFormat.Flavor;

/**
 * A factory for generating plot images and sample messages by parsing the
 * associated plotModel SVG file.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Nov 20, 2006           brockwoo    Initial creation.
 * Mar 16, 2009           jsanchez    Added processAvailDirective.
 * Jun 29, 2009  2538     jsanchez    Implemented pointdata.
 * Aug 09, 2012  1085     jkorman     Corrected data construction.
 * Sep 05, 2013  2316     bsteffen    Unify pirep and ncpirep.
 * Sep 05, 2013  2307     dgilling    Use better PythonScript constructor.
 * Nov 20, 2013  2033     njensen     Fix detecting plotModels dirs from multiple plugins
 * Mar 21, 2014  2868     njensen     Refactored python usage to PythonJobCoordinator
 * Jun 06, 2014  2061     bsteffen    Rename and add support for data formats in sampling.
 * Aug 07, 2014  3478     bclement    removed PointDataDescription.Type.Double
 * Dec 16, 2014 16193     kshrestha   Updated range limits
 * Oct 27, 2015  4798     bsteffen    Extend SVGImageFactory
 * Dec 14, 2015  4816     dgilling    Support refactored PythonJobCoordinator API.
 * Jun 12, 2017  6303     bsteffen    Add getColor
 * Aug 07, 2017  6376     bsteffen    Handle script inside cdata.
 * Jan 26, 2018  6698     njensen     Create antialiased images
 * Mar 26, 2018  6759     tgurney     Take the negative sign off negative zero
 * May 03, 2018  6894     njensen     Valid value filter is now lowerLimit <= value <= upperLimit
 * Sep 03, 2019  67550    ksunil      correctLat/Lon code to fix IllegalArgument exception
 * Nov 01, 2019  71272    ksunil      code re-factoring to process richer .svg files. Original 
 *                                      PlotModelFactory was renamed to PlotModelFactoryDefault.
 *                                      Both versions implement PlotModelFactoryIntf.
 * 12/10/2019   72280      ksunil      Added condition filter
 * Jan 13, 2020 73084    ksunil       addElement has a new signature
 *
 *
 * </pre>
 *
 * @author BRock97
 */
public class PlotModelFactoryDefault extends SVGImageFactory
        implements IPlotModelFactory {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(PlotModelFactoryDefault.class);

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

    private static final int NUM_POOL_THREADS = 1;

    private static final Pattern PYTHON_LINE_PATTERN = Pattern
            .compile("^(\\s+)(\\S.*)?");

    private int width = 1;

    private LineStyle lineStyle = LineStyle.DEFAULT;

    private String currentStyleStr;

    private int plotModelWidth;

    private int plotModelHeight;

    private final int originalPlotModelWidth;

    private final int originalPlotModelHeight;

    private final Element svgRoot;

    private final GeodeticCalculator gc;

    private final IMapDescriptor mapDescriptor;

    private RGB color;

    private final List<IPlotModelElement> plotFields;

    private final List<IPlotModelElement> sampleFields;

    private double lowerLimit = -9999.0;

    private double upperLimit = -9999.0;

    private boolean plotMissingData = false;

    private Map<String, BufferedImage> imageCache = null;

    protected final String plotModelFile;

    protected PythonJobCoordinator<PlotPythonScript> python;

    public static enum DisplayMode {
        TEXT, BARB, TABLE, AVAIL, RANGE, NULL, SAMPLE, ARROW
    }

    public PlotModelFactoryDefault(IMapDescriptor mapDescriptor,
            String plotModelFile) throws VizException {
        super(plotModelFile(plotModelFile));
        regenerateStyle();
        this.plotModelFile = plotModelFile;
        this.plotFields = new ArrayList<>();
        this.sampleFields = new ArrayList<>();

        setColor(new RGB(0, 0, 255));
        this.gc = new GeodeticCalculator(mapDescriptor.getCRS());
        this.mapDescriptor = mapDescriptor;

        this.svgRoot = document.getDocumentElement();
        this.originalPlotModelWidth = Integer
                .parseInt(svgRoot.getAttributeNS(null, "width"));
        this.originalPlotModelHeight = Integer
                .parseInt(svgRoot.getAttributeNS(null, "height"));
        this.plotModelWidth = this.originalPlotModelWidth;
        this.plotModelHeight = this.originalPlotModelHeight;

        int displayedElementCount = 0;

        Element plot = document.getElementById("plotData");
        NodeList plotElements = plot.getChildNodes();

        for (int i = 0; i < plotElements.getLength(); i++) {
            if (Node.ELEMENT_NODE == plotElements.item(i).getNodeType()) {
                Element plotElement = (Element) plotElements.item(i);
                if (plotElement.hasAttribute(DM_ATTRIBUTE)) {
                    PlotModelElementDefault thisElement = parseElement(
                            plotElement);
                    if (thisElement.mode != DisplayMode.SAMPLE) {
                        this.plotFields.add(thisElement);
                        if (thisElement.mode != DisplayMode.NULL) {
                            displayedElementCount += 1;
                        }
                    } else {
                        this.sampleFields.add(thisElement);
                        thisElement.plotNode.setNodeValue("");
                    }
                }
            }
        }

        if (displayedElementCount <= 3) {
            /*
             * Don't use image caching if more then 3 elements are used, with
             * very few elements the hit rate is good enough to risk keeping the
             * images in memory, but with more elements the hit rate drops and
             * the cache size increases. 3 elements might not be the optimal way
             * of detecting complexity but it is better than nothing.
             */
            imageCache = new HashMap<>();
        }
        NodeList scriptNodes = document.getElementsByTagName("script");

        // Only one script node supported
        int nScriptNodes = scriptNodes.getLength();
        if (nScriptNodes > 1) {
            throw new UnsupportedOperationException(
                    "Only one script node allowed in plotModel SVG file.  Please check and fix "
                            + plotModelFile);
        } else if (nScriptNodes == 1) {
            Element scriptNode = (Element) scriptNodes.item(0);
            String plotDelegateName = scriptNode.getAttribute("plotDelegate");
            NodeList childNodes = scriptNode.getChildNodes();
            StringBuilder sb = new StringBuilder();
            for (int i = 0; i < childNodes.getLength(); i++) {
                Node child = childNodes.item(i);
                if (Node.TEXT_NODE == child.getNodeType()
                        || Node.CDATA_SECTION_NODE == child.getNodeType()) {
                    sb.append(((Text) child).getData());
                }

            }
            String scriptText = sb.toString();
            /*
             * The python script may have extra indenting to align with the
             * indent level of the properly formatted xml around it. Python
             * can't handle the extra indent, so calculate the least common
             * indent(LCI) and remove it from every line.
             */
            String[] scriptLines = scriptText.split("\n");
            int lci = Integer.MAX_VALUE;
            for (String line : scriptLines) {
                if (line.isEmpty()) {
                    continue;
                }
                Matcher m = PYTHON_LINE_PATTERN.matcher(line);
                if (!m.matches()) {
                    lci = 0;
                    break;
                } else if (m.group(2) != null) {
                    /* if group 2 is null it is a blank line, ignore. */
                    lci = Math.min(m.group(1).length(), lci);
                }
            }
            if (lci > 0) {
                sb.setLength(0);
                for (String line : scriptLines) {
                    if (!line.trim().isEmpty()) {
                        sb.append(line.substring(lci)).append("\n");
                    }
                }
                scriptText = sb.toString();
            }
            if (scriptText.length() > 0) {
                PlotPythonScriptFactory pythonFactory = new PlotPythonScriptFactory(
                        plotModelFile, scriptText, plotDelegateName);
                python = new PythonJobCoordinator<>(NUM_POOL_THREADS,
                        plotModelFile, pythonFactory);
            }

            /*
             * Remove the scriptNode in memory so time isn't wasted later
             * attempting to render it
             */
            scriptNode.getParentNode().removeChild(scriptNode);
        }
    }

    private static PlotModelElementDefault parseElement(Element plotElement) {
        String dmAttribute = plotElement.getAttribute(DM_ATTRIBUTE);
        PlotModelElementDefault thisElement = new PlotModelElementDefault();
        thisElement.plotElement = plotElement;
        thisElement.plotNode = plotElement.getChildNodes().item(0);
        if ("text".equals(dmAttribute)) {
            thisElement.mode = DisplayMode.TEXT;
            plotElement.setAttribute("class", "text");
        } else if ("barb".equals(dmAttribute)) {
            thisElement.mode = DisplayMode.BARB;
            thisElement.winds = new PlotWindElement();
            NodeList windElements = plotElement.getChildNodes();
            for (int j = 0; j < windElements.getLength(); j++) {
                if (Node.ELEMENT_NODE == windElements.item(j).getNodeType()) {
                    Element windElement = (Element) windElements.item(j);
                    String elementClass = windElement.getAttribute("class");
                    if (elementClass.matches("arrow")) {
                        thisElement.winds.arrowElement = windElement;
                        thisElement.winds.arrowNode = windElement
                                .getChildNodes().item(0);
                    } else if (elementClass.matches("barb")) {
                        thisElement.winds.barbElement = windElement;
                        thisElement.winds.barbNode = windElement.getChildNodes()
                                .item(0);
                    } else if (elementClass.matches("text")) {
                        thisElement.winds.gustElement = windElement;
                        thisElement.winds.gustNode = windElement.getChildNodes()
                                .item(0);
                        thisElement.winds.gustX = windElement.getAttribute("x");
                        thisElement.winds.gustY = windElement.getAttribute("y");
                    }
                }
            }
        } else if ("arrowuv".equals(dmAttribute)) {
            thisElement.mode = DisplayMode.ARROW;
            thisElement.winds = new PlotWindElement();
            NodeList windElements = plotElement.getChildNodes();
            for (int j = 0; j < windElements.getLength(); j++) {
                if (Node.ELEMENT_NODE == windElements.item(j).getNodeType()) {
                    Element windElement = (Element) windElements.item(j);
                    String attrClass = windElement.getAttribute("class");
                    if ("arrow".matches(attrClass)) {
                        thisElement.winds.arrowElement = windElement;
                        thisElement.winds.arrowNode = windElement
                                .getChildNodes().item(0);
                    } else if ("arrow1".matches(attrClass)) {
                        thisElement.winds.arrowElement = windElement;
                        thisElement.winds.arrowNode = windElement
                                .getChildNodes().item(0);
                        thisElement.winds.arrowElement.setAttribute("arrowtype",
                                attrClass);
                    } else if ("arrow2".matches(attrClass)) {
                        thisElement.winds.arrowElement = windElement;
                        thisElement.winds.arrowNode = windElement
                                .getChildNodes().item(0);
                        thisElement.winds.arrowElement.setAttribute("arrowtype",
                                attrClass);
                    } else if ("text".matches(attrClass)) {
                        thisElement.winds.gustElement = windElement;
                        thisElement.winds.gustNode = windElement.getChildNodes()
                                .item(0);
                        thisElement.winds.gustX = windElement.getAttribute("x");
                        thisElement.winds.gustY = windElement.getAttribute("y");
                    }
                }
            }
        } else if ("table".equals(dmAttribute)
                || "recursive_translation".equals(dmAttribute)) {
            thisElement.mode = DisplayMode.TABLE;
            if (plotElement.hasAttribute(PFT_ATTRIBUTE)) {
                thisElement.ranking = S2N
                        .readS2NFile(plotElement.getAttribute(PFT_ATTRIBUTE));
            }
            if (plotElement.hasAttribute(PLT_ATTRIBUTE)) {
                File table = getTableFile(
                        plotElement.getAttribute(PLT_ATTRIBUTE));
                thisElement.lookup = LookupUtils.buildLookupTable(table);
                thisElement.lookup.setMode(dmAttribute);
            }
        } else if ("arrow".equals(dmAttribute)) {
            plotElement.setAttribute("class", "text");
            thisElement.mode = DisplayMode.BARB;
        } else if ("available".equals(dmAttribute)) {
            thisElement.mode = DisplayMode.AVAIL;
            plotElement.setAttribute("class", "text");
        } else if ("range".equals(dmAttribute)) {
            thisElement.mode = DisplayMode.RANGE;
            if (plotElement.hasAttribute(PLT_ATTRIBUTE)) {
                File table = getTableFile(
                        plotElement.getAttribute(PLT_ATTRIBUTE));
                thisElement.lookup = LookupUtils.buildLookupTable(table);
                thisElement.lookup.setMode(dmAttribute);
            }
        } else if ("null".equals(dmAttribute)) {
            thisElement.mode = DisplayMode.NULL;
        } else if ("sample".equals(dmAttribute)) {
            thisElement.mode = DisplayMode.SAMPLE;
            if (plotElement.hasAttribute(PLT_ATTRIBUTE)) {
                File table = getTableFile(
                        plotElement.getAttribute(PLT_ATTRIBUTE));
                thisElement.lookup = LookupUtils.buildLookupTable(table);
                thisElement.lookup.setMode(dmAttribute);
            }
        }
        thisElement.parameter = plotElement.getAttribute(P_ATTRIBUTE);
        if (plotElement.hasAttribute(FMT_ATTRIBUTE)) {
            thisElement.format = plotElement.getAttribute(FMT_ATTRIBUTE);
        }
        if (plotElement.hasAttribute(UNIT_ATTRIBUTE)) {
            thisElement.unit = plotElement.getAttribute(UNIT_ATTRIBUTE);
        }
        if (plotElement.hasAttribute(SYMBOL_ATTRIBUTE)) {
            thisElement.symbol = plotElement.getAttribute(SYMBOL_ATTRIBUTE);
        }
        if (plotElement.hasAttribute(TRIM_ATTRIBUTE)) {
            thisElement.trim = Integer
                    .parseInt(plotElement.getAttribute(TRIM_ATTRIBUTE));
        }
        if (plotElement.hasAttribute(PLT_INDEX)) {
            thisElement.index = Integer
                    .parseInt(plotElement.getAttribute(PLT_INDEX));
        }
        if (plotElement.hasAttribute(REQUIRED)) {
            thisElement.required = Boolean
                    .parseBoolean(plotElement.getAttribute(REQUIRED));
        }
        return thisElement;
    }

    @Override
    public void setColor(RGB color) {
        if (imageCache != null) {
            imageCache.clear();
        }
        this.color = color;
    }

    @Override
    public RGB getColor() {
        return color;
    }

    @Override
    public void setLineWidth(int width) {
        this.width = width;
        regenerateStyle();
    }

    @Override
    public void setLineStyle(LineStyle style) {
        this.lineStyle = style;
        regenerateStyle();
    }

    private void regenerateStyle() {
        if (imageCache != null) {
            imageCache.clear();
        }
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

    @Override
    public void setPlotDimensions(long x, long y) {
        if (imageCache != null) {
            imageCache.clear();
        }
        this.svgRoot.setAttributeNS(null, "width", Long.toString(x));
        this.svgRoot.setAttributeNS(null, "height", Long.toString(y));
        this.plotModelWidth = (int) x;
        this.plotModelHeight = (int) y;
    }

    @Override
    public int getDefinedPlotModelWidth() {
        return this.originalPlotModelWidth;
    }

    @Override
    public int getDefinedPlotModelHeight() {
        return this.originalPlotModelHeight;
    }

    /**
     * Takes the plot data object and produces a buffered image.
     *
     * @param stationData
     *            The data
     * @param latitude
     *            the longitude the data will be displayed
     * @param longitude
     *            the latitude the data will be displayed
     * @return A buffered image representing the station data
     */
    @Override
    public synchronized BufferedImage getStationPlot(PlotData stationData,
            double latitude, double longitude) {
        double[] stationLocation = { longitude, latitude };
        double[] stationPixelLocation = this.mapDescriptor
                .worldToPixel(stationLocation);

        if (stationPixelLocation != null) {
            stationPixelLocation[1]--;
            double[] newWorldLocation = this.mapDescriptor
                    .pixelToWorld(stationPixelLocation);
            if (Double.isNaN(newWorldLocation[0])
                    || Double.isNaN(newWorldLocation[1])) {
                return null;
            }
            newWorldLocation[0] = MapUtil.correctLon(newWorldLocation[0]);
            newWorldLocation[1] = MapUtil.correctLat(newWorldLocation[1]);
            this.gc.setStartingGeographicPoint(stationLocation[0],
                    stationLocation[1]);
            this.gc.setDestinationGeographicPoint(newWorldLocation[0],
                    newWorldLocation[1]);
        }

        try {
            boolean discard = false;
            if (python != null) {
                Boolean result = false;
                CheckPlotValidityExecutor task = new CheckPlotValidityExecutor(
                        stationData);
                try {
                    result = python.submitJob(task).get();
                } catch (Exception e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error checking if plot is valid for plot model "
                                    + getPlotModelFilename(),
                            e);
                } finally {
                    if (!result) {
                        return null;
                    }
                }
            }

            StringBuilder imageId = new StringBuilder();

            for (IPlotModelElement iElement : this.plotFields) {
                PlotModelElementDefault element = (PlotModelElementDefault) iElement;
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
                    if (element.getConverter() != null) {
                        speed = element.getConverter().convert(speed);
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

            BufferedImage bufferedImage = createSingleColorImage(color,
                    plotModelHeight, plotModelWidth, true, true);

            if (imageCache != null) {
                imageCache.put(imageId.toString(), bufferedImage);
            }
            return bufferedImage;

        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error:" + e.getLocalizedMessage(), e);
        }

        return null;
    }

    @Override
    public synchronized String getStationMessage(PlotData stationData,
            String dataURI) {
        StringBuilder sampleMessage = new StringBuilder();
        try {
            if (python != null) {
                String result = null;
                SampleTextExecutor task = new SampleTextExecutor(stationData);
                try {
                    result = python.submitJob(task).get();
                } catch (Exception e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error getting sample text for plot model "
                                    + getPlotModelFilename(),
                            e);
                } finally {
                    if (result != null) {
                        sampleMessage.append(result);
                    }
                }
            } else {
                for (IPlotModelElement element : this.sampleFields) {
                    sampleMessage.append(
                            processSampleDirective(stationData, element));
                }
            }

        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error generating sample text with " + plotModelFile, e);
        }

        String message = sampleMessage.toString();
        if (message.length() == 0) {
            message = PlotResource.NO_DATA;
        }

        return message;
    }

    private void addToImageId(PlotData stationData, String parameters,
            StringBuilder imageId) {
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

    private void processTextDirective(PlotData ob,
            PlotModelElementDefault element) throws VizException {
        int dimensions = -1;
        String sValue = null;
        String param = element.parameter;
        Number value = null;
        dimensions = ob.getDimensions(param);
        switch (ob.getType(param)) {
        case FLOAT:
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
                            Unit<?> unit = SimpleUnitFormat
                                    .getInstance(Flavor.ASCII).parseProductUnit(
                                            element.unit, new ParsePosition(0));
                            element.converter = ob.getUnit(param)
                                    .getConverterToAny(unit);
                        } catch (ParserException | UnconvertibleException
                                | IncommensurableException e) {
                            throw new VizException(
                                    "Unable to parse or convert units "
                                            + element.unit + " and "
                                            + ob.getUnit(param).toString(),
                                    e);
                        }
                    }
                    displayValue = element.converter
                            .convert(value.doubleValue());
                } else {
                    displayValue = value.doubleValue();
                }
                if (element.format != null) {
                    StringBuilder sb = new StringBuilder();
                    Formatter testing = new Formatter(sb);
                    testing.format(element.format, displayValue);
                    sValue = sb.toString();
                    testing.close();
                } else {
                    sValue = Double.toString(displayValue);
                }
                sValue = sValue.substring(element.trim);
                // Take the negative sign off negative zero
                if ("-0".equals(sValue.trim())) {
                    sValue = sValue.replace("-", "");
                }
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

    private boolean processBarbDirective(PlotData ob,
            PlotModelElementDefault element) throws VizException {
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
                    Unit<?> unit = SimpleUnitFormat.getInstance(Flavor.ASCII)
                            .parseProductUnit(element.unit,
                                    new ParsePosition(0));
                    element.converter = speedUnit.getConverterToAny(unit);
                } catch (ParserException | UnconvertibleException
                        | IncommensurableException e) {
                    throw new VizException("Unable to parse or convert units "
                            + element.unit + " and " + speedUnit.toString(), e);
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
                    element.winds.barbNode
                            .setNodeValue(Integer.toString(iWindSpeed));
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
            renderArrow(windGust, windDir, speedUnit, element);
            return false;
        }
        return true;
    }

    private void processArrowDirective(PlotData ob,
            PlotModelElementDefault element) throws VizException {
        String[] params = element.parameter.split(",");
        Number magnitude = ob.getNumber(params[0]);
        Unit<?> speedUnit = ob.getUnit(params[0]);
        Number direction = ob.getNumber(params[1]);
        renderArrow(magnitude, direction, speedUnit, element);
    }

    private void renderArrow(Number magnitude, Number direction,
            Unit<?> speedUnit, PlotModelElementDefault element)
            throws VizException {
        double dDir = -9999.0;
        double cMag = -9999.0;
        if (element.unit != null && magnitude != null) {
            if (element.converter == null) {
                try {
                    Unit<?> unit = SimpleUnitFormat.getInstance(Flavor.ASCII)
                            .parseSingleUnit(element.unit,
                                    new ParsePosition(0));
                    element.converter = speedUnit.getConverterToAny(unit);
                } catch (ParserException | UnconvertibleException
                        | IncommensurableException e) {
                    throw new VizException("Unable to parse or convert units "
                            + element.unit + " and " + speedUnit.toString(), e);
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
                element.winds.arrowElement.setAttribute("transform",
                        "rotate(" + dDir + ",0,0)");
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
                element.winds.gustNode.setNodeValue(
                        Integer.toString(((int) Math.round(cMag))));
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

    private void processTableDirective(PlotData ob,
            PlotModelElementDefault element) {

        int dimensions = ob.getDimensions(element.parameter);
        String display = null;
        String[] fields = null;
        switch (ob.getType(element.parameter)) {
        case FLOAT:
        case INT:
        case LONG:
            if (dimensions == 1) {
                Number n = ob.getNumber(element.parameter);
                if ((n != null) && (n.doubleValue() != -9999)) {
                    if ((n.doubleValue() != -9999)
                            && (!Double.isNaN(n.doubleValue()))) {
                        display = n.toString();
                    }
                }
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
        } else if ((fields != null) && (fields.length > 0)) {
            StringBuilder sb = new StringBuilder(fields[fields.length - 1]);
            for (int i = fields.length - 2; i >= 0; i--) {
                sb.append(" ");
                sb.append(fields[i]);
            }
            display = sb.toString().trim();
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

    private String processSampleDirective(PlotData ob,
            IPlotModelElement iElement) throws VizException {
        PlotModelElementDefault element = (PlotModelElementDefault) iElement;
        String sValue = null;
        String parameter = element.parameter;
        switch (ob.getType(parameter)) {
        case FLOAT:
        case INT:
        case LONG:
            Number value = ob.getNumber(parameter);
            if (value != null && value.doubleValue() != -9999.0) {
                double displayValue = 0.0;

                if (element.unit != null) {
                    if (element.converter == null) {
                        try {
                            Unit<?> unit = SimpleUnitFormat
                                    .getInstance(Flavor.ASCII).parseSingleUnit(
                                            element.unit, new ParsePosition(0));
                            element.converter = ob.getUnit(parameter)
                                    .getConverterToAny(unit);
                        } catch (ParserException | UnconvertibleException
                                | IncommensurableException e) {
                            throw new VizException(
                                    "Unable to parse or convert units "
                                            + element.unit + " and "
                                            + ob.getUnit(parameter).toString(),
                                    e);
                        }
                    }
                    displayValue = element.converter
                            .convert(value.doubleValue());
                } else {
                    displayValue = value.doubleValue();
                }
                if (element.format != null) {
                    if ("time".equals(element.format)) {
                        Date d = new Date((long) displayValue);
                        synchronized (SAMPLE_DATE) {
                            SAMPLE_DATE.setTimeZone(TimeUtil.GMT_TIME_ZONE);
                            sValue = SAMPLE_DATE.format(d);
                        }
                    } else if (element.format.startsWith("time:")) {
                        Date d = new Date((long) displayValue);
                        SimpleDateFormat sampleData = new SimpleDateFormat(
                                element.format.substring(5));
                        sampleData.setTimeZone(TimeUtil.GMT_TIME_ZONE);
                        sValue = sampleData.format(d);
                    } else {
                        StringBuilder sb = new StringBuilder();
                        Formatter testing = new Formatter(sb);
                        testing.format(element.format, displayValue);
                        sValue = sb.toString();
                        testing.close();
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
            if (!"?".equals(sValue)) {
                lu = element.lookup.lookup(sValue);
            }
            if (lu != null) {
                sValue = lu.trim();
            }
        }

        if (sValue != null && element.symbol != null) {
            sValue = sValue + element.symbol;
        }

        return sValue + " ";
    }

    private boolean processRangeDirective(PlotData ob,
            PlotModelElementDefault element) throws VizException {

        String sValue = null;
        switch (ob.getType(element.parameter)) {
        case FLOAT:
        case INT:
        case LONG:
            Number value = ob.getNumber(element.parameter);
            if (value != null && value.doubleValue() != -9999.0) {
                double displayValue = 0.0;
                if (element.unit != null) {
                    if (element.converter == null) {
                        try {
                            Unit<?> unit = SimpleUnitFormat
                                    .getInstance(Flavor.ASCII).parseProductUnit(
                                            element.unit, new ParsePosition(0));
                            element.converter = ob.getUnit(element.parameter)
                                    .getConverterToAny(unit);
                        } catch (ParserException | UnconvertibleException
                                | IncommensurableException e) {
                            throw new VizException(
                                    "Unable to parse or convert units "
                                            + element.unit + " and "
                                            + ob.getUnit(element.parameter)
                                                    .toString(),
                                    e);
                        }
                    }
                    displayValue = element.converter
                            .convert(value.doubleValue());
                } else {
                    displayValue = value.doubleValue();
                }

                if (isValidValue(displayValue)) {
                    if (element.format != null) {
                        StringBuilder sb = new StringBuilder();
                        Formatter testing = new Formatter(sb);
                        testing.format(element.format, displayValue);
                        sValue = sb.toString();
                        testing.close();
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
        if (element.lookup != null && sValue != null) {
            String lu = null;
            lu = element.lookup.lookup(sValue);
            if (!lu.isEmpty()) {
                sValue = lu;
            }
        }
        if (sValue != null) {
            element.plotNode.setNodeValue(sValue.substring(element.trim));
            return !sValue.trim().isEmpty();
        } else if (plotMissingData) {
            element.plotNode.setNodeValue("m");
            setPlotMissingData(false);
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
     *            PlotModelElementDefault object
     */
    private void processAvailDirective(PlotModelElementDefault element) {
        element.plotNode.setNodeValue("*");
    }

    private void processNullDirective(PlotModelElementDefault element) {
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

    @Override
    public List<IPlotModelElement> getPlotFields() {
        return Collections.unmodifiableList(this.plotFields);
    }

    /**
     * Convert an array of Numbers to their String representation. Note that
     * indexing may be used on the return array so the output size must match
     * the input size.
     *
     * @param values
     *            An array of Number to convert.
     * @return The converted data. If the input is null, the return will be
     *         null.
     */
    private String[] numberToStringArray(Number[] values) {
        String[] retVal = null;
        if (values != null) {
            retVal = new String[values.length];
            Arrays.fill(retVal, "");
            for (int i = 0; i < values.length; i++) {
                Number n = values[i];
                if ((n.doubleValue() != -9999)
                        && (!Double.isNaN(n.doubleValue()))) {
                    retVal[i] = n.toString();
                }
            }
        }
        return retVal;
    }

    @Override
    public void setLowerLimit(double lowerLimit) {
        this.lowerLimit = lowerLimit;
    }

    @Override
    public void setUpperLimit(double upperLimit) {
        this.upperLimit = upperLimit;
    }

    private boolean isValidValue(Number value) {
        return value != null && value.doubleValue() >= lowerLimit
                && value.doubleValue() <= upperLimit;
    }

    @Override
    public void setPlotMissingData(boolean b) {
        this.plotMissingData = b;
    }

    private static File getTableFile(String fileName) {
        File rval = PathManagerFactory.getPathManager()
                .getStaticFile(IPlotModelFactory.PLOT_MODEL_DIR
                        + IPathManager.SEPARATOR + fileName);
        return rval;
    }

    @Override
    public List<IPlotModelElement> getSampleFields() {
        return Collections.unmodifiableList(this.sampleFields);
    }

    @Override
    public boolean isCachingImages() {
        return imageCache != null;
    }

    @Override
    public String getPlotModelFilename() {
        return this.plotModelFile;
    }

    /**
     * Disposes of the plot model
     */
    @Override
    public void dispose() {
        if (python != null) {
            python.shutdown();
        }
    }

    @Override
    public void clearImageCache() {

    }

    @Override
    public BufferedImage getSamplePlot() {
        return null;
    }

    @Override
    public String getPlugin() {
        return null;
    }

    @Override
    public void savePlotModel() {

    }

    @Override
    public boolean isSingleColor() {
        return true;
    }

    @Override
    public void setConditionFilter(Condition filter) {

    }

    @Override
    public IPlotModelElement addElement(PlotParameterDefinition paremDef) {
        return null;
    }

}
