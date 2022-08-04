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
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.text.ParsePosition;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.Formatter;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.measure.IncommensurableException;
import javax.measure.UnconvertibleException;
import javax.measure.Unit;
import javax.measure.UnitConverter;
import javax.measure.format.ParserException;
import javax.measure.quantity.Speed;

import org.apache.batik.anim.dom.SVGOMGElement;
import org.apache.batik.anim.dom.SVGOMTextElement;
import org.apache.batik.dom.AbstractDocument;
import org.apache.batik.dom.util.DOMUtilities;
import org.eclipse.swt.graphics.RGB;
import org.geotools.referencing.GeodeticCalculator;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.python.concurrent.PythonJobCoordinator;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.point.svg.SVGImageFactory;
import com.raytheon.viz.pointdata.def.Condition;
import com.raytheon.viz.pointdata.def.ConditionalFilter;
import com.raytheon.viz.pointdata.def.PlotParameterDefinition;
import com.raytheon.viz.pointdata.def.PlotParameterDefinitionsManager;
import com.raytheon.viz.pointdata.lookup.IAbstractLookupTable;
import com.raytheon.viz.pointdata.python.CheckPlotValidityExecutor;
import com.raytheon.viz.pointdata.python.PlotPythonScript;
import com.raytheon.viz.pointdata.python.PlotPythonScriptFactory;
import com.raytheon.viz.pointdata.python.SampleTextExecutor;
import com.raytheon.viz.pointdata.rsc.PlotResource;

import tec.uom.se.AbstractUnit;
import tec.uom.se.format.SimpleUnitFormat;

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
 * Apr 15, 2019  7596     lsingh      Upgraded Units Framework to JSR-363. Handled
 *                                    unit conversions.
 * Sep 03, 2019  67550    ksunil      correctLat/Lon code to fix IllegalArgument exception
 * Nov 01, 2019  71272    ksunil      code re-factoring to process richer .svg files. Original
 *                                      PlotModelFactory was renamed to PlotModelFactoryDefault.
 *                                      And code added to PlotModelFactory.java to handle Plot customizations.
 * Dec 10, 2019  72280    ksunil      new methods to grab existing conditional filters and models, conditional filter
 * Jan 07, 2020  73083    ksunil      changes to honor sub directories while searching for .svg files
 * Jan 13, 2020  73084    ksunil      addElement has a new signature/implementation to fix issues identified 
 *                                     during testing of 73084
 * Feb 05, 2020  74587    ksunil      sorted display of plugins, svg and conditional filters. Code to ignore plotMode NONE
 * Feb 18, 2020  74587    ksunil      Fixed a NPE in the sample plot when plotIndex is involved.
 * Mar 02, 2020  75528    ksunil      Changed enum PLOT to VISIBLE. Use svg's built in visibility attribute. Fixed NPE issues
 *                                      while adding a new BARB/ARROWUV parameter ("<g>" elements in .svg)
 * </pre>
 *
 * @author BRock97
 */
public class PlotModelFactory extends SVGImageFactory
        implements IPlotModelFactory {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(PlotModelFactory.class);

    private static final String P_ATTRIBUTE = "plotParam";

    private static final String P_MODE = "plotMode";

    private static final String PLUGIN_ATTRIBUTE = "plugin";

    private final SimpleDateFormat SAMPLE_DATE = new SimpleDateFormat("HHmm");

    private static final int NUM_POOL_THREADS = 1;

    private static final Pattern PYTHON_LINE_PATTERN = Pattern
            .compile("^(\\s+)(\\S.*)?");

    private int plotModelWidth;

    private int plotModelHeight;

    private final int originalPlotModelWidth;

    private final int originalPlotModelHeight;

    private final String plugin;

    private final Element svgRoot;

    private final GeodeticCalculator gc;

    private final IMapDescriptor mapDescriptor;

    private final List<PlotModelElement> plotFields;

    private final List<PlotModelElement> sampleFields;

    private double lowerLimit = -9999.0;

    private double upperLimit = -9999.0;

    private boolean plotMissingData = false;

    private Map<String, BufferedImage> imageCache = null;

    protected final String plotModelFile;

    protected PythonJobCoordinator<PlotPythonScript> python;

    private static List<String> markers = new ArrayList<>();

    private static Map<String, Set<String>> pluginsAndModelsInfo = new TreeMap<>();

    private static Map<String, Set<LocalizationFile>> pluginsAndCondFiltersInfo = new TreeMap<>();

    private Condition conditionFilter;

    private static SingleTypeJAXBManager<ConditionalFilter> condFilterJaxb = SingleTypeJAXBManager
            .createWithoutException(ConditionalFilter.class);

    public static enum DisplayType {
        TEXT, BARB, TABLE, RANGE, ARROW, ARROWUV, MARKER
    }

    public static enum DisplayMode {
        // HIDDEN = in the GUI but unchecked, NONE = not in the GUI at all
        VISIBLE, SAMPLE, HIDDEN, NONE
    }

    public PlotModelFactory(IMapDescriptor mapDescriptor, String plotModelFile)
            throws VizException {
        super(plotModelFile(plotModelFile));
        this.plotModelFile = plotModelFile;
        this.plotFields = new ArrayList<>();
        this.sampleFields = new ArrayList<>();

        this.gc = new GeodeticCalculator(mapDescriptor.getCRS());
        this.mapDescriptor = mapDescriptor;

        this.svgRoot = document.getDocumentElement();
        this.originalPlotModelWidth = Integer
                .parseInt(svgRoot.getAttributeNS(null, "width"));
        this.originalPlotModelHeight = Integer
                .parseInt(svgRoot.getAttributeNS(null, "height"));
        this.plugin = svgRoot.getAttribute(PLUGIN_ATTRIBUTE);
        this.plotModelWidth = this.originalPlotModelWidth;
        this.plotModelHeight = this.originalPlotModelHeight;

        int displayedElementCount = 0;

        Element plot = document.getElementById("plotData");
        NodeList plotElements = plot.getChildNodes();

        for (int i = 0; i < plotElements.getLength(); i++) {
            if (Node.ELEMENT_NODE == plotElements.item(i).getNodeType()) {
                Element plotElement = (Element) plotElements.item(i);
                if (plotElement.hasAttribute(P_ATTRIBUTE)) {
                    PlotModelElement thisElement = parseElement(plotElement);
                    if (thisElement != null) {
                        DisplayMode mode = thisElement.getMode();
                        if (mode != DisplayMode.SAMPLE) {
                            this.plotFields.add(thisElement);
                            if (mode != DisplayMode.NONE) {
                                displayedElementCount += 1;
                            }
                        } else {
                            this.sampleFields.add(thisElement);
                            thisElement.setValue(" ");
                        }
                    }
                }
            }
        }

        if (displayedElementCount <= 3) {
            /*
             * Don't use image caching if more than 3 elements are used, with
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

    private PlotModelElement parseElement(Element plotElement) {
        PlotModelElement thisElement = new PlotModelElement(plotElement);

        String paramDisplayName = plotElement.getAttribute(P_ATTRIBUTE);
        if (DisplayMode.NONE.name().equals(plotElement.getAttribute(P_MODE))) {
            return null;
        }
        PlotParameterDefinitionsManager defsManager = PlotParameterDefinitionsManager
                .getInstance();
        PlotParameterDefinition paramDef = defsManager.getDefinitions(plugin)
                .getParamDef(paramDisplayName);
        if (paramDef == null) {
            statusHandler.error("Parameter '" + paramDisplayName + "' in "
                    + getPlotModelFilename() + " is not defined in "
                    + defsManager.getDefinitionsPath(plugin));
            thisElement.setValue(" ");
            return null;
        }

        thisElement.setParamDef(paramDef);
        return thisElement;
    }

    @Override
    public void clearImageCache() {
        if (imageCache != null) {
            imageCache.clear();
        }
    }

    @Override
    public void setColor(RGB color) {
        boolean defaultColorUsed = false;
        for (IPlotModelElement pme : plotFields) {
            if (pme.processDefaultColor(color)) {
                defaultColorUsed = true;
            }
        }

        if (defaultColorUsed) {
            clearImageCache();
        }
    }

    @Override
    public void setLineWidth(int width) {
        boolean defaultWidthUsed = false;
        for (IPlotModelElement pme : plotFields) {
            if (pme.processDefaultLineWidth(width)) {
                defaultWidthUsed = true;
            }
        }

        if (defaultWidthUsed) {
            clearImageCache();
        }
    }

    @Override
    public void setLineStyle(LineStyle style) {
        boolean lineStyleUsed = false;
        for (IPlotModelElement pme : plotFields) {
            if (pme.setLineStyle(style)) {
                lineStyleUsed = true;
            }
        }

        if (lineStyleUsed) {
            clearImageCache();
        }
    }

    @Override
    public void setPlotDimensions(long x, long y) {
        clearImageCache();
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

    @Override
    public synchronized BufferedImage getStationPlot(PlotData stationData,
            double latitude, double longitude) {
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

        return (getStationPlotInternal(stationData, latitude, longitude));
    }

    @Override
    public BufferedImage getSamplePlot() {
        SamplePlotData sampleData = new SamplePlotData();
        for (IPlotModelElement element : getPlotFields()) {
            PlotParameterDefinition paramDef = element.getParamDef();
            String param = paramDef.getParamName();
            String sampleValue = paramDef.getSampleValue();

            Unit<?> unit;
            String unitStr = paramDef.getUnit();
            if (unitStr != null) {
                try {
                    unit = SimpleUnitFormat
                            .getInstance(SimpleUnitFormat.Flavor.ASCII)
                            .parseProductUnit(unitStr, new ParsePosition(0));
                } catch (ParserException e) {
                    statusHandler.error("Error parsing units '" + unitStr
                            + "' for " + paramDef.getDisplayName(), e);
                    continue;
                }
            } else {
                unit = AbstractUnit.ONE;
            }

            String[] params = param.split(",");
            if (params.length > 1) {
                String[] sampleValues = sampleValue.split(",");
                if (params.length != sampleValues.length) {
                    statusHandler.error(
                            "Number of parameters doesn't match number of sample values for "
                                    + paramDef.getDisplayName());
                    continue;
                } else {
                    for (int i = 0; i < params.length; ++i) {
                        sampleData.addData(params[i], sampleValues[i], unit);
                    }
                }
            } else {
                sampleData.addData(param, sampleValue, unit);
            }
        }
        return getStationPlotInternal(sampleData, -40.0, 90.0);
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
    private BufferedImage getStationPlotInternal(IPlotData stationData,
            double latitude, double longitude) {

        if (conditionFilter != null) {
            try {
                if (!conditionFilter.evaluate(stationData, this.plugin)) {
                    return null;
                }
            } catch (VizException e1) {
                statusHandler.warn("Filter condition threw an exception.", e1);
                return null;
            }
        }

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
            StringBuilder imageId = new StringBuilder();

            Set<RGB> elementRgbs = new HashSet<>();
            for (IPlotModelElement element : this.plotFields) {
                if (element.getMode() != DisplayMode.VISIBLE) {
                    element.setValue(" ");
                    continue;
                }

                element.processConditionalColor(stationData, plugin);

                RGB elementRgb = element.getColor();
                if (elementRgb != null) {
                    elementRgbs.add(elementRgb);
                }

                PlotParameterDefinition paramDef = element.getParamDef();
                String param = paramDef.getParamName();

                boolean valid = true;
                boolean required = element.isRequired();
                switch (paramDef.getDisplayType()) {
                case TEXT:
                    this.processTextDirective(stationData, element);
                    addToImageId(stationData, param, imageId);
                    break;
                case BARB:
                case ARROW:
                    valid = this.processBarbDirective(stationData, element);
                    // normalize speed before adding to id so all identical
                    // barbs will share plots
                    String[] windParams = param.split(",");
                    double speed = stationData.getNumber(windParams[0])
                            .doubleValue();
                    double dir = stationData.getNumber(windParams[1])
                            .doubleValue();
                    UnitConverter converter = element.getConverter();
                    if (converter != null) {
                        speed = converter.convert(speed);
                    }
                    imageId.append(windNormalizer(speed));
                    // Accurate to a fourth a degree.
                    imageId.append((int) (dir - gc.getAzimuth()) * 4);
                    if (windParams.length == 3) {
                        imageId.append(stationData.getNumber(windParams[2]));
                    }
                    addToImageId(stationData, param.split(",", 2)[1], imageId);
                    break;
                case ARROWUV:
                    addToImageId(stationData, param, imageId);
                    this.processArrowDirective(stationData, element);
                    break;
                case TABLE:
                    this.processTableDirective(stationData, element);
                    addToImageId(stationData, param, imageId);
                    break;
                case RANGE:
                    valid = this.processRangeDirective(stationData, element);
                    addToImageId(stationData, param, imageId);
                    break;
                case MARKER:
                    imageId.append(element.getValue());
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

            BufferedImage bufferedImage = createImage(elementRgbs,
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

    private void addToImageId(IPlotData stationData, String parameters,
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

    private void processTextDirective(IPlotData ob, IPlotModelElement element)
            throws VizException {
        int dimensions = -1;
        String sValue = null;
        PlotParameterDefinition paramDef = element.getParamDef();
        String param = paramDef.getParamName();
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
                int index = paramDef.getIndex();
                if (index != -1 && values != null && index < values.length) {
                    value = values[index];
                }
            }
            if (isValidValue(value)) {
                double displayValue = 0.0;
                String unitStr = paramDef.getUnit();
                if (unitStr != null) {
                    UnitConverter converter = element.getConverter();
                    if (converter == null) {
                        try {
                            Unit<?> unit = SimpleUnitFormat
                                    .getInstance(SimpleUnitFormat.Flavor.ASCII)
                                    .parseProductUnit(unitStr,
                                            new ParsePosition(0));
                            converter = ob.getUnit(param)
                                    .getConverterToAny(unit);
                        } catch (ParserException | UnconvertibleException
                                | IncommensurableException e) {
                            throw new VizException(
                                    "Unable to parse and/or convert units"
                                            + unitStr + " and "
                                            + ob.getUnit(param).toString(),
                                    e);
                        }
                    }
                    displayValue = converter.convert(value.doubleValue());
                } else {
                    displayValue = value.doubleValue();
                }
                String format = paramDef.getFormat();
                if (format != null) {
                    StringBuilder sb = new StringBuilder();
                    Formatter testing = new Formatter(sb);
                    testing.format(format, displayValue);
                    sValue = sb.toString();
                    testing.close();
                } else {
                    sValue = Double.toString(displayValue);
                }
                sValue = sValue.substring(paramDef.getTrim());
                // Take the negative sign off negative zero
                if ("-0".equals(sValue.trim())) {
                    sValue = sValue.replace("-", "");
                }
                element.setValue(sValue);
            } else if (plotMissingData) {
                element.setValue("m");
            } else {
                element.setValue(" ");
            }
            break;
        case STRING:
            element.setValue(ob.getString(param));
            break;
        default:
            element.setValue(" ");
        }
    }

    private boolean processBarbDirective(IPlotData ob,
            IPlotModelElement element) throws VizException {
        PlotParameterDefinition paramDef = element.getParamDef();
        String[] windParams = paramDef.getParamName().split(",");
        Number windSpeed = ob.getNumber(windParams[0]);
        Unit<Speed> speedUnit = ob.getUnit(windParams[0]).asType(Speed.class);
        Number windDir = ob.getNumber(windParams[1]);
        Number windGust = null;
        if (windParams.length == 3) {
            windGust = ob.getNumber(windParams[2]);
        }
        double dWindDir = -9999.0;
        double cWindSpeed = -9999.0;

        String unitStr = paramDef.getUnit();
        if (unitStr != null && isValidValue(windSpeed)
                && windSpeed.intValue() != -9999) {
            UnitConverter converter = element.getConverter();
            if (converter == null) {
                try {
                    Unit<Speed> unit = SimpleUnitFormat
                            .getInstance(SimpleUnitFormat.Flavor.ASCII)
                            .parseProductUnit(unitStr, new ParsePosition(0))
                            .asType(Speed.class);
                    converter = speedUnit.getConverterTo(unit);
                } catch (ParserException e) {
                    throw new VizException("Unable parse units ", e);
                }
            }
            cWindSpeed = converter.convert(windSpeed.doubleValue());
        } else if (windSpeed != null) {
            cWindSpeed = windSpeed.doubleValue();
        }
        if (windDir != null) {
            dWindDir = windDir.doubleValue();
        }

        PlotWindElement winds = element.getWinds();
        boolean valid = isValidValue(cWindSpeed);
        if (valid) {
            if (winds.barbElement != null) {
                if (cWindSpeed == -9999.0 && plotMissingData) {
                    // TODO need to allow for scaling?
                    winds.barbElement.removeAttribute("transform");
                    winds.barbNode.setNodeValue("m");
                } else if (cWindSpeed >= 0 && cWindSpeed < 2.5) {
                    winds.barbElement.removeAttribute("transform");
                    winds.barbNode.setNodeValue("0");
                } else if (cWindSpeed >= 2.5 && dWindDir != -9999.0) {
                    dWindDir -= this.gc.getAzimuth();
                    int iWindSpeed = this.windNormalizer(cWindSpeed);
                    winds.barbElement.setAttribute("transform",
                            "rotate(" + dWindDir + ",0,0)");
                    winds.barbNode.setNodeValue(Integer.toString(iWindSpeed));
                } else {
                    winds.barbElement.removeAttribute("transform");
                    winds.barbNode.setNodeValue(" ");
                }
            }
            renderArrow(windGust, windDir, speedUnit, element);
        } else {
            if (winds.barbElement != null) {
                winds.barbNode.setNodeValue(" ");
            }
            renderArrow(windGust, windDir, speedUnit, element);
        }
        element.setWinds(winds);

        return valid;
    }

    private void processArrowDirective(IPlotData ob, IPlotModelElement element)
            throws VizException {
        String[] params = element.getParamDef().getParamName().split(",");
        Number magnitude = ob.getNumber(params[0]);
        Unit<Speed> speedUnit = ob.getUnit(params[0]).asType(Speed.class);
        Number direction = ob.getNumber(params[1]);
        renderArrow(magnitude, direction, speedUnit, element);
    }

    private void renderArrow(Number magnitude, Number direction,
            Unit<Speed> speedUnit, IPlotModelElement element)
            throws VizException {
        double dDir = -9999.0;
        double cMag = -9999.0;
        String unitStr = element.getParamDef().getUnit();
        if (unitStr != null && magnitude != null) {
            UnitConverter converter = element.getConverter();
            if (converter == null) {
                try {
                    Unit<Speed> unit = SimpleUnitFormat
                            .getInstance(SimpleUnitFormat.Flavor.ASCII)
                            .parseSingleUnit(unitStr, new ParsePosition(0))
                            .asType(Speed.class);
                    converter = speedUnit.getConverterTo(unit);
                } catch (ParserException e) {
                    throw new VizException("Unable parse units ", e);
                }
            }
            if (isValidValue(magnitude)) {
                cMag = converter.convert(magnitude.doubleValue());
            }
        } else if (magnitude != null) {
            cMag = magnitude.doubleValue();
        }
        if (direction != null) {
            dDir = direction.doubleValue();
        }

        PlotWindElement winds = element.getWinds();
        if (dDir != -9999.0 && cMag != -9999.0) {
            dDir -= this.gc.getAzimuth();

            String arrowType = null;
            if (winds.arrowElement != null) {
                winds.arrowElement.setAttribute("transform",
                        "rotate(" + dDir + ",0,0)");
                arrowType = winds.arrowElement.getAttribute("arrowtype");
                if ("arrow1".equals(arrowType) || "arrow2".equals(arrowType)) {
                    winds.arrowNode.setNodeValue(arrowType);
                } else {
                    winds.arrowNode.setNodeValue("arrow");
                }
            }
            if (winds.gustElement != null) {
                double len = 32.0;
                if ("arrow1".equals(arrowType) || "arrow2".equals(arrowType)) {
                    try {
                        len = Double.parseDouble(winds.gustY);
                    } catch (NumberFormatException nfe) {
                        len = 32.0;
                    }
                }

                double rWindDir = Math.toRadians(dDir + 180.0);

                long x = Math.round(len * Math.sin(rWindDir));
                long y = Math.round(len * Math.cos(rWindDir)) * -1;

                winds.gustElement.setAttribute("x", Long.toString(x));
                winds.gustElement.setAttribute("y", Long.toString(y));
                winds.gustNode.setNodeValue(
                        Integer.toString(((int) Math.round(cMag))));
            }

        } else {
            if (winds.arrowElement != null) {
                winds.arrowElement.removeAttribute("transform");
                winds.arrowNode.setNodeValue(" ");
            }
            if (winds.gustElement != null) {
                winds.gustNode.setNodeValue(" ");
            }
        }
        element.setWinds(winds);
    }

    private void processTableDirective(IPlotData ob,
            IPlotModelElement element) {
        PlotParameterDefinition paramDef = element.getParamDef();
        String parameter = paramDef.getParamName();

        int dimensions = ob.getDimensions(parameter);
        String display = null;
        String[] fields = null;
        switch (ob.getType(parameter)) {
        case FLOAT:
        case INT:
        case LONG:
            if (dimensions == 1) {
                Number n = ob.getNumber(parameter);
                if ((n != null) && (n.doubleValue() != -9999)) {
                    if ((n.doubleValue() != -9999)
                            && (!Double.isNaN(n.doubleValue()))) {
                        display = n.toString();
                    }
                }
            } else if (dimensions == 2) {
                Number[] values = ob.getNumberAllLevels(parameter);
                fields = numberToStringArray(values);
            }
            break;
        case STRING:
            if (dimensions == 1) {
                display = ob.getString(parameter);
            } else if (dimensions == 2) {
                fields = ob.getStringAllLevels(parameter);
            }
            break;
        default:
            element.setValue(" ");
        }
        int index = paramDef.getIndex();

        if (fields != null) {
            if (index != -1 && index < fields.length) {
                display = fields[index];
            } else if (paramDef.getRanking() != null) {
                display = paramDef.getRanking().getRankedField(fields);
            } else if (fields.length > 0) {
                StringBuilder sb = new StringBuilder(fields[fields.length - 1]);
                for (int i = fields.length - 2; i >= 0; i--) {
                    sb.append(" ");
                    sb.append(fields[i]);
                }
                display = sb.toString().trim();
            }
        }

        IAbstractLookupTable lookup = paramDef.getActualLookupTable();
        if (lookup != null) {
            display = lookup.lookup(display);
        }

        if (display != null) {
            element.setValue(display);
        } else {
            element.setValue(" ");
        }
    }

    private String processSampleDirective(PlotData ob,
            IPlotModelElement element) throws VizException {
        String sValue = null;
        PlotParameterDefinition paramDef = element.getParamDef();
        String parameter = paramDef.getParamName();
        switch (ob.getType(parameter)) {
        case FLOAT:
        case INT:
        case LONG:
            Number value = ob.getNumber(parameter);
            if (value != null && value.doubleValue() != -9999.0) {
                double displayValue = 0.0;

                String unitStr = paramDef.getUnit();
                if (unitStr != null) {
                    UnitConverter converter = element.getConverter();
                    if (converter == null) {
                        try {
                            Unit<?> unit = SimpleUnitFormat
                                    .getInstance(SimpleUnitFormat.Flavor.ASCII)
                                    .parseSingleUnit(unitStr,
                                            new ParsePosition(0));
                            converter = ob.getUnit(parameter)
                                    .getConverterToAny(unit);
                        } catch (ParserException | IncommensurableException
                                | UnconvertibleException e) {
                            throw new VizException(
                                    "Unable to parse or convert units "
                                            + unitStr + " and "
                                            + ob.getUnit(parameter).toString(),
                                    e);
                        }
                    }
                    displayValue = converter.convert(value.doubleValue());
                } else {
                    displayValue = value.doubleValue();
                }

                String format = paramDef.getFormat();
                if (format != null) {
                    if ("time".equals(format)) {
                        Date d = new Date((long) displayValue);
                        synchronized (SAMPLE_DATE) {
                            SAMPLE_DATE.setTimeZone(TimeUtil.GMT_TIME_ZONE);
                            sValue = SAMPLE_DATE.format(d);
                        }
                    } else if (format.startsWith("time:")) {
                        Date d = new Date((long) displayValue);
                        SimpleDateFormat sampleData = new SimpleDateFormat(
                                format.substring(5));
                        sampleData.setTimeZone(TimeUtil.GMT_TIME_ZONE);
                        sValue = sampleData.format(d);
                    } else {
                        StringBuilder sb = new StringBuilder();
                        Formatter testing = new Formatter(sb);
                        testing.format(format, displayValue);
                        sValue = sb.toString();
                        testing.close();
                    }
                } else {
                    sValue = Double.toString(displayValue);
                }
                sValue = sValue.substring(paramDef.getTrim());
            } else {
                sValue = "?";
            }
            break;
        case STRING:
            sValue = ob.getString(parameter);
            break;
        }

        IAbstractLookupTable lookup = paramDef.getActualLookupTable();
        if (lookup != null && sValue != null) {
            String lu = null;
            if (!"?".equals(sValue)) {
                lu = lookup.lookup(sValue);
            }
            if (lu != null) {
                sValue = lu.trim();
            }
        }

        String symbol = element.getSymbol();
        if (sValue != null && symbol != null) {
            sValue = sValue + symbol;
        }

        return sValue + " ";
    }

    private boolean processRangeDirective(IPlotData ob,
            IPlotModelElement element) throws VizException {
        PlotParameterDefinition paramDef = element.getParamDef();
        String parameter = paramDef.getParamName();
        String sValue = null;
        switch (ob.getType(parameter)) {
        case FLOAT:
        case INT:
        case LONG:
            Number value = ob.getNumber(parameter);
            if (value != null && value.doubleValue() != -9999.0) {
                double displayValue = 0.0;
                String unitStr = paramDef.getUnit();
                if (unitStr != null) {
                    UnitConverter converter = element.getConverter();
                    if (converter == null) {
                        try {
                            Unit<?> unit = SimpleUnitFormat
                                    .getInstance(SimpleUnitFormat.Flavor.ASCII)
                                    .parseProductUnit(unitStr,
                                            new ParsePosition(0));
                            converter = ob.getUnit(parameter)
                                    .getConverterToAny(unit);
                        } catch (ParserException | IncommensurableException
                                | UnconvertibleException e) {
                            throw new VizException(
                                    "Unable to parse or convert units "
                                            + unitStr + " and "
                                            + ob.getUnit(parameter).toString(),
                                    e);
                        }
                    }
                    displayValue = converter.convert(value.doubleValue());
                } else {
                    displayValue = value.doubleValue();
                }

                if (isValidValue(displayValue)) {
                    String format = paramDef.getFormat();
                    if (format != null) {
                        StringBuilder sb = new StringBuilder();
                        Formatter testing = new Formatter(sb);
                        testing.format(format, displayValue);
                        sValue = sb.toString();
                        testing.close();
                    } else {
                        sValue = Double.toString(displayValue);
                    }
                }
            }
            break;
        case STRING:
            sValue = ob.getString(parameter);
            break;
        default:
            element.setValue(" ");
        }

        IAbstractLookupTable lookup = paramDef.getActualLookupTable();
        if (lookup != null && sValue != null) {
            String lu = null;
            lu = lookup.lookup(sValue);
            if (!lu.isEmpty()) {
                sValue = lu;
            }
        }
        if (sValue != null) {
            element.setValue(sValue.substring(paramDef.getTrim()));
            return !sValue.trim().isEmpty();
        } else if (plotMissingData) {
            element.setValue("m");
            setPlotMissingData(false);
            return true;
        } else {
            element.setValue(" ");
            return false;
        }
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

    @Override
    public List<IPlotModelElement> getSampleFields() {
        return Collections.unmodifiableList(this.sampleFields);
    }

    @Override
    public String getPlugin() {
        return plugin;
    }

    @Override
    public boolean isCachingImages() {
        return imageCache != null;
    }

    @Override
    public String getPlotModelFilename() {
        return this.plotModelFile;
    }

    @Override
    public void dispose() {
        if (python != null) {
            python.shutdown();
        }
    }

    @Override
    public void savePlotModel() {
        /*
         * TODO remove attributes that we don't care about that are
         * auto-populating (e.g. preserveAspectRatio="xMidYMid meet")...and
         * maybe remove attributes that are default values?
         */

        for (IPlotModelElement plotElement : getPlotFields()) {
            plotElement.prepareForSave();
        }

        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext context = pm.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.USER);
        String path = LocalizationUtil.join(IPlotModelFactory.PLOT_MODEL_DIR,
                getPlotModelFilename());
        ILocalizationFile lFile = PathManagerFactory.getPathManager()
                .getLocalizationFile(context, path);
        try (SaveableOutputStream sos = lFile.openOutputStream();
                OutputStreamWriter writer = new OutputStreamWriter(sos)) {
            DOMUtilities.writeDocument(document, writer);
            writer.close();
            sos.save();
        } catch (IOException | LocalizationException e) {
            statusHandler.error("Error saving plot model to file: " + path, e);
        }
    }

    @Override
    public IPlotModelElement addElement(PlotParameterDefinition paramDef) {
        Element plot = document.getElementById("plotData");

        /*
         * If you are adding a BARB or ARROWUV you need to create a "g" element
         * and add Arrow/Barb/Gust in majority of the cases. there are few
         * Arrow/Barb and a couple of ArrowUVs (only Arrow for direction)
         */

        Element element = null;
        if (paramDef.getDisplayType() == DisplayType.BARB
                || paramDef.getDisplayType() == DisplayType.ARROWUV) {
            element = new SVGOMGElement(null, (AbstractDocument) document);
            element.setAttribute(P_ATTRIBUTE, paramDef.getDisplayName());
            element.setAttribute("id", paramDef.getDisplayName());

            long commaCnt = paramDef.getParamName().chars()
                    .filter(num -> num == ',').count();

            // we always have Arrow
            element.appendChild(getArrowDirectionElement());

            // don't put BARB for Arrow requests
            if (paramDef.getDisplayType() == DisplayType.BARB) {
                element.appendChild(getBarbElement());
            }

            if (commaCnt == 2) {
                element.appendChild(getGustElement());
            }
        } else {

            element = new SVGOMTextElement(null, (AbstractDocument) document);
            element.setAttribute(P_ATTRIBUTE, paramDef.getDisplayName());

            element.setAttribute("id", paramDef.getDisplayName());
            if (paramDef.getSampleValue() != null) {
                element.setTextContent(paramDef.getSampleValue());
            } else {
                element.setTextContent(" ");
            }
            if (paramDef.getSvgClass() != null) {
                element.setAttribute("class", paramDef.getSvgClass());
            }

        }
        plot.appendChild(element);
        PlotModelElement pme = new PlotModelElement(element, true);
        plotFields.add(pme);

        return pme;
    }

    private Element getArrowDirectionElement() {
        SVGOMTextElement element = new SVGOMTextElement(null,
                (AbstractDocument) document);

        element.setAttribute("id", "windVaneText");
        element.setAttribute("class", "arrow");
        element.setTextContent("0");
        element.setAttribute("x", "0");
        element.setAttribute("y", "0");
        return element;
    }

    private Element getBarbElement() {
        SVGOMTextElement element = new SVGOMTextElement(null,
                (AbstractDocument) document);

        element.setAttribute("id", "windArrowText");
        element.setAttribute("class", "barb");
        element.setTextContent("arrow");
        element.setAttribute("x", "0");
        element.setAttribute("y", "0");
        return element;
    }

    private Element getGustElement() {
        SVGOMTextElement element = new SVGOMTextElement(null,
                (AbstractDocument) document);

        element.setAttribute("id", "windGustText");
        element.setAttribute("class", "text");
        element.setTextContent("0");
        element.setAttribute("x", "0");
        element.setAttribute("y", "32");
        element.setAttribute("style", "text-anchor: middle");
        return element;
    }

    @Override
    public RGB getColor() {
        return null;
    }

    /**
     * utility to decide if the plot model file uses the newer style syntax.
     * used mainly in plot customization code
     *
     * @return true or false
     */

    public static boolean isNewSVGFormat(String fileName) throws VizException {

        Document doc = loadLocalizationSVG(plotModelFile(fileName));
        String plugin = doc.getDocumentElement().getAttribute(PLUGIN_ATTRIBUTE);
        if (plugin == null || plugin.isEmpty()) {
            return false;
        }
        return true;
    }

    public static String findSVGPluginName(String fileName)
            throws VizException {

        Document doc = loadLocalizationSVG(fileName);
        return (doc.getDocumentElement().getAttribute(PLUGIN_ATTRIBUTE));

    }

    /**
     * Returns the plugin attribute defined in the .xml filter file
     *
     * @param LocalizationFile
     * @return the plugin name or null if plugin attribute is not available
     */

    public static String findFilterPlugin(LocalizationFile lFile)
            throws VizException {

        ConditionalFilter conditionalFilter;
        try (InputStream is = lFile.openInputStream()) {
            conditionalFilter = condFilterJaxb.unmarshalFromInputStream(is);
            conditionalFilter.setLocalizationFile(lFile);
            return conditionalFilter.getPlugin();
        } catch (SerializationException | LocalizationException
                | IOException e) {
            statusHandler.warn("Unable to open filter file: " + lFile.getName(),
                    e);
            return null;
        }

    }

    /**
     * Returns a map of plugin names and the associated model names for plugin
     *
     * @return map of Plugin-> Set of ModelNames
     */

    public static synchronized Map<String, Set<String>> getPluginsAndModels()
            throws VizException {
        if (pluginsAndModelsInfo.isEmpty()) {
            String[] extensions = new String[] { ".svg" };

            List<LocalizationFile> localFiles = new ArrayList<>();
            LocalizationContext[] contexts = PathManagerFactory.getPathManager()
                    .getLocalSearchHierarchy(LocalizationType.CAVE_STATIC);
            localFiles.addAll(Arrays.asList(PathManagerFactory.getPathManager()
                    .listFiles(contexts, IPlotModelFactory.PLOT_MODEL_DIR,
                            extensions, true, true)));

            for (LocalizationFile file : localFiles) {
                String plugin = findSVGPluginName(file.getPath());
                if (plugin != null && !plugin.isEmpty()) {
                    /*
                     * There can be .svg files as plotModels/foo1.svg and/or
                     * plotModels/subDir1/foo2.svg. In the second case, model(s)
                     * list displayed will say "subDir/foo2.svg".
                     *
                     */
                    String shortName = file.getPath()
                            .substring(LocalizationUtil
                                    .join(IPlotModelFactory.PLOT_MODEL_DIR, "")
                                    .length());
                    if (!pluginsAndModelsInfo.containsKey(plugin)) {
                        pluginsAndModelsInfo.put(plugin,
                                new TreeSet<>(Arrays.asList(shortName)));
                    } else {
                        pluginsAndModelsInfo.get(plugin).add(shortName);
                    }
                }

            }
        }
        return pluginsAndModelsInfo;
    }

    /**
     * Returns a map of plugin names and the set of conditional filter files
     * defined for each plugin.
     *
     * @return map of Plugin-> Set of conditional filter files
     */

    public static synchronized Map<String, Set<LocalizationFile>> getPluginsAndCondFilters()
            throws VizException {
        if (pluginsAndCondFiltersInfo.isEmpty()) {
            String[] extensions = new String[] { ".xml" };

            List<LocalizationFile> localFiles = new ArrayList<>();
            LocalizationContext[] contexts = PathManagerFactory.getPathManager()
                    .getLocalSearchHierarchy(LocalizationType.CAVE_STATIC);
            localFiles.addAll(Arrays.asList(
                    PathManagerFactory.getPathManager().listFiles(contexts,
                            LocalizationUtil.join(
                                    IPlotModelFactory.PLOT_MODEL_DIR,
                                    IPlotModelFactory.PLOT_FILTERS_DIR),
                            extensions, true, true)));

            for (LocalizationFile file : localFiles) {
                String plugin = findFilterPlugin(file);

                if (plugin != null && !plugin.isEmpty()) {
                    if (!pluginsAndCondFiltersInfo.containsKey(plugin)) {
                        pluginsAndCondFiltersInfo.put(plugin,
                                new TreeSet<>(Arrays.asList(file)));
                    } else {
                        pluginsAndCondFiltersInfo.get(plugin).add(file);
                    }
                }
            }
        }
        return pluginsAndCondFiltersInfo;
    }

    /**
     * returns the marker names defined in MarkerSymbols.svg
     * 
     * @return true or false
     */

    public static synchronized String[] findMarkerSymbolNames()
            throws VizException {
        if (markers.isEmpty()) {
            Document doc = loadLocalizationSVG(
                    plotModelFile(MARKER_SYMBOL_FILE));
            NodeList scriptNodes = doc.getDocumentElement()
                    .getElementsByTagName("glyph");
            // Default "no marker" tag.
            markers.add(" ");
            for (int i = 0; i < scriptNodes.getLength(); ++i) {
                Element e = (Element) scriptNodes.item(i);
                markers.add(e.getAttribute("glyph-name"));
            }
            markers = Collections.unmodifiableList(markers);
        }
        return markers.toArray(new String[markers.size()]);
    }

    @Override
    public boolean isSingleColor() {
        return false;
    }

    @Override
    public void setConditionFilter(Condition filter) {
        this.conditionFilter = filter;

    }
}
