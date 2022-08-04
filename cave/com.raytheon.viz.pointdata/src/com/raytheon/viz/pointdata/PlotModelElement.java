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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import javax.measure.UnitConverter;
import javax.xml.bind.JAXBException;

import org.apache.commons.lang.StringEscapeUtils;
import org.eclipse.swt.graphics.RGB;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.google.common.collect.ImmutableSet;
import com.raytheon.uf.common.serialization.MarshalOptions;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.pointdata.PlotModelFactory.DisplayMode;
import com.raytheon.viz.pointdata.PlotModelFactory.DisplayType;
import com.raytheon.viz.pointdata.def.ConditionalColor;
import com.raytheon.viz.pointdata.def.PlotParameterDefinition;
import com.raytheon.viz.pointdata.def.ui.EditPlotModelComposite;
import com.raytheon.viz.pointdata.def.ui.EditPlotModelComposite.TextAnchor;

/**
 * PlotModelElement object to handle the new plot customization code
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 10/31/2019   71272      K Sunil     Initial Creation
 * 01/13/2020   73084      K Sunil     PlotParameterDefinition class changed an attribute name.
 * 02/05/2020   74587      K Sunil     removed system.out
 * 03/02/2020   75528      ksunil      Added setAttribute to use svg's built in visibility attribute.
 * </pre>
 *
 * @author ksunil
 */

/*
 * TODO may be good to have some PlotModelGuiElement subclass that has all the
 * setters, which is just used by the GUIs then...also maybe some interface, and
 * this is specifically the SVG implementation?
 */
public class PlotModelElement implements IPlotModelElement {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(PlotModelElement.class);

    private static final SingleTypeJAXBManager<ConditionalColor> conditionalColorJaxb = SingleTypeJAXBManager
            .createWithoutException(ConditionalColor.class);

    private static final String X = "x";

    private static final String Y = "y";

    private static final String PX = "px";

    private static final String EM = "em";

    private static final String FONT_FAMILY = "font-family";

    private static final String FONT_SIZE = "font-size";

    private static final int FONT_SIZE_PIXEL_BASIS = 16;

    private static final String FONT_WEIGHT = "font-weight";

    private static final String FONT_STYLE = "font-style";

    private static final String FILL = "fill";

    private static final String STROKE = "stroke";

    private static final String TEXT_ANCHOR = "text-anchor";

    private static final String CONDITIONAL_COLOR = "plotConditionalColor";

    private static final String SYMBOL_ATTRIBUTE = "plotSymbol";

    private static final String CLASS_ATTRIBUTE = "class";

    private static final String REQUIRED = "required";

    private static final String STROKE_WIDTH = "stroke-width";

    private static final String BASELINE_SHIFT = "baseline-shift";

    private static final String TRANSFORM = "transform";

    private static final Pattern SCALE_PATTERN = Pattern
            .compile("scale\\((\\d*\\.\\d+)\\)");

    /*
     * TODO more flexible with whitespace? Support other color formats at all?
     */
    private static final Pattern RGB_PATTERN = Pattern
            .compile("rgb\\((\\d+)(?:,|\\s+)(\\d+)(?:,|\\s+)(\\d+)\\)");

    private static final Map<String, Map<String, String>> classToAttrToValue;
    static {
        // TODO does this need to happen in PlotModelElement.setParamDef()?
        // how to know class when changing params...

        // TODO should fill=none be fill-opacity=0?

        Map<String, Map<String, String>> tempClassToAttrToValue = new HashMap<>();

        Map<String, String> textMap = new HashMap<>();
        textMap.put(FONT_SIZE, "0.8em");
        textMap.put(FONT_WEIGHT, "bold");
        textMap.put(FONT_FAMILY, "Courier");
        textMap.put(BASELINE_SHIFT, "-0.4em");
        tempClassToAttrToValue.put("text",
                Collections.unmodifiableMap(textMap));

        Map<String, String> weatherSpecialMap = new HashMap<>();
        weatherSpecialMap.put(FONT_SIZE, "1em");
        weatherSpecialMap.put(FILL, "none");
        weatherSpecialMap.put(STROKE_WIDTH, "1px");
        tempClassToAttrToValue.put("weather",
                Collections.unmodifiableMap(weatherSpecialMap));
        tempClassToAttrToValue.put("special",
                Collections.unmodifiableMap(weatherSpecialMap));

        Map<String, String> barbArrowMap = new HashMap<>();
        barbArrowMap.put(FONT_SIZE, "1em");
        barbArrowMap.put(FILL, "none");
        tempClassToAttrToValue.put("barb",
                Collections.unmodifiableMap(barbArrowMap));
        tempClassToAttrToValue.put("arrow",
                Collections.unmodifiableMap(barbArrowMap));

        classToAttrToValue = Collections
                .unmodifiableMap(tempClassToAttrToValue);
    }

    private PlotParameterDefinition paramDef;

    private String symbol = null;

    private UnitConverter converter = null;

    private Element plotElement = null;

    private PlotWindElement winds = null;

    private ConditionalColor conditionalColor;

    private boolean required = false;

    private boolean useDefaultColor;

    private boolean useDefaultLineWidth;

    public PlotModelElement(Element element) {
        this(element, false);
    }

    public PlotModelElement(Element element, boolean initialize) {
        this.plotElement = element;

        boolean hasColorDefined = false;
        for (String attr : getColorAttributes()) {
            if (plotElement.hasAttribute(attr)) {
                String value = plotElement.getAttribute(attr);
                if (!value.isEmpty() && !"none".equalsIgnoreCase(value)) {
                    hasColorDefined = true;
                    break;
                }
            }
        }
        this.useDefaultColor = !hasColorDefined;

        this.useDefaultLineWidth = !plotElement.hasAttribute(STROKE_WIDTH);

        applyDefaultClassValues();
    }

    private void applyDefaultClassValues() {
        List<Element> textElements = new ArrayList<>();
        if ("text".equals(plotElement.getTagName())) {
            textElements.add(plotElement);
        }
        NodeList textNodes = plotElement.getElementsByTagName("text");
        for (int i = 0; i < textNodes.getLength(); ++i) {
            textElements.add((Element) textNodes.item(i));
        }
        /*
         * TODO Maybe remove these values when saving to file? Really we should
         * just add these to individual elements in the SVG (and in overrides
         * via delta script) and not have Java mess with this
         */
        for (Element element : textElements) {
            String classAttr = "text";
            if (element.hasAttribute(CLASS_ATTRIBUTE)) {
                classAttr = element.getAttribute(CLASS_ATTRIBUTE);
            }

            Map<String, String> attrToValue = classToAttrToValue.get(classAttr);
            if (attrToValue != null) {
                for (Entry<String, String> attrAndValue : attrToValue
                        .entrySet()) {
                    String attr = attrAndValue.getKey();
                    if (!element.hasAttribute(attr)) {
                        String value = attrAndValue.getValue();
                        element.setAttribute(attr, value);
                    }
                }
            }
        }
    }

    @Override
    public int getX() {
        return getPixelAttribute(X);
    }

    @Override
    public void setX(int x) {
        plotElement.setAttribute(X, x + PX);
    }

    @Override
    public int getY() {
        return -getPixelAttribute(Y);
    }

    @Override
    public void setY(int y) {
        plotElement.setAttribute(Y, -y + PX);
    }

    private int getPixelAttribute(String attr) {
        if (plotElement.hasAttribute(attr)) {
            String attrStr = plotElement.getAttribute(attr);
            attrStr = attrStr.replace(PX, "");
            return Integer.valueOf(attrStr);
        }
        return 0;
    }

    @Override
    public int getFontSize() {
        // TODO perhaps need to use this:
        // em = desired element pixel value / parent element font-size in pixels
        String fontStr = plotElement.getAttribute(FONT_SIZE);
        fontStr = fontStr.replaceAll(EM, "");
        return Math.round(Float.valueOf(fontStr) * FONT_SIZE_PIXEL_BASIS);
    }

    @Override
    public void setFontSize(int size) {
        // should probably just set it as ${size}px - if that works
        float emSize = (float) size / FONT_SIZE_PIXEL_BASIS;
        plotElement.setAttribute(FONT_SIZE, String.valueOf(emSize) + EM);
    }

    @Override
    public String getFontFamily() {
        return plotElement.getAttribute(FONT_FAMILY);
    }

    @Override
    public void setFontFamily(String font) {
        plotElement.setAttribute(FONT_FAMILY, font);
    }

    @Override
    public String getFontStyle() {
        // TODO case-sensitivity...applyDefaultValues() sets to "bold"
        String fontStyle = plotElement.getAttribute(FONT_STYLE);
        String fontWeight = plotElement.getAttribute(FONT_WEIGHT);
        if (EditPlotModelComposite.NORMAL.equals(fontStyle)) {
            if (EditPlotModelComposite.NORMAL.equals(fontStyle)) {
                return EditPlotModelComposite.NORMAL;
            }
            return fontWeight;
        } else if (EditPlotModelComposite.NORMAL.equals(fontWeight)) {
            return fontStyle;
        } else {
            return fontWeight + "-" + fontStyle;
        }
    }

    @Override
    public void setFontStyle(String style) {
        plotElement.setAttribute(FONT_STYLE, EditPlotModelComposite.NORMAL);
        plotElement.setAttribute(FONT_WEIGHT, EditPlotModelComposite.NORMAL);
        switch (style) {
        case EditPlotModelComposite.NORMAL:
            // Defaulted to above
            break;
        case EditPlotModelComposite.ITALIC:
            plotElement.setAttribute(FONT_STYLE, style);
            break;
        case EditPlotModelComposite.BOLD:
            plotElement.setAttribute(FONT_WEIGHT, style);
            break;
        case EditPlotModelComposite.BOLD_ITALIC:
            plotElement.setAttribute(FONT_WEIGHT, EditPlotModelComposite.BOLD);
            plotElement.setAttribute(FONT_STYLE, EditPlotModelComposite.ITALIC);
            break;
        default:
            throw new IllegalArgumentException(
                    "Unexpected font style: " + style);
        }
    }

    @Override
    public RGB getColor() {
        // Just use the first one since they should match
        String colorAttr = getColorAttributes().iterator().next();
        String rgbStr = plotElement.getAttribute(colorAttr);
        Matcher rgbMatcher = RGB_PATTERN.matcher(rgbStr);
        if (rgbMatcher.find()) {
            int r = Integer.valueOf(rgbMatcher.group(1));
            int g = Integer.valueOf(rgbMatcher.group(2));
            int b = Integer.valueOf(rgbMatcher.group(3));
            return new RGB(r, g, b);
        }
        return null;
    }

    private Set<String> getColorAttributes() {
        // TODO currently ordering issues between paramDef being set and this
        // being called, fix this
        if (true) {
            return ImmutableSet.of(STROKE, FILL);
        }
        switch (paramDef.getDisplayType()) {
        case ARROWUV:
        case BARB:
        case TABLE:
            /*
             * TABLE may not always be? May need to check class attr as well
             */
            return Collections.singleton(STROKE);
        case MARKER:
            return ImmutableSet.of(STROKE, FILL);
        default:
            return Collections.singleton(FILL);
        }
    }

    @Override
    public void setColor(RGB color) {
        setColorInternal(color);
        if (color != null) {
            setConditionalColor(null);
            useDefaultColor = false;
        }
    }

    private void setColorInternal(RGB color) {
        for (String attr : getColorAttributes()) {
            if (color != null) {
                StringBuilder rgbStr = new StringBuilder("rgb(");
                rgbStr.append(color.red).append(",");
                rgbStr.append(color.green).append(",");
                rgbStr.append(color.blue).append(")");

                plotElement.setAttribute(attr, rgbStr.toString());
            } else {
                plotElement.removeAttribute(attr);
            }
        }
    }

    @Override
    public TextAnchor getTextAnchor() {
        // TODO have to remove all style="text-anchor:end" things, or handle
        // them?
        String anchorStr = plotElement.getAttribute(TEXT_ANCHOR);
        if (anchorStr.isEmpty()) {
            // SVG defaults it to START
            return TextAnchor.START;
        }
        return TextAnchor.valueOf(anchorStr.toUpperCase());
    }

    @Override
    public void setTextAnchor(TextAnchor anchor) {
        if (anchor == TextAnchor.START) {
            // SVG defaults it to start
            plotElement.removeAttribute(TEXT_ANCHOR);
        } else {
            String anchorStr = anchor.toString().toLowerCase();
            plotElement.setAttribute(TEXT_ANCHOR, anchorStr);
        }
    }

    @Override
    public boolean processDefaultColor(RGB defaultColor) {
        if (useDefaultColor) {
            setColorInternal(defaultColor);
        }
        return useDefaultColor;
    }

    @Override
    public boolean setLineStyle(LineStyle lineStyle) {
        DisplayType mode = getParamDef().getDisplayType();
        if (mode != DisplayType.BARB && mode != DisplayType.ARROW
                && mode != DisplayType.ARROWUV) {
            return false;
        }

        String dashArray;
        switch (lineStyle) {
        case DASH_DOTTED: {
            dashArray = "3, 2, 1, 2";
            break;
        }
        case DASHED: {
            dashArray = "4, 4";
            break;
        }
        case DASHED_LARGE: {
            dashArray = "12, 4";
            break;
        }
        case DOTTED: {
            dashArray = "1, 1";
            break;
        }
        default: {
            dashArray = "1, 0";
            break;
        }
        }

        plotElement.setAttribute("stroke-dasharray", dashArray);
        return true;
    }

    @Override
    public void setLineWidth(int width) {
        setLineWidthInternal(width);
        useDefaultLineWidth = false;
    }

    private void setLineWidthInternal(int width) {
        plotElement.setAttribute(STROKE_WIDTH, String.valueOf(width));
    }

    @Override
    public boolean processDefaultLineWidth(int width) {
        if (useDefaultLineWidth) {
            setLineWidthInternal(width);
        }
        return useDefaultLineWidth;
    }

    @Override
    public PlotParameterDefinition getParamDef() {
        return paramDef;
    }

    @Override
    public void setParamDef(PlotParameterDefinition paramDef) {
        this.paramDef = paramDef;

        if (paramDef.getSvgClass() != null) {
            plotElement.setAttribute("class", paramDef.getSvgClass());
        } else {
            plotElement.setAttribute("class", "text");
        }
        switch (paramDef.getDisplayType()) {
        case TEXT: {
            break;
        }
        case BARB: {
            PlotWindElement winds = new PlotWindElement();
            NodeList windElements = plotElement.getChildNodes();
            for (int j = 0; j < windElements.getLength(); j++) {
                if (Node.ELEMENT_NODE == windElements.item(j).getNodeType()) {
                    Element windElement = (Element) windElements.item(j);
                    String elementClass = windElement.getAttribute("class");
                    if (elementClass.matches("arrow")) {
                        winds.arrowElement = windElement;
                        winds.arrowNode = windElement.getChildNodes().item(0);
                    } else if (elementClass.matches("barb")) {
                        winds.barbElement = windElement;
                        winds.barbNode = windElement.getChildNodes().item(0);
                    } else if (elementClass.matches("text")) {
                        winds.gustElement = windElement;
                        winds.gustNode = windElement.getChildNodes().item(0);
                        winds.gustX = windElement.getAttribute("x");
                        winds.gustY = windElement.getAttribute("y");
                    }
                }
            }
            setWinds(winds);
            break;
        }
        case ARROWUV: {
            PlotWindElement winds = new PlotWindElement();
            NodeList windElements = plotElement.getChildNodes();
            for (int j = 0; j < windElements.getLength(); j++) {
                if (Node.ELEMENT_NODE == windElements.item(j).getNodeType()) {
                    Element windElement = (Element) windElements.item(j);
                    String attrClass = windElement.getAttribute("class");
                    if ("arrow".matches(attrClass)) {
                        winds.arrowElement = windElement;
                        winds.arrowNode = windElement.getChildNodes().item(0);
                    } else if ("arrow1".matches(attrClass)) {
                        winds.arrowElement = windElement;
                        winds.arrowNode = windElement.getChildNodes().item(0);
                        winds.arrowElement.setAttribute("arrowtype", attrClass);
                    } else if ("arrow2".matches(attrClass)) {
                        winds.arrowElement = windElement;
                        winds.arrowNode = windElement.getChildNodes().item(0);
                        winds.arrowElement.setAttribute("arrowtype", attrClass);
                    } else if ("text".matches(attrClass)) {
                        winds.gustElement = windElement;
                        winds.gustNode = windElement.getChildNodes().item(0);
                        winds.gustX = windElement.getAttribute("x");
                        winds.gustY = windElement.getAttribute("y");
                    }
                }
            }
            setWinds(winds);
            break;
        }
        case ARROW: {
            break;
        }
        case TABLE:
        case RANGE:
        case MARKER: {
            break;
        }
        }

        // Probably move these to PlotModelElement getters
        if (plotElement.hasAttribute(SYMBOL_ATTRIBUTE)) {
            setSymbol(plotElement.getAttribute(SYMBOL_ATTRIBUTE));
        }
        if (plotElement.hasAttribute(REQUIRED)) {
            setRequired(
                    Boolean.parseBoolean(plotElement.getAttribute(REQUIRED)));
        }
    }

    @Override
    public PlotWindElement getWinds() {
        return winds;
    }

    @Override
    public void setWinds(PlotWindElement winds) {
        this.winds = winds;
    }

    @Override
    public String getSymbol() {
        return symbol;
    }

    @Override
    public void setSymbol(String symbol) {
        this.symbol = symbol;
    }

    @Override
    public UnitConverter getConverter() {
        return converter;
    }

    @Override
    public void setConverter(UnitConverter converter) {
        this.converter = converter;
    }

    @Override
    public boolean isRequired() {
        return required;
    }

    @Override
    public void setRequired(boolean required) {
        this.required = required;
    }

    @Override
    public String getValue() {
        Node plotNode = plotElement.getFirstChild();
        return plotNode.getNodeValue();
    }

    @Override
    public void setValue(String value) {

        Node plotNode = plotElement.getFirstChild();
        if (plotNode == null) {
            return;
        }
        plotNode.setNodeValue(value);
    }

    @Override
    public DisplayMode getMode() {

        DisplayMode mode = DisplayMode.VISIBLE;
        String plotMode = null;
        if (plotElement.hasAttribute(DM_ATTRIBUTE)) {
            plotMode = plotElement.getAttribute(DM_ATTRIBUTE);
        } else if (plotElement.hasAttribute(VISIBILITY)) {
            plotMode = plotElement.getAttribute(VISIBILITY).toUpperCase();
        }
        if (plotMode != null) {
            try {
                mode = DisplayMode.valueOf(plotMode);
            } catch (Exception e) {
                String validModes = Arrays.stream(DisplayMode.values())
                        .map(DisplayMode::name)
                        .collect(Collectors.joining(", "));
                statusHandler.error("Invalid plot display mode: " + plotMode
                        + "(valid modes: " + validModes + ")", e);
            }
        }
        return mode;
    }

    @Override
    public void setMode(DisplayMode mode) {
        plotElement.setAttribute(VISIBILITY,
                mode == DisplayMode.VISIBLE ? "visible" : "hidden");
        plotElement.setAttribute(DM_ATTRIBUTE, mode.toString());
    }

    @Override
    public double getSymbolSize() {
        if (plotElement.hasAttribute(TRANSFORM)) {
            String transform = plotElement.getAttribute(TRANSFORM);
            Matcher matcher = SCALE_PATTERN.matcher(transform);
            if (matcher.find()) {
                return Double.valueOf(matcher.group(1));
            }
        }
        return 1d;
    }

    @Override
    public void setSymbolSize(double size) {
        // TODO just use font-size=${size}em?
        String scale = "scale(" + size + ")";
        String transform;
        if (plotElement.hasAttribute(TRANSFORM)) {
            transform = plotElement.getAttribute(TRANSFORM).trim();
            Matcher matcher = SCALE_PATTERN.matcher(transform);
            if (matcher.find()) {
                transform = matcher.replaceAll(scale);
            } else {
                if (!transform.isEmpty() && !transform.endsWith(",")) {
                    transform += ",";
                }
                transform += scale;
            }
        } else {
            transform = scale;
        }
        plotElement.setAttribute(TRANSFORM, transform);
    }

    // TODO these need consolidated with get/setLineWidth

    @Override
    public double getSymbolWidth() {
        if (plotElement.hasAttribute(STROKE_WIDTH)) {
            String attrStr = plotElement.getAttribute(STROKE_WIDTH);
            attrStr = attrStr.replace(PX, "");
            return Double.valueOf(attrStr);
        }
        return 1d;
    }

    @Override
    public void setSymbolWidth(double width) {
        plotElement.setAttribute(STROKE_WIDTH, width + PX);
    }

    @Override
    public ConditionalColor getConditionalColor() {
        if (conditionalColor != null) {
            return conditionalColor;
        }

        String colorStr = plotElement.getAttribute(CONDITIONAL_COLOR);
        if (!colorStr.isEmpty()) {
            colorStr = StringEscapeUtils.unescapeXml(colorStr);
            try {
                return conditionalColorJaxb.unmarshalFromXml(colorStr);
            } catch (JAXBException e) {
                statusHandler.error(
                        "Error getting conditional color for plot element (XML string: "
                                + colorStr + ")",
                        e);
            }
        }
        return null;
    }

    @Override
    public void setConditionalColor(ConditionalColor color) {
        if (color != null) {
            try {
                MarshalOptions unformattedFragment = new MarshalOptions(false,
                        true);
                String colorStr = conditionalColorJaxb.marshalToXml(color,
                        unformattedFragment);
                colorStr = StringEscapeUtils.escapeXml(colorStr);
                plotElement.setAttribute(CONDITIONAL_COLOR, colorStr);
                setColorInternal(null);
                useDefaultColor = false;
            } catch (JAXBException e) {
                statusHandler.error(
                        "Error setting conditional color on plot element", e);
            }
        } else {
            plotElement.removeAttribute(CONDITIONAL_COLOR);
        }
        conditionalColor = color;
    }

    /*
     * TODO not sure where to call this from to make sure we are doing it on the
     * right units. Also, what to do if data is missing? (e.g.
     * PlotModelFactory.processTextDirective() sets text to "m" in this
     * case...probably want default color then)
     */

    @Override
    public void processConditionalColor(IPlotData plotData, String plugin)
            throws VizException {
        // TODO probably need to remove the color we just set when saving to
        // file...
        ConditionalColor conditionalColor = getConditionalColor();
        if (conditionalColor != null) {
            RGB color = conditionalColor.getColor(plotData, plugin);
            setColorInternal(color);
        }
    }

    @Override
    public void dispose() {
        plotElement.getParentNode().removeChild(plotElement);
    }

    @Override
    public void prepareForSave() {
        // TODO remove unnecessary/auto-added attributes
    }

    @Override
    public String getParam() {
        return getParamDef().getParamName();
    }
}