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

import javax.measure.UnitConverter;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.pointdata.PlotModelFactory.DisplayMode;
import com.raytheon.viz.pointdata.def.ConditionalColor;
import com.raytheon.viz.pointdata.def.PlotParameterDefinition;
import com.raytheon.viz.pointdata.def.ui.EditPlotModelComposite.TextAnchor;

/**
 * PlotModelElementIntf pulled from an existing PlotModelElement. Needed for new
 * customized plot code
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 10/31/2019   71272      K Sunil     Initial Creation
 * 03/02/2020   75528      ksunil      Added new static variables
 * </pre>
 *
 * @author ksunil
 */

public interface IPlotModelElement {

    public static final String VISIBILITY = "visibility";

    public static final String DM_ATTRIBUTE = "plotMode";

    int getX();

    void setX(int x);

    int getY();

    void setY(int y);

    int getFontSize();

    void setFontSize(int size);

    String getFontFamily();

    void setFontFamily(String font);

    String getFontStyle();

    void setFontStyle(String style);

    RGB getColor();

    void setColor(RGB color);

    TextAnchor getTextAnchor();

    void setTextAnchor(TextAnchor anchor);

    boolean processDefaultColor(RGB defaultColor);

    boolean setLineStyle(LineStyle lineStyle);

    void setLineWidth(int width);

    boolean processDefaultLineWidth(int width);

    PlotParameterDefinition getParamDef();

    void setParamDef(PlotParameterDefinition paramDef);

    /**
     * @return the winds
     */
    PlotWindElement getWinds();

    /**
     * @param winds
     *            the winds to set
     */
    void setWinds(PlotWindElement winds);

    /**
     * @return the symbol
     */
    String getSymbol();

    /**
     * @param symbol
     *            the symbol to set
     */
    void setSymbol(String symbol);

    /**
     * @return the converter
     */
    UnitConverter getConverter();

    /**
     * @param converter
     *            the converter to set
     */
    void setConverter(UnitConverter converter);

    /**
     * @return the required
     */
    boolean isRequired();

    /**
     * @param required
     *            the required to set
     */
    void setRequired(boolean required);

    String getValue();

    void setValue(String value);

    /**
     * @return the mode
     */
    DisplayMode getMode();

    /**
     * @param mode
     *            the mode to set
     */
    void setMode(DisplayMode mode);

    double getSymbolSize();

    void setSymbolSize(double size);

    // TODO these need consolidated with get/setLineWidth
    double getSymbolWidth();

    void setSymbolWidth(double width);

    ConditionalColor getConditionalColor();

    void setConditionalColor(ConditionalColor color);

    /*
     * TODO not sure where to call this from to make sure we are doing it on the
     * right units. Also, what to do if data is missing? (e.g.
     * PlotModelFactory.processTextDirective() sets text to "m" in this
     * case...probably want default color then)
     */
    void processConditionalColor(IPlotData plotData, String plugin)
            throws VizException;

    void dispose();

    void prepareForSave();

    String getParam();

}