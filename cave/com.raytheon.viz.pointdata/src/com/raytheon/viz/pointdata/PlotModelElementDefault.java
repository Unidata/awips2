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
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.pointdata.PlotModelFactoryDefault.DisplayMode;
import com.raytheon.viz.pointdata.def.ConditionalColor;
import com.raytheon.viz.pointdata.def.PlotParameterDefinition;
import com.raytheon.viz.pointdata.def.ui.EditPlotModelComposite.TextAnchor;
import com.raytheon.viz.pointdata.lookup.IAbstractLookupTable;

/**
 * PlotModelElement object to handle the old style plots
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 10/31/2019   71272      K Sunil     Initial Creation
 * 03/02/2020   75528      ksunil      General cleanup
 * </pre>
 *
 * @author ksunil
 */

public class PlotModelElementDefault implements IPlotModelElement {

    public DisplayMode mode = DisplayMode.TEXT;

    public String format = null;

    public String parameter = null;

    public String unit = null;

    public String symbol = null;

    public int trim = 0;

    public int index = -1;

    public UnitConverter converter = null;

    public Element plotElement = null;

    public Node plotNode = null;

    public S2N ranking = null;

    public IAbstractLookupTable lookup = null;

    public PlotWindElement winds = null;

    public boolean required = false;

    public Node getPlotNode() {
        return plotNode;
    }

    public String getParameter() {
        return parameter;
    }

    @Override
    public int getX() {
        // TODO Auto-generated method stub
        return 0;
    }

    @Override
    public void setX(int x) {
        // TODO Auto-generated method stub

    }

    @Override
    public int getY() {
        // TODO Auto-generated method stub
        return 0;
    }

    @Override
    public void setY(int y) {
        // TODO Auto-generated method stub

    }

    @Override
    public int getFontSize() {
        // TODO Auto-generated method stub
        return 0;
    }

    @Override
    public void setFontSize(int size) {
        // TODO Auto-generated method stub

    }

    @Override
    public String getFontFamily() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void setFontFamily(String font) {
        // TODO Auto-generated method stub

    }

    @Override
    public String getFontStyle() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void setFontStyle(String style) {
        // TODO Auto-generated method stub

    }

    @Override
    public RGB getColor() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void setColor(RGB color) {
        // TODO Auto-generated method stub

    }

    @Override
    public TextAnchor getTextAnchor() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void setTextAnchor(TextAnchor anchor) {
        // TODO Auto-generated method stub

    }

    @Override
    public boolean processDefaultColor(RGB defaultColor) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public boolean setLineStyle(LineStyle lineStyle) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public void setLineWidth(int width) {
        // TODO Auto-generated method stub

    }

    @Override
    public boolean processDefaultLineWidth(int width) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public PlotParameterDefinition getParamDef() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void setParamDef(PlotParameterDefinition paramDef) {
        // TODO Auto-generated method stub

    }

    @Override
    public String getSymbol() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void setSymbol(String symbol) {
        // TODO Auto-generated method stub

    }

    @Override
    public UnitConverter getConverter() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void setConverter(UnitConverter converter) {
        // TODO Auto-generated method stub

    }

    @Override
    public boolean isRequired() {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public void setRequired(boolean required) {
        // TODO Auto-generated method stub

    }

    @Override
    public String getValue() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void setValue(String value) {
        // TODO Auto-generated method stub

    }

    @Override
    public double getSymbolSize() {
        // TODO Auto-generated method stub
        return 0;
    }

    @Override
    public void setSymbolSize(double size) {
        // TODO Auto-generated method stub

    }

    @Override
    public double getSymbolWidth() {
        // TODO Auto-generated method stub
        return 0;
    }

    @Override
    public void setSymbolWidth(double width) {
        // TODO Auto-generated method stub

    }

    @Override
    public ConditionalColor getConditionalColor() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void setConditionalColor(ConditionalColor color) {
        // TODO Auto-generated method stub

    }

    @Override
    public void processConditionalColor(IPlotData plotData, String plugin)
            throws VizException {
        // TODO Auto-generated method stub

    }

    @Override
    public void dispose() {
        // TODO Auto-generated method stub

    }

    @Override
    public void prepareForSave() {
        // TODO Auto-generated method stub

    }

    @Override
    public com.raytheon.viz.pointdata.PlotModelFactory.DisplayMode getMode() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void setMode(
            com.raytheon.viz.pointdata.PlotModelFactory.DisplayMode mode) {
        // TODO Auto-generated method stub

    }

    @Override
    public PlotWindElement getWinds() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void setWinds(PlotWindElement winds) {
        // TODO Auto-generated method stub

    }

    @Override
    public String getParam() {
        return getParameter();
    }
}