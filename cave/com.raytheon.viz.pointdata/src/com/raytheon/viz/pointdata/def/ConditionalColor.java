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
package com.raytheon.viz.pointdata.def;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.pointdata.IPlotData;

/**
 * ConditionalColor object Used in the Edit Plot Attributes to map different
 * parameter values and ranges to colors.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 10/10/2019   71272      Mark Peters Initial Creation
 * Jan 07, 2020 73083      ksunil      changes to absorb the new ColorCondition class
 * 
 * </pre>
 * 
 * @author mpeters
 */

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class ConditionalColor {

    @XmlElement
    @XmlJavaTypeAdapter(value = RgbAdapter.class)
    private RGB defaultColor;

    @XmlElement
    private List<ColorCondition> conditionsAndColors;

    public ConditionalColor() {
        this(new ArrayList<>(), null);
    }

    public ConditionalColor(List<ColorCondition> conditionsAndColors,
            RGB defaultColor) {
        this.conditionsAndColors = conditionsAndColors;
        this.defaultColor = defaultColor;
    }

    public ConditionalColor(ConditionalColor other) {
        conditionsAndColors = other.conditionsAndColors.stream()
                .map(ColorCondition::new).collect(Collectors.toList());
        defaultColor = cloneRgb(other.defaultColor);
    }

    public RGB getDefaultColor() {
        return defaultColor;
    }

    public void setDefaultColor(RGB color) {
        this.defaultColor = color;
    }

    public List<ColorCondition> getConditionsAndColors() {
        return conditionsAndColors;
    }

    public void setConditionsToColors(
            List<ColorCondition> conditionsAndColors) {
        this.conditionsAndColors = conditionsAndColors;
    }

    public RGB getColor(IPlotData plotData, String plugin) throws VizException {
        for (ColorCondition conditionalColor : conditionsAndColors) {
            if (conditionalColor.evaluate(plotData, plugin)) {
                return conditionalColor.getColor();
            }
        }

        return defaultColor;
    }

    static RGB cloneRgb(RGB rgb) {
        return new RGB(rgb.red, rgb.green, rgb.blue);
    }
}
