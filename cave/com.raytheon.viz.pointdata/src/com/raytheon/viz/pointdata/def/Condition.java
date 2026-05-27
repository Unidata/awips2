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

import java.text.ParsePosition;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import javax.measure.IncommensurableException;
import javax.measure.UnconvertibleException;
import javax.measure.Unit;
import javax.measure.UnitConverter;
import javax.measure.format.MeasurementParseException;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;

import org.apache.commons.lang3.StringUtils;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.pointdata.IPlotData;

import tech.units.indriya.format.SimpleUnitFormat;
import tech.units.indriya.format.SimpleUnitFormat.Flavor;

/**
 * Group of parameter constraints (T<50 and windSpeed >30). This class is
 * extended by ColorCondition to map a color to the above set of constraints.
 * This class is also used by the filters to decide which plots to display.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer      Description
 * ------------ ---------- -----------   --------------------------
 * 2019-10-10   71272      Mark Peters   Initial Creation
 * 2020-01-07   73083      ksunil        Some code moved to ColorCondition
 * 2023-10-04   2036243    tgurney       Unconvertible values shall return NaN
 *                                       instead of throwing NumberFormatException
 * </pre>
 *
 * @author mpeters
 */

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class Condition {

    @XmlElement
    private final List<ParamConstraint> paramConstraints;

    /**
     * Empty constructor for serialization
     */
    public Condition() {
        this.paramConstraints = new ArrayList<>();
    }

    public Condition(List<ParamConstraint> paramConstraints) {
        this.paramConstraints = paramConstraints;
    }

    public Condition(Condition other) {
        paramConstraints = other.paramConstraints.stream()
                .map(ParamConstraint::new).collect(Collectors.toList());

    }

    public List<ParamConstraint> getParamConstraints() {
        return Collections.unmodifiableList(paramConstraints);
    }

    public void addParamConstraint(ParamConstraint constraint) {
        paramConstraints.add(constraint);
    }

    public boolean removeParamConstraint(ParamConstraint constraint) {
        return paramConstraints.remove(constraint);
    }

    public boolean evaluate(IPlotData plotData, String plugin)
            throws VizException {
        PlotParameterDefinitions paramDefs = PlotParameterDefinitionsManager
                .getInstance().getDefinitions(plugin);
        if (paramDefs == null) {
            return false;
        }
        for (ParamConstraint paramConstraint : paramConstraints) {
            String paramDisplayName = paramConstraint.getParam();
            PlotParameterDefinition paramDef = paramDefs
                    .getParamDef(paramDisplayName);
            if (!paramConstraint.getConstraint()
                    .evaluate(getValue(plotData, paramDef))) {
                return false;
            }
        }
        return true;
    }

    public Object getValue(IPlotData plotData, PlotParameterDefinition paramDef)
            throws VizException {
        // Mostly copied from PlotModelFactory.processTextDirective.
        Object rval = null;
        String param = paramDef.getParamName();
        int dimensions = plotData.getDimensions(param);
        switch (plotData.getType(param)) {
        case FLOAT:
        case INT:
        case LONG:
            Number value = null;
            if (dimensions == 1) {
                value = plotData.getNumber(param);
            } else if (dimensions == 2) {
                Number[] values = plotData.getNumberAllLevels(param);
                int index = paramDef.getIndex();
                if (index != -1 && values != null && index < values.length) {
                    value = values[index];
                }
            }

            if (value != null) {
                String unitStr = paramDef.getUnit();
                if (unitStr != null) {
                    UnitConverter converter;
                    try {
                        Unit<?> unit = SimpleUnitFormat
                                .getInstance(Flavor.ASCII).parseProductUnit(
                                        unitStr, new ParsePosition(0));
                        converter = plotData.getUnit(param)
                                .getConverterToAny(unit);
                    } catch (MeasurementParseException | UnconvertibleException
                            | IncommensurableException e) {
                        throw new VizException(
                                "Unable to parse or convert units " + unitStr
                                        + " and "
                                        + plotData.getUnit(param).toString(),
                                e);
                    }
                    try {
                        rval = converter.convert(value.doubleValue());
                    } catch (NumberFormatException e) {
                        // unconvertible value
                        rval = Double.NaN;
                    }
                } else {
                    rval = value.doubleValue();
                }
            }
            break;
        case STRING:
            rval = plotData.getString(param);
            break;
        }

        return rval;
    }

    @Override
    public String toString() {
        return StringUtils.join(paramConstraints, ", ");
    }

}