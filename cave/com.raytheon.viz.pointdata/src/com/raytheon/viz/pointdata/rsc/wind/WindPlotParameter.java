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
package com.raytheon.viz.pointdata.rsc.wind;

import java.text.ParseException;
import java.text.ParsePosition;
import java.util.Date;

import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlTransient;

import com.raytheon.uf.common.pointdata.PointDataDescription.Type;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * 
 * Configuration information for a single parameter that is part of a
 * {@link WindPlotConfig}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Nov 13, 2015  4903     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class WindPlotParameter {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(WindPlotParameter.class);

    @XmlAttribute
    private String parameter;

    @XmlAttribute
    private String unit;

    @XmlAttribute
    private Boolean time;

    @XmlElement(name = "format")
    private FormatValue[] formats;

    @XmlTransient
    private Unit<?> parsedUnit;

    /* Avoid spamming logs. */
    @XmlTransient
    private boolean unitParseFailed = false;

    public WindPlotParameter() {

    }

    public WindPlotParameter(String parameter) {
        this.parameter = parameter;
    }

    public WindPlotParameter(String parameter, String unit) {
        this.parameter = parameter;
        this.unit = unit;
    }

    public String getParameter() {
        return parameter;
    }

    public void setParameter(String parameter) {
        this.parameter = parameter;
    }

    public String getUnit() {
        return unit;
    }

    public void setUnit(String unit) {
        this.unit = unit;
    }

    public Boolean getTime() {
        return time;
    }

    public void setTime(Boolean time) {
        this.time = time;
    }

    public FormatValue[] getFormats() {
        return formats;
    }

    public void setFormats(FormatValue[] formats) {
        this.formats = formats;
    }

    public Unit<?> getParsedUnit() {
        if (!unitParseFailed && parsedUnit == null) {
            try {
                parsedUnit = UnitFormat.getUCUMInstance().parseProductUnit(
                        unit, new ParsePosition(0));
            } catch (ParseException e) {
                statusHandler.handle(Priority.WARN,
                        "Error retrieving wind data.", e);
                unitParseFailed = true;
            }
        }
        return parsedUnit;
    }

    public double getNumericValue(PointDataView view) {
        double value = view.getNumber(parameter).doubleValue();
        if (unit != null) {
            Unit<?> desired = getParsedUnit();
            Unit<?> current = view.getUnit(parameter);
            if (desired != null && current.isCompatible(desired)) {
                value = current.getConverterTo(desired).convert(value);
            }
        }
        return value;
    }

    public Object getValue(PointDataView view) {
        if (view.getType(parameter) == Type.STRING) {
            return view.getString(parameter);
        } else if (Boolean.TRUE.equals(time)) {
            return TimeUtil.newGmtCalendar(new Date(view.getNumber(parameter)
                    .longValue()));
        } else if (formats != null) {
            Number value = view.getNumber(parameter);
            for (FormatValue format : formats) {
                if (format.matches(value)) {
                    return format.getText();
                }
            }
            return value.toString();
        } else if (unit != null) {
            return getNumericValue(view);
        }
        return view.getNumber(parameter);
    }

    @XmlAccessorType(XmlAccessType.NONE)
    public static class FormatValue {

        @XmlAttribute
        private Double high;

        @XmlAttribute
        private Double low;

        @XmlAttribute
        private Double value;

        @XmlAttribute
        private String text;

        public boolean matches(Number value) {
            if (this.value != null
                    && value.doubleValue() == this.value.doubleValue()) {
                return true;
            } else if (high != null && low != null
                    && high.doubleValue() >= value.doubleValue()
                    && low.doubleValue() < value.doubleValue()) {
                return true;
            }
            return false;
        }

        public Double getHigh() {
            return high;
        }

        public void setHigh(Double high) {
            this.high = high;
        }

        public Double getLow() {
            return low;
        }

        public void setLow(Double low) {
            this.low = low;
        }

        public Double getValue() {
            return value;
        }

        public void setValue(Double value) {
            this.value = value;
        }

        public String getText() {
            return text;
        }

        public void setText(String text) {
            this.text = text;
        }

    }

}