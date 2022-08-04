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
package com.raytheon.edex.plugin.grib.decoderpostprocessors.precipitation;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Describes the parameters used in an accumulation calculation.
 *
 * <pre>
 *
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 28, 2015 3756       nabowle     Initial creation
 *
 * </pre>
 *
 * @author nabowle
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class AccumulationCreationConfig {

    @XmlAttribute(required = true)
    private int forecastPeriodHours;

    @XmlAttribute(required = true)
    private String accumulationParam;

    @XmlAttribute(required = true)
    private String minuendParam;

    @XmlAttribute(required = true)
    private String subtrahendParam;

    /**
     * Constructor.
     */
    public AccumulationCreationConfig() {
        super();
    }

    /**
     * Constructor.
     *
     *
     * @param forecastPeriod
     *            The forecast period in hours.
     * @param accumulation
     *            The parameter to use for the calculated accumulation.
     * @param minuend
     *            The parameter subtracted from in calculating accumulation.
     * @param subtrahend
     *            The parameter being subtracted.
     */
    public AccumulationCreationConfig(int forecastPeriod, String accumulation,
            String minuend, String subtrahend) {
        super();
        this.forecastPeriodHours = forecastPeriod;
        this.accumulationParam = accumulation;
        this.minuendParam = minuend;
        this.subtrahendParam = subtrahend;
    }

    /**
     * @return the forecastPeriod
     */
    public int getForecastPeriodHours() {
        return forecastPeriodHours;
    }

    /**
     * @param forecastPeriod
     *            the forecastPeriod to set
     */
    public void setForecastPeriodHours(int forecastPeriod) {
        this.forecastPeriodHours = forecastPeriod;
    }

    /**
     * @return the forecastPeriod in seconds.
     */
    public int getForecastPeriodSeconds() {
        return forecastPeriodHours * TimeUtil.SECONDS_PER_HOUR;
    }

    /**
     * @return the accumulation
     */
    public String getAccumulationParam() {
        return accumulationParam;
    }

    /**
     * @param accumulation
     *            the accumulation to set
     */
    public void setAccumulationParam(String accumulation) {
        this.accumulationParam = accumulation;
    }

    /**
     * @return the minuend
     */
    public String getMinuendParam() {
        return minuendParam;
    }

    /**
     * @param minuend
     *            the minuend to set
     */
    public void setMinuendParam(String minuend) {
        this.minuendParam = minuend;
    }

    /**
     * @return the subtrahend
     */
    public String getSubtrahendParam() {
        return subtrahendParam;
    }

    /**
     * @param subtrahend
     *            the subtrahend to set
     */
    public void setSubtrahendParam(String subtrahend) {
        this.subtrahendParam = subtrahend;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime
                * result
                + ((accumulationParam == null) ? 0 : accumulationParam
                        .hashCode());
        result = prime * result + forecastPeriodHours;
        result = prime * result
                + ((minuendParam == null) ? 0 : minuendParam.hashCode());
        result = prime * result
                + ((subtrahendParam == null) ? 0 : subtrahendParam.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        AccumulationCreationConfig other = (AccumulationCreationConfig) obj;
        if (accumulationParam == null) {
            if (other.accumulationParam != null) {
                return false;
            }
        } else if (!accumulationParam.equals(other.accumulationParam)) {
            return false;
        }
        if (forecastPeriodHours != other.forecastPeriodHours) {
            return false;
        }
        if (minuendParam == null) {
            if (other.minuendParam != null) {
                return false;
            }
        } else if (!minuendParam.equals(other.minuendParam)) {
            return false;
        }
        if (subtrahendParam == null) {
            if (other.subtrahendParam != null) {
                return false;
            }
        } else if (!subtrahendParam.equals(other.subtrahendParam)) {
            return false;
        }
        return true;
    }
}
