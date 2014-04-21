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
package com.raytheon.uf.viz.truecolor.rsc;

import java.text.ParsePosition;

import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.viz.truecolor.extension.ITrueColorImagingExtension.Channel;

/**
 * Channel information (range/unit) for a true color channel
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Aug 20, 2012           mschenke    Initial creation
 * Apr 18, 2014  2947     bsteffen    Support unitless data.
 * 
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class ChannelInfo {

    @XmlAttribute
    private Channel channel;

    @XmlElement
    private double rangeMin = 0.0;

    @XmlElement
    private double rangeMax = 1.0;

    private Unit<?> unit = Unit.ONE;

    /**
     * @return the channel
     */
    public Channel getChannel() {
        return channel;
    }

    /**
     * @param channel
     *            the channel to set
     */
    public void setChannel(Channel channel) {
        this.channel = channel;
    }

    /**
     * @return the rangeMin
     */
    public double getRangeMin() {
        return rangeMin;
    }

    /**
     * @param rangeMin
     *            the rangeMin to set
     */
    public void setRangeMin(double rangeMin) {
        this.rangeMin = rangeMin;
    }

    /**
     * @return the rangeMax
     */
    public double getRangeMax() {
        return rangeMax;
    }

    /**
     * @param rangeMax
     *            the rangeMax to set
     */
    public void setRangeMax(double rangeMax) {
        this.rangeMax = rangeMax;
    }

    /**
     * @return the unit
     */
    public Unit<?> getUnit() {
        return unit;
    }

    /**
     * @param unit
     *            the unit to set
     */
    public void setUnit(Unit<?> unit) {
        if (unit == null) {
            this.unit = Unit.ONE;
        } else {
            this.unit = unit;
        }
    }

    @XmlElement(name = "unit")
    public void setUnitString(String unit) {
        if (unit != null) {
            setUnit(UnitFormat.getUCUMInstance().parseObject(unit,
                    new ParsePosition(0)));
        } else {
            setUnit(null);
        }

    }

    public String getUnitString() {
        // Let anything with Unit.ONE serialize back out as null since it was
        // probably never set and will default to Unit.ONE
        return unit != Unit.ONE ? UnitFormat.getUCUMInstance().format(unit)
                : null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((channel == null) ? 0 : channel.hashCode());
        long temp;
        temp = Double.doubleToLongBits(rangeMax);
        result = prime * result + (int) (temp ^ (temp >>> 32));
        temp = Double.doubleToLongBits(rangeMin);
        result = prime * result + (int) (temp ^ (temp >>> 32));
        result = prime * result + ((unit == null) ? 0 : unit.hashCode());
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        ChannelInfo other = (ChannelInfo) obj;
        if (channel != other.channel)
            return false;
        if (Double.doubleToLongBits(rangeMax) != Double
                .doubleToLongBits(other.rangeMax))
            return false;
        if (Double.doubleToLongBits(rangeMin) != Double
                .doubleToLongBits(other.rangeMin))
            return false;
        if (unit == null) {
            if (other.unit != null)
                return false;
        } else if (!unit.equals(other.unit))
            return false;
        return true;
    }

}
