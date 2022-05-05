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
package com.raytheon.uf.common.monitor.scan.xml;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

@XmlAccessorType(XmlAccessType.NONE)
public class SCANAttributesXML {
    @XmlElement(name = "AttrName")
    private String attrName;

    @XmlElement(name = "Low")
    private Double low;

    @XmlElement(name = "Mid")
    private Double mid;

    @XmlElement(name = "Upper")
    private Double upper;

    @XmlElement(name = "Rank")
    private String rank;

    @XmlElement(name = "Trend")
    private Boolean trend;

    @XmlElement(name = "TimeHeight")
    private Boolean timeHeight;

    @XmlElement(name = "Colored")
    private Boolean colored;

    @XmlElement(name = "Clutter")
    private Boolean clutter;

    @XmlElement(name = "InTable")
    private Boolean inTable;

    @XmlElement(name = "Min")
    private Double min;

    @XmlElement(name = "Interval")
    private Double interval;

    @XmlElement(name = "Range")
    private Double range;

    @XmlElement(name = "Alarm")
    private Double alarm;

    @XmlElement(name = "AbsAlarm")
    private Double absAlarm;

    @XmlElement(name = "HasAlarm")
    private Boolean hasAlarm;

    @XmlElement(name = "Units")
    private String units;

    public SCANAttributesXML() {
    }

    public String getAttrName() {
        return attrName;
    }

    public void setAttrName(String attrName) {
        this.attrName = attrName;
    }

    public double getLow() {
        return low;
    }

    public void setLow(double low) {
        this.low = low;
    }

    public double getMid() {
        return mid;
    }

    public void setMid(double mid) {
        this.mid = mid;
    }

    public double getUpper() {
        return upper;
    }

    public void setUpper(double upper) {
        this.upper = upper;
    }

    public String getRank() {
        return rank;
    }

    public void setRank(String rank) {
        this.rank = rank;
    }

    public boolean getTrend() {
        return trend;
    }

    public void setTrend(boolean trend) {
        this.trend = trend;
    }

    public boolean getTimeHeight() {
        return timeHeight;
    }

    public void setTimeHeight(boolean timeHeight) {
        this.timeHeight = timeHeight;
    }

    public boolean getColored() {
        return colored;
    }

    public void setColored(boolean colored) {
        this.colored = colored;
    }

    public boolean getClutter() {
        return clutter;
    }

    public void setClutter(boolean clutter) {
        this.clutter = clutter;
    }

    public boolean getInTable() {
        return inTable;
    }

    public void setInTable(boolean inTable) {
        this.inTable = inTable;
    }

    public double getMin() {
        return min;
    }

    public void setMin(double min) {
        this.min = min;
    }

    public double getInterval() {
        return interval;
    }

    public void setInterval(double interval) {
        this.interval = interval;
    }

    public double getRange() {
        return range;
    }

    public void setRange(double range) {
        this.range = range;
    }

    public double getAlarm() {
        return alarm;
    }

    public void setAlarm(double alarm) {
        this.alarm = alarm;
    }

    public double getAbsAlarm() {
        return absAlarm;
    }

    public void setAbsAlarm(double absAlarm) {
        this.absAlarm = absAlarm;
    }

    public boolean getHasAlarm() {
        return hasAlarm;
    }

    public void setHasAlarm(boolean hasAlarm) {
        this.hasAlarm = hasAlarm;
    }

    public String getUnits() {
        return units;
    }

    public void setUnits(String units) {
        this.units = units;
    }

    /**
     * Incremental override. Copy all non-null fields from other to this. Both
     * must have the same attrname
     */
    public void combine(SCANAttributesXML other) {
        if (!other.attrName.equals(this.attrName)) {
            String fmt = "Cannot combine() attributes with two different names"
                    + " (this: %s, other: %s)";
            throw new IllegalArgumentException(
                    String.format(fmt, this.attrName, other.attrName));
        }
        if (other.low != null) {
            this.low = other.low;
        }
        if (other.mid != null) {
            this.mid = other.mid;
        }
        if (other.upper != null) {
            this.upper = other.upper;
        }
        if (other.rank != null) {
            this.rank = other.rank;
        }
        if (other.trend != null) {
            this.trend = other.trend;
        }
        if (other.timeHeight != null) {
            this.timeHeight = other.timeHeight;
        }
        if (other.colored != null) {
            this.colored = other.colored;
        }
        if (other.clutter != null) {
            this.clutter = other.clutter;
        }
        if (other.inTable != null) {
            this.inTable = other.inTable;
        }
        if (other.min != null) {
            this.min = other.min;
        }
        if (other.interval != null) {
            this.interval = other.interval;
        }
        if (other.range != null) {
            this.range = other.range;
        }
        if (other.alarm != null) {
            this.alarm = other.alarm;
        }
        if (other.absAlarm != null) {
            this.absAlarm = other.absAlarm;
        }
        if (other.hasAlarm != null) {
            this.hasAlarm = other.hasAlarm;
        }
        if (other.units != null) {
            this.units = other.units;
        }
    }
}
