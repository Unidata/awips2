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

import com.raytheon.uf.common.serialization.ISerializableObject;

@XmlAccessorType(XmlAccessType.NONE)
public class SCANAttributesXML implements ISerializableObject
{
    @XmlElement(name = "AttrName")
    private String attrName;
    
    @XmlElement(name = "Low")
    private double low;
    
    @XmlElement(name = "Mid")
    private double mid;
    
    @XmlElement(name = "Upper")
    private double upper;
    
    @XmlElement(name = "Rank")
    private String rank;
    
    @XmlElement(name = "Trend")
    private boolean trend;
    
    @XmlElement(name = "TimeHeight")
    private boolean timeHeight;
    
    @XmlElement(name = "Colored")
    private boolean colored;
    
    @XmlElement(name = "Clutter")
    private boolean clutter;
    
    @XmlElement(name = "InTable")
    private boolean inTable;
    
    @XmlElement(name = "Min")
    private double min;
    
    @XmlElement(name = "Interval")
    private double interval;
    
    @XmlElement(name = "Range")
    private double range;
    
    @XmlElement(name = "Alarm")
    private int alarm;
    
    @XmlElement(name = "AbsAlarm")
    private int absAlarm;
    
//    @XmlElement(name = "AlarmDisabled")
//    private boolean alarmDisabled;
    
    @XmlElement(name = "HasAlarm")
    private boolean hasAlarm;
    
    @XmlElement(name = "Units")
    private String units;
    
    public SCANAttributesXML()
    {        
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

    public int getAlarm() {
        return alarm;
    }

    public void setAlarm(int alarm) {
        this.alarm = alarm;
    }

    public int getAbsAlarm() {
        return absAlarm;
    }

    public void setAbsAlarm(int absAlarm) {
        this.absAlarm = absAlarm;
    }

//    public boolean getAlarmDisabled() {
//        return alarmDisabled;
//    }
//
//    public void setAlarmDisabled(boolean alarmDisabled) {
//        this.alarmDisabled = alarmDisabled;
//    }

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
}
