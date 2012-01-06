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

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

@XmlRootElement(name = "ScanConfigDMDTable")
@XmlAccessorType(XmlAccessType.NONE)
public class SCANConfigDmdXML extends SCANAbstractXML implements ISerializableObject
{
    @XmlElement(name = "LinkToFrame")
    private boolean linkToFrame;
    
    @XmlElement(name = "VertOption")
    private boolean vertOption;
    
    @XmlElement(name = "TipsOption")
    private boolean tipsOption;
    
    @XmlElement(name = "FilterOption")
    private boolean filterOption;
    
    @XmlElement(name = "ZoomFactor")
    private int zoomFactor;
    
    @XmlElement(name = "OverlapOpt")
    private boolean overlapOpt;
    
    @XmlElement(name = "TrackOpt")
    private boolean trackOpt;
    
    @XmlElement(name = "ClutterControl")
    private String clutterControl;
    
    @XmlElement(name = "DefaultRank")
    private String defaultRank;
    
    @XmlElement(name = "RadVar")
    private String radVar;
    
    @XmlElement(name = "AlarmsDisabled")
    private boolean alarmsDisabled;
    
    @XmlElement(name = "AlarmBellOn")
    private boolean alarmBellOn;
    
    @XmlElements( { @XmlElement(name = "Attribute", type = SCANAttributesXML.class) })
    private ArrayList<SCANAttributesXML> attributesData;

    public SCANConfigDmdXML()
    {        
    }

    public boolean getLinkToFrame() {
        return linkToFrame;
    }

    public void setLinkToFrame(boolean linkToFrame) {
        this.linkToFrame = linkToFrame;
    }

    public boolean getVertOption() {
        return vertOption;
    }

    public void setVertOption(boolean vertOption) {
        this.vertOption = vertOption;
    }

    public boolean getTipsOption() {
        return tipsOption;
    }

    public void setTipsOption(boolean tipsOption) {
        this.tipsOption = tipsOption;
    }

    public boolean getFilterOption() {
        return filterOption;
    }

    public void setFilterOption(boolean filterOption) {
        this.filterOption = filterOption;
    }

    public int getZoomFactor() {
        return zoomFactor;
    }

    public void setZoomFactor(int zoomFactor) {
        this.zoomFactor = zoomFactor;
    }

    public boolean getOverlapOpt() {
        return overlapOpt;
    }

    public void setOverlapOpt(boolean overlapOpt) {
        this.overlapOpt = overlapOpt;
    }

    public boolean getTrackOpt() {
        return trackOpt;
    }

    public void setTrackOpt(boolean trackOpt) {
        this.trackOpt = trackOpt;
    }

    public String getClutterControl() {
        return clutterControl;
    }

    public void setClutterControl(String clutterControl) {
        this.clutterControl = clutterControl;
    }

    public String getDefaultRank() {
        return defaultRank;
    }

    public void setDefaultRank(String defaultRank) {
        this.defaultRank = defaultRank;
    }

    public String getRadVar() {
        return radVar;
    }

    public void setRadVar(String radVar) {
        this.radVar = radVar;
    }
    
    public boolean getAlarmsDisabled() {
        return alarmsDisabled;
    }

    public void setAlarmsDisabled(boolean alarmsDisabled) {
        this.alarmsDisabled = alarmsDisabled;
    }
    
    public boolean getAlarmBell() {
        return alarmBellOn;
    }

    public void setAlarmBell(boolean alarmBellOn) {
        this.alarmBellOn = alarmBellOn;
    }

    public ArrayList<SCANAttributesXML> getAttributesData() {
        return attributesData;
    }

    public void setAttributesData(ArrayList<SCANAttributesXML> attributesData) {
        this.attributesData = attributesData;
    }
}
