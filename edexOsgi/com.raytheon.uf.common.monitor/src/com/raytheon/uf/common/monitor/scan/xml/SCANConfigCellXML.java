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

@XmlRootElement(name = "ScanConfigCellTable")
@XmlAccessorType(XmlAccessType.NONE)
public class SCANConfigCellXML extends SCANAbstractXML implements
        ISerializableObject {
    @XmlElement(name = "LinkToFrame")
    private boolean linkToFrame;

    @XmlElement(name = "VertOption")
    private boolean vertOption;

    @XmlElement(name = "TipsOption")
    private boolean tipsOption;

    @XmlElement(name = "FilterOption")
    private boolean filterOption;

    @XmlElement(name = "MinRadius")
    private int minRadius;

    @XmlElement(name = "MaxRadius")
    private int maxRadius;

    @XmlElement(name = "RadLow")
    private double radLow;

    @XmlElement(name = "RadHigh")
    private double radHigh;

    @XmlElement(name = "ArrowMode")
    private boolean arrowMode;

    @XmlElement(name = "ArrowConversion")
    private int arrowConversion;

    @XmlElement(name = "SymsCircleHigh")
    private boolean symsCircleHigh;

    @XmlElement(name = "SymsCircleMid")
    private boolean symsCircleMid;

    @XmlElement(name = "SymsCircleLow")
    private boolean symsCircleLow;

    @XmlElement(name = "FutureTracks")
    private boolean futureTracks;

    @XmlElement(name = "PastTracks")
    private boolean pastTracks;

    @XmlElement(name = "SymsArrowHigh")
    private boolean symsArrowHigh;

    @XmlElement(name = "SymsArrowMid")
    private boolean symsArrowMid;

    @XmlElement(name = "SymsArrowLow")
    private boolean symsArrowLow;

    @XmlElement(name = "SymsIdHigh")
    private boolean symsIdHigh;

    @XmlElement(name = "SymsIdMid")
    private boolean symsIdMid;

    @XmlElement(name = "SymsIdLow")
    private boolean symsIdLow;

    @XmlElement(name = "ZoomFactor")
    private int zoomFactor;

    @XmlElement(name = "RadVar")
    private String radVar;

    @XmlElement(name = "ClutterControl")
    private String clutterControl;

    @XmlElement(name = "DefaultRank")
    private String defaultRank;

    @XmlElement(name = "AlarmsDisabled")
    private boolean alarmsDisabled;

    @XmlElement(name = "AlarmBellOn")
    private boolean alarmBellOn;

    @XmlElements({ @XmlElement(name = "Attribute", type = SCANAttributesXML.class) })
    private ArrayList<SCANAttributesXML> attributesData;

    public SCANConfigCellXML() {
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

    public int getMinRadius() {
        return minRadius;
    }

    public void setMinRadius(int minRadius) {
        this.minRadius = minRadius;
    }

    public int getMaxRadius() {
        return maxRadius;
    }

    public void setMaxRadius(int maxRadius) {
        this.maxRadius = maxRadius;
    }

    public double getRadLow() {
        return radLow;
    }

    public void setRadLow(double radLow) {
        this.radLow = radLow;
    }

    public double getRadHigh() {
        return radHigh;
    }

    public void setRadHigh(double radHigh) {
        this.radHigh = radHigh;
    }

    public boolean getArrowMode() {
        return arrowMode;
    }

    public void setArrowMode(boolean arrowMode) {
        this.arrowMode = arrowMode;
    }

    public int getArrowConversion() {
        return arrowConversion;
    }

    public void setArrowConversion(int arrowConversion) {
        this.arrowConversion = arrowConversion;
    }

    public boolean getSymsCircleHigh() {
        return symsCircleHigh;
    }

    public boolean getSymsCircleLow() {
        return symsCircleLow;
    }

    public boolean getSymsCircleMid() {
        return symsCircleMid;
    }

    public void setSymsCircleHigh(boolean symsCircleHigh) {
        this.symsCircleHigh = symsCircleHigh;
    }

    public void setSymsCircleMid(boolean symsCircleMid) {
        this.symsCircleMid = symsCircleMid;
    }

    public void setSymsCircleLow(boolean symsCircleLow) {
        this.symsCircleLow = symsCircleLow;
    }

    public boolean getFutureTracks() {
        return futureTracks;
    }

    public boolean getPastTracks() {
        return pastTracks;
    }

    public void setFutureTracks(boolean futureTracks) {
        this.futureTracks = futureTracks;
    }

    public void setPastTracks(boolean pastTracks) {
        this.pastTracks = pastTracks;
    }

    public void setSymsArrowHigh(boolean symsArrow) {
        this.symsArrowHigh = symsArrow;
    }

    public boolean getSymsArrowHigh() {
        return symsArrowHigh;
    }

    public void setSymsArrowMid(boolean symsArrow) {
        this.symsArrowMid = symsArrow;
    }

    public boolean getSymsArrowMid() {
        return symsArrowMid;
    }

    public void setSymsArrowLow(boolean symsArrow) {
        this.symsArrowLow = symsArrow;
    }

    public boolean getSymsArrowLow() {
        return symsArrowLow;
    }

    public boolean getSymsIdHigh() {
        return symsIdHigh;
    }

    public boolean getSymsIdMid() {
        return symsIdMid;
    }

    public boolean getSymsIdLow() {
        return symsIdLow;
    }

    public void setSymsIdHigh(boolean symsId) {
        this.symsIdHigh = symsId;
    }

    public void setSymsIdMid(boolean symsId) {
        this.symsIdMid = symsId;
    }

    public void setSymsIdLow(boolean symsId) {
        this.symsIdLow = symsId;
    }

    public int getZoomFactor() {
        return zoomFactor;
    }

    public void setZoomFactor(int zoomFactor) {
        this.zoomFactor = zoomFactor;
    }

    public String getRadVar() {
        return radVar;
    }

    public void setRadVar(String radVar) {
        this.radVar = radVar;
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
