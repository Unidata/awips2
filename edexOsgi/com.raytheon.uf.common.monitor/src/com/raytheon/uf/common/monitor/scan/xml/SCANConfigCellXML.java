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

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

/**
 *
 * SCAN config XML for CELL table
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer   Description
 * ------------- -------- ---------- -------------------------------------------
 * Jul 09, 2018  6673     tgurney    Implement combine()
 *
 * </pre>
 *
 * @author unknown
 */
@XmlRootElement(name = "ScanConfigCellTable")
@XmlAccessorType(XmlAccessType.NONE)
public class SCANConfigCellXML extends SCANAbstractXML<SCANConfigCellXML> {

    @XmlElement(name = "LinkToFrame")
    private Boolean linkToFrame;

    @XmlElement(name = "VertOption")
    private Boolean vertOption;

    @XmlElement(name = "TipsOption")
    private Boolean tipsOption;

    @XmlElement(name = "FilterOption")
    private Boolean filterOption;

    @XmlElement(name = "MinRadius")
    private Integer minRadius;

    @XmlElement(name = "MaxRadius")
    private Integer maxRadius;

    @XmlElement(name = "RadLow")
    private Double radLow;

    @XmlElement(name = "RadHigh")
    private Double radHigh;

    @XmlElement(name = "ArrowMode")
    private Boolean arrowMode;

    @XmlElement(name = "ArrowConversion")
    private Integer arrowConversion;

    @XmlElement(name = "SymsCircleHigh")
    private Boolean symsCircleHigh;

    @XmlElement(name = "SymsCircleMid")
    private Boolean symsCircleMid;

    @XmlElement(name = "SymsCircleLow")
    private Boolean symsCircleLow;

    @XmlElement(name = "FutureTracks")
    private Boolean futureTracks;

    @XmlElement(name = "PastTracks")
    private Boolean pastTracks;

    @XmlElement(name = "SymsArrowHigh")
    private Boolean symsArrowHigh;

    @XmlElement(name = "SymsArrowMid")
    private Boolean symsArrowMid;

    @XmlElement(name = "SymsArrowLow")
    private Boolean symsArrowLow;

    @XmlElement(name = "SymsIdHigh")
    private Boolean symsIdHigh;

    @XmlElement(name = "SymsIdMid")
    private Boolean symsIdMid;

    @XmlElement(name = "SymsIdLow")
    private Boolean symsIdLow;

    @XmlElement(name = "ZoomFactor")
    private Integer zoomFactor;

    @XmlElement(name = "RadVar")
    private String radVar;

    @XmlElement(name = "ClutterControl")
    private String clutterControl;

    @XmlElement(name = "DefaultRank")
    private String defaultRank;

    @XmlElement(name = "AlarmsDisabled")
    private Boolean alarmsDisabled;

    @XmlElement(name = "AlarmBellOn")
    private Boolean alarmBellOn;

    @XmlElements({
            @XmlElement(name = "Attribute", type = SCANAttributesXML.class) })
    private List<SCANAttributesXML> attributesData;

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

    public List<SCANAttributesXML> getAttributesData() {
        return attributesData;
    }

    public void setAttributesData(List<SCANAttributesXML> attributesData) {
        this.attributesData = attributesData;
    }

    @Override
    public void combine(SCANConfigCellXML other) {
        if (other.alarmBellOn != null) {
            this.alarmBellOn = other.alarmBellOn;
        }
        if (other.alarmsDisabled != null) {
            this.alarmsDisabled = other.alarmsDisabled;
        }
        if (other.arrowConversion != null) {
            this.arrowConversion = other.arrowConversion;
        }
        if (other.arrowMode != null) {
            this.arrowMode = other.arrowMode;
        }
        if (other.clutterControl != null) {
            this.clutterControl = other.clutterControl;
        }
        if (other.defaultRank != null) {
            this.defaultRank = other.defaultRank;
        }
        if (other.filterOption != null) {
            this.filterOption = other.filterOption;
        }
        if (other.futureTracks != null) {
            this.futureTracks = other.futureTracks;
        }
        if (other.linkToFrame != null) {
            this.linkToFrame = other.linkToFrame;
        }
        if (other.maxRadius != null) {
            this.maxRadius = other.maxRadius;
        }
        if (other.minRadius != null) {
            this.minRadius = other.minRadius;
        }
        if (other.pastTracks != null) {
            this.pastTracks = other.pastTracks;
        }
        if (other.radHigh != null) {
            this.radHigh = other.radHigh;
        }
        if (other.radLow != null) {
            this.radLow = other.radLow;
        }
        if (other.radVar != null) {
            this.radVar = other.radVar;
        }
        if (other.symsArrowHigh != null) {
            this.symsArrowHigh = other.symsArrowHigh;
        }
        if (other.symsArrowLow != null) {
            this.symsArrowLow = other.symsArrowLow;
        }
        if (other.symsArrowMid != null) {
            this.symsArrowMid = other.symsArrowMid;
        }
        if (other.symsCircleHigh != null) {
            this.symsCircleHigh = other.symsCircleHigh;
        }
        if (other.symsCircleLow != null) {
            this.symsCircleLow = other.symsCircleLow;
        }
        if (other.symsCircleMid != null) {
            this.symsCircleMid = other.symsCircleMid;
        }
        if (other.symsIdHigh != null) {
            this.symsIdHigh = other.symsIdHigh;
        }
        if (other.symsIdLow != null) {
            this.symsIdLow = other.symsIdLow;
        }
        if (other.symsIdMid != null) {
            this.symsIdMid = other.symsIdMid;
        }
        if (other.tipsOption != null) {
            this.tipsOption = other.tipsOption;
        }
        if (other.vertOption != null) {
            this.vertOption = other.vertOption;
        }
        if (other.zoomFactor != null) {
            this.zoomFactor = other.zoomFactor;
        }
        combineAttributesData(this.attributesData, other.attributesData);
    }
}
