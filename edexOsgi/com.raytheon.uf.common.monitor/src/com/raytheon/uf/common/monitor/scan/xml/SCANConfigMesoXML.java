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
 * SCAN config XML for MESO table
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
@XmlRootElement(name = "ScanConfigMesoTable")
@XmlAccessorType(XmlAccessType.NONE)
public class SCANConfigMesoXML extends SCANAbstractXML<SCANConfigMesoXML> {
    @XmlElement(name = "LinkToFrame")
    private Boolean linkToFrame;

    @XmlElement(name = "VertOption")
    private Boolean vertOption;

    @XmlElement(name = "TipsOption")
    private Boolean tipsOption;

    @XmlElement(name = "FilterOption")
    private Boolean filterOption;

    @XmlElement(name = "ZoomFactor")
    private Integer zoomFactor;

    @XmlElement(name = "DefaultRank")
    private String defaultRank;

    @XmlElements({
            @XmlElement(name = "Attribute", type = SCANAttributesXML.class) })
    private List<SCANAttributesXML> attributesData;

    public SCANConfigMesoXML() {
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

    public String getDefaultRank() {
        return defaultRank;
    }

    public void setDefaultRank(String defaultRank) {
        this.defaultRank = defaultRank;
    }

    public List<SCANAttributesXML> getAttributesData() {
        return attributesData;
    }

    public void setAttributesData(List<SCANAttributesXML> attributesData) {
        this.attributesData = attributesData;
    }

    @Override
    public void combine(SCANConfigMesoXML other) {
        if (other.linkToFrame != null) {
            this.linkToFrame = other.linkToFrame;
        }
        if (other.vertOption != null) {
            this.vertOption = other.vertOption;
        }
        if (other.tipsOption != null) {
            this.tipsOption = other.tipsOption;
        }
        if (other.filterOption != null) {
            this.filterOption = other.filterOption;
        }
        if (other.zoomFactor != null) {
            this.zoomFactor = other.zoomFactor;
        }
        if (other.defaultRank != null) {
            this.defaultRank = other.defaultRank;
        }
        combineAttributesData(this.attributesData, other.attributesData);
    }
}
