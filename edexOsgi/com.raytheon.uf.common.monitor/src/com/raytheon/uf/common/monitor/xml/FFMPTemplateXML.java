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
package com.raytheon.uf.common.monitor.xml;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * The FFMP Template configuration
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * --/--/----                          Initial creation
 * 
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

@XmlRootElement(name = "FFMPTemplate")
@XmlAccessorType(XmlAccessType.NONE)
public class FFMPTemplateXML {

    @XmlAttribute(name = "hucDepth")
    private Integer hucDepth;

    @XmlAttribute(name = "numberOfHuc")
    private Integer numberOfHuc;

    @XmlAttribute(name = "virtual")
    private boolean virtual;

    @XmlAttribute(name = "extents")
    private Double extents;

    @XmlElement(name = "excludedVGBs", type = VGBXML.class)
    private VGBXML excludedVGBs;

    @XmlElement(name = "regenerate")
    private boolean regenerate = false;

    public Double getExtents() {
        return extents;
    }

    public void setExtents(Double extents) {
        this.extents = extents;
    }

    public Integer getHucDepth() {
        return hucDepth;
    }

    public void setHucDepth(Integer hucDepth) {
        this.hucDepth = hucDepth;
    }

    public Integer getNumberOfHuc() {
        return numberOfHuc;
    }

    public void setNumberOfHuc(Integer numberOfHuc) {
        this.numberOfHuc = numberOfHuc;
    }

    public boolean getVirtual() {
        return virtual;
    }

    public void setVirtual(boolean virtual) {
        this.virtual = virtual;
    }

    public VGBXML getExcludedVGBs() {
        return excludedVGBs;
    }

    public void setExcludedVGBs(VGBXML excludedVGBs) {
        this.excludedVGBs = excludedVGBs;
    }

    public void setRegenerate(boolean regenerate) {
        this.regenerate = regenerate;
    }

    public boolean isRegenerate() {
        return regenerate;
    }

}
