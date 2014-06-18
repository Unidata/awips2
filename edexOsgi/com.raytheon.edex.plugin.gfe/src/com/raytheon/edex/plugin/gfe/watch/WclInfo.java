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
/**
 * 
 */
package com.raytheon.edex.plugin.gfe.watch;

import java.util.Collections;
import java.util.Date;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * A class for the information in a WCL.
 * 
 * @author wldougher
 * 
 */
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class WclInfo {

    @XmlAttribute
    @DynamicSerializeElement
    private Date issueTime;

    @XmlElement
    @DynamicSerializeElement
    private List<String> lines;

    @XmlAttribute
    @DynamicSerializeElement
    private String completeProductPil;

    @XmlAttribute
    @DynamicSerializeElement
    private Boolean notify;

    public WclInfo() {
    }

    public WclInfo(long issueTimeInMillis, String completeProductPil,
            List<String> lines, boolean notify) {
        this();
        setIssueTime(new Date(issueTimeInMillis));
        setCompleteProductPil(completeProductPil);
        setLines(lines);
        setNotify(notify);
    }

    /**
     * @return the issue time
     */
    public Date getIssueTime() {
        return issueTime;
    }

    /**
     * @param issueTime
     */
    public void setIssueTime(Date issueTime) {
        this.issueTime = issueTime;
    }

    /**
     * @return the lines
     */
    public List<String> getLines() {
        return lines;
    }

    /**
     * @param lines
     */
    public void setLines(List<String> lines) {
        this.lines = Collections.unmodifiableList(lines);
    }

    /**
     * @return the completeProductPil
     */
    public String getCompleteProductPil() {
        return completeProductPil;
    }

    /**
     * @param completeProductPil
     */
    public void setCompleteProductPil(String completeProductPil) {
        this.completeProductPil = completeProductPil;
    }

    /**
     * @param notify
     *            the notify to set
     */
    public void setNotify(Boolean notify) {
        this.notify = notify;
    }

    /**
     * @return the notify
     */
    public Boolean getNotify() {
        return notify;
    }
}
