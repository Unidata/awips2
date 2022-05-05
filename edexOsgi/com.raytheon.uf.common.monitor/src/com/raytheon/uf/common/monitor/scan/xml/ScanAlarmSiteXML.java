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

/**
 * Alarm settings for a specific SCAN site
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Jan 30, 2019  7655     tgurney   Initial creation
 *
 * </pre>
 *
 * @author tgurney
 */

@XmlAccessorType(XmlAccessType.NONE)
public class ScanAlarmSiteXML {
    @XmlElement(name = "scanSite")
    private String scanSite;

    @XmlElement(name = "CELL")
    private Integer cellAlarmTime;

    @XmlElement(name = "DMD")
    private Integer dmdAlarmTime;

    @XmlElement(name = "MESO")
    private Integer mesoAlarmTime;

    @XmlElement(name = "TVS")
    private Integer tvsAlarmTime;

    public ScanAlarmSiteXML() {
    }

    public String getScanSite() {
        return scanSite;
    }

    public void setScanSite(String scanSite) {
        this.scanSite = scanSite;
    }

    public Integer getCellAlarmTime() {
        return cellAlarmTime;
    }

    /**
     * @param cellAlarmTime
     *            the cellAlarmTime to set in minutes
     */
    public void setCellAlarmTime(Integer cellAlarmTime) {
        this.cellAlarmTime = cellAlarmTime;
    }

    /**
     * @return the dmdAlarmTime in minutes
     */
    public Integer getDmdAlarmTime() {
        return dmdAlarmTime;
    }

    /**
     * @param dmdAlarmTime
     *            the dmdAlarmTime to set in minutes
     */
    public void setDmdAlarmTime(Integer dmdAlarmTime) {
        this.dmdAlarmTime = dmdAlarmTime;
    }

    /**
     * @return the mesoAlarmTime in minutes
     */
    public Integer getMesoAlarmTime() {
        return mesoAlarmTime;
    }

    /**
     * @param mesoAlarmTime
     *            the mesoAlarmTime to set in minutes
     */
    public void setMesoAlarmTime(Integer mesoAlarmTime) {
        this.mesoAlarmTime = mesoAlarmTime;
    }

    /**
     * @return the tvsAlarmTime in minutes
     */
    public Integer getTvsAlarmTime() {
        return tvsAlarmTime;
    }

    /**
     * @param tvsAlarmTime
     *            the tvsAlarmTime to set in minutes
     */
    public void setTvsAlarmTime(Integer tvsAlarmTime) {
        this.tvsAlarmTime = tvsAlarmTime;
    }

}
