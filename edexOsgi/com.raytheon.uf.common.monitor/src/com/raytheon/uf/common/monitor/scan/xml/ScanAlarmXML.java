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
import java.util.List;
import java.util.Optional;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;

/**
 * Scan Alarm XML class.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Nov 21, 2010           lvenable  Initial creation
 * Jul 30, 2018  6685     randerso  Code cleanup.
 * Jan 31, 2019  7655     tgurney   Add site-specific config
 *
 * </pre>
 *
 * @author lvenable
 */

@XmlRootElement(name = "ScanAlarms")
@XmlAccessorType(XmlAccessType.NONE)
public class ScanAlarmXML {

    public static final int MAX_ALARM_TIME = 999_999;

    @XmlElement(name = "CELL")
    private Integer defaultCellAlarmTime;

    @XmlElement(name = "DMD")
    private Integer defaultDmdAlarmTime;

    @XmlElement(name = "MESO")
    private Integer defaultMesoAlarmTime;

    @XmlElement(name = "TVS")
    private Integer defaultTvsAlarmTime;

    @XmlElements({ @XmlElement(name = "site") })
    private List<ScanAlarmSiteXML> sites = new ArrayList<>();

    public ScanAlarmXML() {
    }

    /** @return the default CELL alarm time in minutes */
    public Integer getDefaultCellAlarmTime() {
        return defaultCellAlarmTime;
    }

    /**
     * @param defaultCellAlarmTime
     *            the default CELL alarm time to set, in minutes
     */
    public void setDefaultCellAlarmTime(Integer defaultCellAlarmTime) {
        this.defaultCellAlarmTime = defaultCellAlarmTime;
    }

    /** @return the default DMD alarm time in minutes */
    public Integer getDefaultDmdAlarmTime() {
        return defaultDmdAlarmTime;
    }

    /**
     * @param defaultDmdAlarmTime
     *            the default DMD alarm time to set, in minutes
     */
    public void setDefaultDmdAlarmTime(Integer defaultDmdAlarmTime) {
        this.defaultDmdAlarmTime = defaultDmdAlarmTime;
    }

    /** @return the default MESO alarm time in minutes */
    public Integer getDefaultMesoAlarmTime() {
        return defaultMesoAlarmTime;
    }

    /**
     * @param defaultMesoAlarmTime
     *            the default MESO alarm time to set, in minutes
     */
    public void setDefaultMesoAlarmTime(Integer defaultMesoAlarmTime) {
        this.defaultMesoAlarmTime = defaultMesoAlarmTime;
    }

    /** @return the default TVS alarm time in minutes */
    public Integer getDefaultTvsAlarmTime() {
        return defaultTvsAlarmTime;
    }

    /**
     * @param defaultTvsAlarmTime
     *            the default TVS alarm time to set, in minutes
     */
    public void setDefaultTvsAlarmTime(Integer defaultTvsAlarmTime) {
        this.defaultTvsAlarmTime = defaultTvsAlarmTime;
    }

    public List<ScanAlarmSiteXML> getSites() {
        return sites;
    }

    public void setSites(List<ScanAlarmSiteXML> sites) {
        this.sites = sites;
    }

    /**
     * @return Alarm time for a specific table type and SCAN site
     */
    public int getAlarmTime(ScanTables tableType, String scanSite) {
        Integer rval = getSiteSpecificAlarmTime(tableType, scanSite);
        if (rval == null) {
            rval = getDefaultAlarmTime(tableType);
        }
        return rval;
    }

    /**
     * @param tableType
     * @param scanSite
     * @param alarmTime
     *            alarm time in minutes. null means delete the site-specific
     *            alarm time value if it exists
     */
    public void setAlarmTime(ScanTables tableType, String scanSite,
            Integer alarmTime) {
        ScanAlarmSiteXML siteXml = getSiteSpecificXml(scanSite).orElse(null);
        if (siteXml == null) {
            siteXml = new ScanAlarmSiteXML();
            siteXml.setScanSite(scanSite);
            sites.add(siteXml);
        }
        switch (tableType) {
        case CELL:
            siteXml.setCellAlarmTime(alarmTime);
            break;
        case DMD:
            siteXml.setDmdAlarmTime(alarmTime);
            break;
        case MESO:
            siteXml.setMesoAlarmTime(alarmTime);
            break;
        case TVS:
            siteXml.setTvsAlarmTime(alarmTime);
            break;
        }
        if (siteXml.getCellAlarmTime() == null
                && siteXml.getDmdAlarmTime() == null
                && siteXml.getTvsAlarmTime() == null
                && siteXml.getMesoAlarmTime() == null) {
            // delete empty config
            sites.remove(siteXml);
        }
    }

    public int getDefaultAlarmTime(ScanTables tableType) {
        Integer rval = null;
        switch (tableType) {
        case CELL:
            rval = getDefaultCellAlarmTime();
            break;
        case DMD:
            rval = getDefaultDmdAlarmTime();
            break;
        case MESO:
            rval = getDefaultMesoAlarmTime();
            break;
        case TVS:
            rval = getDefaultTvsAlarmTime();
            break;
        }
        if (rval == null) {
            rval = 999;
        }
        return rval;
    }

    public boolean hasSiteSpecificValue(ScanTables tableType, String scanSite) {
        return getSiteSpecificAlarmTime(tableType, scanSite) != null;
    }

    private Optional<ScanAlarmSiteXML> getSiteSpecificXml(String scanSite) {
        return sites.stream()
                .filter(s -> s.getScanSite().equalsIgnoreCase(scanSite))
                .findFirst();
    }

    private Integer getSiteSpecificAlarmTime(ScanTables tableType,
            String scanSite) {
        Optional<ScanAlarmSiteXML> siteXml = getSiteSpecificXml(scanSite);
        if (siteXml.isPresent()) {
            switch (tableType) {
            case CELL:
                return siteXml.get().getCellAlarmTime();
            case DMD:
                return siteXml.get().getDmdAlarmTime();
            case MESO:
                return siteXml.get().getMesoAlarmTime();
            case TVS:
                return siteXml.get().getTvsAlarmTime();
            default:
                return null;
            }
        }
        return null;
    }
}
