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

package com.raytheon.viz.hydrocommon.datasources;

/**
 * This class contains data sources DCP data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 10/6/2008    1555       grichard    Initial creation.
 * 
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 * 
 */
public class DataSourcesDcpData {

    /**
     * Location (Station) ID.
     */
    private String lid;

    /**
     * Criteria.
     */
    private String criteria;

    /**
     * Owner.
     */
    private String owner;

    /**
     * GOES.
     */
    private String goes;

    /**
     * Report Frequency.
     */
    private String rptFreq;

    /**
     * Report Time.
     */
    private String rptTime;

    /**
     * Notify.
     */
    private String notify;

    /**
     * Constructor
     * 
     * @param lid --
     *            location identifier
     * @param criteria --
     *            criteria
     * @param goes --
     *            GOES
     * @param rptFreq --
     *            report frequency
     * @param rptTime --
     *            report time
     * @param notify --
     *            notify
     */
    public DataSourcesDcpData(String lid, String criteria, String goes,
            String rptFreq, String rptTime, String notify) {
        this.lid = lid;
        this.criteria = criteria;
        // this.owner = owner;
        this.goes = goes;
        this.rptFreq = rptFreq;
        this.rptTime = rptTime;
        this.notify = notify;
    }

    /**
     * Get the location (station) ID.
     * 
     * @return location (station) ID.
     */
    public String getLid() {
        return lid;
    }

    /**
     * Get the criteria.
     * 
     * @return criteria
     */
    public String getCriteria() {
        return criteria;
    }

    /**
     * Get the owner.
     * 
     * @return owner
     */
    public String getOwner() {
        return owner;
    }

    /**
     * Get the GOES data.
     * 
     * @return goes
     */
    public String getGoes() {
        return goes;
    }

    /**
     * Get the report frequency.
     * 
     * @return rptFreq
     */
    public String getRptFreq() {
        return rptFreq;
    }

    /**
     * Get the report time.
     * 
     * @return rptTime
     */
    public String getRptTime() {
        return rptTime;
    }

    /**
     * Get the notify data.
     * 
     * @return notify
     */
    public String getNotify() {
        return notify;
    }

    /**
     * Get the data in a formatted string.
     * 
     * @return The formatted data in a string format.
     */
    @Override
    public String toString() {
        return String.format("%s %s %s %s %s %s", lid, criteria, goes, rptFreq,
                rptTime, notify);
    }

}
