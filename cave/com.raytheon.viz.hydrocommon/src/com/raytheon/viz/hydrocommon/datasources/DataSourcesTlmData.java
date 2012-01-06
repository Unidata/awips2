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
 * This class contains data sources TLM data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 8, 2008  1555       grichard    Initial creation.
 * 12/16/2008   1782       grichard    Refreshed Data Sources.
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */

public class DataSourcesTlmData {

    /**
     * Location (Station) ID.
     */
    private String lid;

    /**
     * Cost.
     */
    private Double cost;

    /**
     * Criteria.
     */
    private String criteria;

    /**
     * Phone.
     */
    private String phone;

    /**
     * Sensor ID.
     */
    private String sensorId;

    /**
     * Report Frequency.
     */
    private String rptFreq;

    public DataSourcesTlmData(String lid, Double cost, String criteria,
            String phone, String sensorId, String rptFreq) {
        this.lid = lid;
        this.cost = cost;
        this.criteria = criteria;
        this.phone = phone;
        this.sensorId = sensorId;
        this.rptFreq = rptFreq;
    }

    /**
     * Getter for LID.
     * 
     * @return lid
     */
    public String getLid() {
        return lid;
    }

    /**
     * Getter for cost.
     * 
     * @return cost
     */
    public Double getCost() {
        return cost;
    }

    /**
     * Getter for criteria.
     * 
     * @return criteria
     */
    public String getCriteria() {
        return criteria;
    }

    /**
     * Getter for phone
     * 
     * @return phone
     */
    public String getPhone() {
        return phone;
    }

    /**
     * Getter for Sensor ID.
     * 
     * @return sensor ID
     */
    public String getSensorId() {
        return sensorId;
    }

    /**
     * Getter for Report Frequency.
     * 
     * @return report frequency
     */
    public String getRptFreq() {
        return rptFreq;
    }

    /**
     * Get the data in a formatted string.
     * 
     * @return The formatted data in a string format.
     */
    @Override
    public String toString() {
        return String.format("%5s %8.2f %s %s %s %s", lid, cost, criteria,
                phone, sensorId, rptFreq);
    }

}
