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
package com.raytheon.uf.edex.plugin.hpe.util;

import java.util.Date;

import com.raytheon.uf.edex.plugin.hpe.util.HpeEnums.HpeBiasSource;
import com.raytheon.uf.edex.plugin.hpe.util.HpeEnums.HpeDataSource;

/**
 * HpeRadarResult table data object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 26, 2014    3026    mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class HpeRadarResult {

    private String hpeProductName;

    private Date productTime;

    private int numRadarAvailable;

    private String biasSource;

    private String radarDataSource;

    /**
     * Default constructor.
     */
    public HpeRadarResult() {

    }

    /**
     * @return the hpeProductName
     */
    public String getHpeProductName() {
        return hpeProductName;
    }

    /**
     * @param hpeProductName
     *            the hpeProductName to set
     */
    public void setHpeProductName(String hpeProductName) {
        this.hpeProductName = hpeProductName;
    }

    /**
     * @return the productTime
     */
    public Date getProductTime() {
        return productTime;
    }

    /**
     * @param productTime
     *            the productTime to set
     */
    public void setProductTime(Date productTime) {
        this.productTime = productTime;
    }

    /**
     * @return the numRadarAvailable
     */
    public int getNumRadarAvailable() {
        return numRadarAvailable;
    }

    /**
     * @param numRadarAvailable
     *            the numRadarAvailable to set
     */
    public void setNumRadarAvailable(int numRadarAvailable) {
        this.numRadarAvailable = numRadarAvailable;
    }

    /**
     * @return the biasSource
     */
    public String getBiasSourceString() {
        return biasSource;
    }

    /**
     * @return the biasSource
     */
    public HpeBiasSource getBiasSource() {
        return HpeBiasSource.fromString(biasSource);
    }

    /**
     * @param biasSource
     *            the biasSource to set
     */
    public void setBiasSource(String biasSource) {
        this.biasSource = biasSource;
    }

    /**
     * @return the radarDataSource String value
     */
    public String getRadarDataSourceString() {
        return radarDataSource;
    }

    /**
     * @return the radarDataSource
     */
    public HpeDataSource getRadarDataSource() {
        return Enum.valueOf(HpeDataSource.class, radarDataSource);
    }

    /**
     * @param radarDataSource
     *            the radarDataSource to set
     */
    public void setRadarDataSource(String radarDataSource) {
        this.radarDataSource = radarDataSource;
    }

    /**
     * Determine if this is an empty data object.
     * 
     * @return true if empty object, false if populated with data
     */
    public boolean isEmpty() {
        if (biasSource != null && biasSource.length() > 0) {
            return false;
        }

        if (hpeProductName != null && hpeProductName.length() > 0) {
            return false;
        }

        if (numRadarAvailable > 0) {
            return false;
        }

        if (productTime != null) {
            return false;
        }

        if (radarDataSource != null && radarDataSource.length() > 0) {
            return false;
        }

        // Empty file
        return true;
    }
}