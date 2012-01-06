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
package com.raytheon.viz.hydro.timeseries.table;


/**
 * Forecast Attribute Data Object.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 26, 2009            mpduff     Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0	
 */

public class ForecastDataAttribute {
    private String productId = null;
    private String time = null;
    private String basisTime = null;
    private String[] typeSource = null;
    private String selectedTS = null;
    
    public ForecastDataAttribute(String productId, String time, String basisTime, String[] typeSource) {
        this.productId = productId;
        this.time = time;
        this.basisTime = basisTime;
        this.typeSource = typeSource;
    }
    
    public ForecastDataAttribute getData() {
        return this;
    }

    /**
     * @return the productId
     */
    public String getProductId() {
        return productId;
    }

    /**
     * @param productId the productId to set
     */
    public void setProductId(String productId) {
        this.productId = productId;
    }

    /**
     * @return the time
     */
    public String getTime() {
        return time;
    }

    /**
     * @param time the time to set
     */
    public void setTime(String time) {
        this.time = time;
    }

    /**
     * @return the basisTime
     */
    public String getBasisTime() {
        return basisTime;
    }

    /**
     * @param basisTime the basisTime to set
     */
    public void setBasisTime(String basisTime) {
        this.basisTime = basisTime;
    }

    /**
     * @return the typeSource
     */
    public String[] getTypeSource() {
        return typeSource;
    }

    /**
     * @param typeSource the typeSource to set
     */
    public void setTypeSource(String[] typeSource) {
        this.typeSource = typeSource;
    }

    /**
     * @return the selectedTS
     */
    public String getSelectedTS() {
        return selectedTS;
    }

    /**
     * @param selectedTS the selectedTS to set
     */
    public void setSelectedTS(String selectedTS) {
        this.selectedTS = selectedTS;
    }
}
