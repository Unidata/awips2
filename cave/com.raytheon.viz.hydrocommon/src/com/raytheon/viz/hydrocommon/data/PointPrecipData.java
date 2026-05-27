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
package com.raytheon.viz.hydrocommon.data;

import java.util.Date;

/**
 * PointPrecipData class represents the point precipitation accumulation data
 * type.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 5, 2008  1662       grichard    Initial creation.
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */

public class PointPrecipData extends HydroData {
    /**
     * OBSTIME.
     */
    protected Date obstime;

    /**
     * DURNAME.
     */
    protected String durname;
    
    /**
     * The max value.
     */
    private double maxValue = -999;
    
    private double amount = -999;
    
    private double hrFill = -999;
    
    private int summedFlag = -999;

    /**
     * Constructor
     */
    public PointPrecipData() {
        super();
    }

    /**
     * Constructor that accepts the object array from a db call.
     * 
     * @param data
     *            The raw data from the database.
     */
    public PointPrecipData(Object[] data) {
        super(data, data[13]);

        setObstime((Date) data[12]);

        // setDurname((String) data[14]);
    }

    /**
     * Getter for the observation time.
     * 
     * @return the observation time
     */
    public Date getObstime() {
        return obstime;
    }

    /**
     * Setter for the observation time.
     * 
     * @param obstime
     */
    public void setObstime(Date obstime) {
        this.obstime = obstime;
    }

    /**
     * Getter for the duration name.
     * 
     * @return the duration name
     */
    public String getDurname() {
        return durname;
    }

    /**
     * Setter for the duration name.
     * 
     * @param durname
     */
    public void setDurname(String durname) {
        this.durname = durname;
    }

    /**
     * @return the maxValue
     */
    public double getMaxValue() {
        return maxValue;
    }

    /**
     * @param maxValue the maxValue to set
     */
    public void setMaxValue(double maxValue) {
        this.maxValue = maxValue;
    }

    /**
     * @return the amount
     */
    public double getAmount() {
        return amount;
    }

    /**
     * @param amount the amount to set
     */
    public void setAmount(double amount) {
        this.amount = amount;
    }

    /**
     * @return the hrFill
     */
    public double getHrFill() {
        return hrFill;
    }

    /**
     * @param hrFill the hrFill to set
     */
    public void setHrFill(double hrFill) {
        this.hrFill = hrFill;
    }

    /**
     * @return the summedFlag
     */
    public int getSummedFlag() {
        return summedFlag;
    }

    /**
     * @param summedFlag the summedFlag to set
     */
    public void setSummedFlag(int summedFlag) {
        this.summedFlag = summedFlag;
    }
}
