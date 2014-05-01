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
 * this class contains the Rejected Data data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Oct 28, 2008				askripsky	Initial creation
 * 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */
public class ForecastData extends HydroData {
    /**
     * Probability.
     */
    protected double probability;

    /**
     * Valid Time.
     */
    protected Date validTime;

    /**
     * Basis Time
     */
    protected Date basisTime;

    /**
     * Constructor
     */
    public ForecastData() {
        super();
    }

    /**
     * Constructor that accepts the object array from a db call, assumes a join
     * with location table, where data[1] = location.name
     * 
     * @param data
     *            The raw data from the database.
     */
    public ForecastData(Object[] data) {
        super(data);

        setProbability((Double) data[13]);
        setValidTime((Date) data[14]);
        setBasisTime((Date) data[15]);
    }

    public double getProbability() {
        return probability;
    }

    public void setProbability(double probability) {
        this.probability = probability;
    }

    public Date getValidTime() {
        return validTime;
    }

    public void setValidTime(Date validTime) {
        this.validTime = validTime;
    }

    public Date getBasisTime() {
        return basisTime;
    }

    public void setBasisTime(Date basisTime) {
        this.basisTime = basisTime;
    }
}
