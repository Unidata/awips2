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
package com.raytheon.uf.viz.d2d.core.time;

import java.util.Date;

import com.raytheon.uf.common.time.DataTime;

/**
 * Configuration parameters from the AbstractTimeMatchingConfigurationFactory.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 14, 2009            bgonzale     Initial creation
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */
public class TimeMatchingConfiguration {

    private boolean cancel = false;

    private DataTime[] dataTimes;

    // forecast in seconds
    private Long forecast;

    // delta time in milliseconds
    private Long delta;

    private Date clock;

    // tolerance in % * 100
    private Float tolerance;

    private LoadMode loadMode;

    private boolean timeMatchBasis = false;

    /**
     * Default Constructor
     */
    public TimeMatchingConfiguration() {
    }

    protected TimeMatchingConfiguration(TimeMatchingConfiguration other) {
        if (other.getDataTimes() == null) {
            this.dataTimes = null;
        } else {
            this.dataTimes = new DataTime[other.getDataTimes().length];
            for (int i = 0; i < other.getDataTimes().length; i++) {
                this.dataTimes[i] = other.getDataTimes()[i].clone();
            }
        }
        this.forecast = other.getForecast();
        this.delta = other.getDelta();
        this.clock = other.getClock();
        this.tolerance = other.getTolerance();
        this.loadMode = other.getLoadMode();
        this.cancel = other.cancel;
    }

    public TimeMatchingConfiguration clone() {
        return new TimeMatchingConfiguration(this);
    }

    /**
     * @return the dataTimes
     */
    public DataTime[] getDataTimes() {
        return dataTimes;
    }

    /**
     * @param dataTimes
     *            the dataTimes to set
     */
    public void setDataTimes(DataTime[] dataTimes) {
        this.dataTimes = dataTimes;
    }

    /**
     * @return the forecast
     */
    public Long getForecast() {
        return forecast;
    }

    /**
     * @param forecast
     *            the forecast to set
     */
    public void setForecast(Long forecast) {
        this.forecast = forecast;
    }

    /**
     * @return the delta
     */
    public Long getDelta() {
        return delta;
    }

    /**
     * @param delta
     *            the delta to set
     */
    public void setDelta(Long delta) {
        this.delta = delta;
    }

    /**
     * @return the clock
     */
    public Date getClock() {
        return clock;
    }

    /**
     * @param clock
     *            the clock to set
     */
    public void setClock(Date clock) {
        this.clock = clock;
    }

    /**
     * @return the tolerance
     */
    public Float getTolerance() {
        return tolerance;
    }

    /**
     * @param tolerance
     *            the tolerance to set
     */
    public void setTolerance(Float tolerance) {
        this.tolerance = tolerance;
    }

    /**
     * @return the loadMode
     */
    public LoadMode getLoadMode() {
        return loadMode;
    }

    /**
     * @param loadMode
     *            the loadMode to set
     */
    public void setLoadMode(LoadMode loadMode) {
        this.loadMode = loadMode;
    }

    /**
     * @return the timeMatchBasis
     */
    public boolean isTimeMatchBasis() {
        return timeMatchBasis;
    }

    /**
     * @param timeMatchBasis
     *            the timeMatchBasis to set
     */
    public void setTimeMatchBasis(boolean timeMatchBasis) {
        this.timeMatchBasis = timeMatchBasis;
    }

    /**
     * @return the cancel
     */
    public boolean isCancel() {
        return cancel;
    }

    /**
     * @param cancel
     *            the cancel to set
     */
    public void setCancel(boolean cancel) {
        this.cancel = cancel;
    }

}
