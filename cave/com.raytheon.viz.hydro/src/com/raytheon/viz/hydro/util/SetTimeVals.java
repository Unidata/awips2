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
package com.raytheon.viz.hydro.util;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import com.raytheon.uf.common.dataplugin.shef.tables.Rpfparams;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.HydroConstants;

/**
 * Set the forecast time values.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 31, 2009            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class SetTimeVals {
    private int obshrs;

    private int fcsthrs;

    private int basishrs;

    private Calendar obsBeginTime = Calendar.getInstance(TimeZone.getTimeZone("GMT"));

    private Calendar fcstEndTime = Calendar.getInstance(TimeZone.getTimeZone("GMT"));

    private Calendar basisTime = Calendar.getInstance(TimeZone.getTimeZone("GMT"));

    public SetTimeVals() throws VizException {
        getHrVals();
        setTimeValues();
    }

    /*********************************************************************
     * set_timevals()
     * 
     * PURPOSE Set the beginning of the observed window and the end of the
     * forecast window.
     ********************************************************************/

    /**
     * Set the beginning of the observed window and the end of the forecast
     * window.
     * 
     * @param currentTime
     *            The current time
     */
    public void setTimeValues() {
        long currentMillis = SimulatedTime.getSystemTime().getTime().getTime();

        obsBeginTime.setTimeInMillis(currentMillis - (HydroConstants.MILLIS_PER_HOUR * (long) obshrs));
        fcstEndTime.setTimeInMillis(currentMillis + (HydroConstants.MILLIS_PER_HOUR * (long) fcsthrs));
        basisTime.setTimeInMillis(currentMillis - (HydroConstants.MILLIS_PER_HOUR * (long) basishrs));
    }

    /**
     * Get the time window hourly offsets for general use throughout the
     * program.
     */
    private void getHrVals() throws VizException {
        int dbObshrs;
        int dbFcsthrs;
        int tokenBasishrs;

        /* retrieve the look-back, look-forward intervals */
        AppsDefaults appsDefaults = AppsDefaults.getInstance();

        ArrayList<Rpfparams> results = null;
        Rpfparams rpfPtr = null;
        int hrValue;

        /* initialize */

        dbObshrs = 72;
        dbFcsthrs = 72;
        tokenBasishrs = 72;

        /* get the database values */
        results = (ArrayList<Rpfparams>) HydroData.getRpfParams(" ");

        if ((results == null) || (results.size() == 0)) {
            // TODO log this message
            // fprintf(stderr,
            // "No records in RpfParams table, using defaults\n");
        } else {
            rpfPtr = results.get(0);
            dbObshrs = rpfPtr.getId().getObshrs();
            dbFcsthrs = rpfPtr.getId().getFcsthrs();
        }

        /* get the token value */
        String basisHrs = appsDefaults.getToken("basis_hours_filter");

        if ((basisHrs != null) && (basisHrs.length() > 0)) {
            hrValue = Integer.parseInt(basisHrs);

            if ((hrValue <= 0) || (hrValue > 480)) {
                // TODO Log this message
                // fprintf(stderr,
                // "invalid value for basis_hours_filter token: %s\n",basis_hours_str);
            } else {
                tokenBasishrs = hrValue;
            }
        }

        basishrs = tokenBasishrs;
        fcsthrs = dbFcsthrs;
        obshrs = dbObshrs;
    }

    /**
     * @return the obsBeginTime
     */
    public Date getObsBeginTime() {
        return obsBeginTime.getTime();
    }

    /**
     * @param obsBeginTime
     *            the obsBeginTime to set
     */
    public void setObsBeginTime(Date obsBeginTime) {
        this.obsBeginTime.setTimeInMillis(obsBeginTime.getTime());
    }

    /**
     * @return the fcstEndTime
     */
    public Date getFcstEndTime() {
        return fcstEndTime.getTime();
    }

    /**
     * @param fcstEndTime
     *            the fcstEndTime to set
     */
    public void setFcstEndTime(Date fcstEndTime) {
        this.fcstEndTime.setTimeInMillis(fcstEndTime.getTime());
    }

    /**
     * @return the basisTime
     */
    public Date getBasisTime() {
        return basisTime.getTime();
    }

    /**
     * @param basisTime
     *            the basisTime to set
     */
    public void setBasisTime(Date basisTime) {
        this.basisTime.setTimeInMillis(basisTime.getTime());
    }
}
