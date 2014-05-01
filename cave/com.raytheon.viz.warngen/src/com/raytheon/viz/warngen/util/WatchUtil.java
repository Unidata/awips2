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
package com.raytheon.viz.warngen.util;

import java.util.ArrayList;
import java.util.Date;
import java.util.Set;
import java.util.TreeSet;

/**
 * This utility will provide an object to be sent into velocity templates which
 * will allow the template to output current Warnings.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 1, 2009            bwoodle     Initial creation
 * 
 * </pre>
 * 
 * @author bwoodle
 * @version 1.0
 */

public class WatchUtil {

    private Date latestTorTime;

    private Date latestSvrTime;

    private ArrayList<WeatherAdvisoryWatch> torWatches;

    private ArrayList<WeatherAdvisoryWatch> svrWatches;

    public WatchUtil() {
        torWatches = new ArrayList<WeatherAdvisoryWatch>();
        svrWatches = new ArrayList<WeatherAdvisoryWatch>();
    }

    public void addWaw(WeatherAdvisoryWatch watch) {
        if (watch.getPhensig().equalsIgnoreCase("SV.A")) {
            svrWatches.add(watch);
            if (latestSvrTime == null
                    || watch.getEndTime().after(latestSvrTime)) {
                latestSvrTime = watch.getEndTime();
            }
        } else if (watch.getPhensig().equalsIgnoreCase("TO.A")) {
            torWatches.add(watch);
            if (latestTorTime == null
                    || watch.getEndTime().after(latestTorTime)) {
                latestTorTime = watch.getEndTime();
            }
        }
    }

    public ArrayList<WeatherAdvisoryWatch> getTorWatches() {
        Set<WeatherAdvisoryWatch> rval = new TreeSet<WeatherAdvisoryWatch>();
        for (WeatherAdvisoryWatch w : torWatches) {
            w.setEndTime(latestTorTime);
            rval.add(w);
        }
        return new ArrayList<WeatherAdvisoryWatch>(rval);
    }

    public ArrayList<WeatherAdvisoryWatch> getSvrWatches() {
        Set<WeatherAdvisoryWatch> rval = new TreeSet<WeatherAdvisoryWatch>();
        for (WeatherAdvisoryWatch w : svrWatches) {
            w.setEndTime(latestSvrTime);
            rval.add(w);
        }
        return new ArrayList<WeatherAdvisoryWatch>(rval);
    }

    public Date getLatestTorTime() {
        return latestTorTime;
    }

    public Date getLatestSvrTime() {
        return latestSvrTime;
    }
}
