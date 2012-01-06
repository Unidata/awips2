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
package com.raytheon.viz.avnconfig;

/**
 * This class holds all the data for one TAF site.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 19, 2009            avarani     Initial creation
 * 
 * </pre>
 * 
 * @author avarani
 * @version 1.0
 */

public class TafSiteData {
    public String wmo;

    public String afos;

    public String longitude;

    public String latitude;

    public String elevation;

    public String runway[];

    public String gfsmos;

    public String metar[];

    public String nam;

    public String gfslamp;

    public String nammos;

    public String acars;

    public String profilers[];

    public String radars[];

    public String radarCutoff[];

    public String ceiling[];

    public String profilerCutoff[];

    public String visibility[];

    public String hours;

    public boolean currentWxQc;

    public boolean climateQc;

    public boolean impactQc;

    public String[] getRunway() {
        return runway;
    }

    public String[] getMetar() {
        return metar;
    }

    public String[] getProfilers() {
        return profilers;
    }

    public String[] getRadars() {
        return radars;
    }

    public String[] getRadarCutoff() {
        return radarCutoff;
    }

    public String[] getCeiling() {
        return ceiling;
    }

    public String[] getProfilerCutoff() {
        return profilerCutoff;
    }

    public String[] getVisibility() {
        return visibility;
    }

    public String getTafDuration() {
        return hours;
    }
}
