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
package com.raytheon.viz.hydro.stationreporting;

/**
 * This class contains station reporting DCP and Telem timing data. This class
 * also implements Comparable so the data can be sorted by ID.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * OCT 8, 2007  1580       askripsky   Initial creation.
 * 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 * 
 */
public class StationReportingTimingData implements
        Comparable<StationReportingTimingData> {

    public StationReportingTimingData(String lid, String dcpFreq,
            String dcpTime, String telemFreq) {
        this.lid = lid;
        this.dcpFrequency = dcpFreq;
        this.dcpTime = dcpTime;
        this.telemFrequency = telemFreq;
    }

    public String lid;

    public String dcpFrequency;

    public String dcpTime;

    public String telemFrequency;

    /**
     * compareTo method to compare location (station) IDs.
     * 
     * @param obj
     *            Object to compare to.
     * @return -1 if ID is less than, 0 if ID is equal to, or 1 id ID is greater
     *         than the object ID that was passed in.
     */
    public int compareTo(StationReportingTimingData obj) {
        return lid.compareTo(obj.lid);
    }
}