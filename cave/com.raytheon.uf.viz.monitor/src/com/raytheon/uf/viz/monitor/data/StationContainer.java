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
package com.raytheon.uf.viz.monitor.data;
import java.util.Date;
import java.util.LinkedHashMap;

/**
 * Keep Station ObReport's keyed by Time in Hash
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 12/07/09                  dhladky    Initial Creation.
 * 
 * </pre>
 * 
 * @author dhladky
 * 
 */

public class StationContainer {

    public LinkedHashMap<Date, ObReport> container = null;

    public String stationId = null;

    public StationContainer(String stationId) {
        this.stationId = stationId;
        this.container = new LinkedHashMap<Date, ObReport>();
    }

    public void addReport(Date date, ObReport report) {
        synchronized (container) {
            container.put(date, report);
        }
    }

    public ObReport getReport(String stationId) {
        ObReport report = null;
        synchronized (container) {
            if (container.containsKey(stationId)) {
                report = container.get(stationId);
            }
        }
        return report;
    }

    /**
     * future use to remove old data
     * 
     * @param date
     */
    public void removeReport(Date date) {
        synchronized (container) {
            if (container.containsKey(date)) {
                container.remove(date);
            }
        }
    }

    /**
     * Get most recent time up to this point
     * 
     * @param data
     * @return
     */
    public Date getMostRecentTime(Date key) {

        Date time = null;
        synchronized (container) {
            Date[] times = container.keySet().toArray(
                    new Date[container.keySet().size()]);
            // get the most recent time up to the time passed
            // we assume they arranged in ascending order
            for (int i = 0; i < times.length; i++) {
               if (times[i].before(key)) {
                   time = times[i];
               }
            }
        }

        return time;
    }

    public ObReport getMostRecent(Date key) {
        ObReport report = null;
        synchronized (container) {
            if (key != null) {
                report = container.get(getMostRecentTime(key));
            }
        }
        return report;
    }

    /**
     * Get Report
     * 
     * @param date
     * @return
     */
    public ObReport getReport(Date date) {
        ObReport report = null;
        synchronized (container) {
            if (container.containsKey(date)) {
                report = container.get(date);
            }
        }

        return report;
    }
    

    public LinkedHashMap<Date, ObReport> getContainer() {
        return container;
    }

}
