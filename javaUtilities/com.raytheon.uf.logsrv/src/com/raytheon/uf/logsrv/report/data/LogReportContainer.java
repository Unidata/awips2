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
package com.raytheon.uf.logsrv.report.data;

import java.sql.Timestamp;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 * A container that holds a concept of a report based on the frequency of
 * occurrences of logging events (ie errors) in the database.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 28, 2013            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class LogReportContainer {

    private Map<String, LogReportEvent> map = new HashMap<String, LogReportEvent>();

    private Timestamp earliestTime;

    private Timestamp latestTime;

    public void addError(LogReportEvent event, String machineName,
            int occurrences) {
        String key = event.getKey();
        if (!map.containsKey(key)) {
            map.put(key, event);
        }

        LogReportEvent current = map.get(key);
        Map<String, Integer> machineCount = current.getMachineCount();
        Integer count = machineCount.get(machineName);
        if (count == null) {
            count = 0;
        }
        count += occurrences;
        machineCount.put(machineName, count);
    }

    public Collection<LogReportEvent> getEvents() {
        return map.values();
    }

    public Timestamp getEarliestTime() {
        return earliestTime;
    }

    public void setEarliestTime(Timestamp earliestTime) {
        this.earliestTime = earliestTime;
    }

    public Timestamp getLatestTime() {
        return latestTime;
    }

    public void setLatestTime(Timestamp latestTime) {
        this.latestTime = latestTime;
    }

}
