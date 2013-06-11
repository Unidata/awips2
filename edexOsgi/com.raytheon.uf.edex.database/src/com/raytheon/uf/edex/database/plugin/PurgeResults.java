// header placeholder 2a20 c8e1
package com.raytheon.uf.edex.database.plugin;

import java.util.Date;
import java.util.Map;
import java.util.Set;

/**
 * Summary of purge
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 12, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class PurgeResults {

    private Map<String, Set<Date>> timesKept;

    private Map<String, Set<Date>> timesPurged;

    public PurgeResults() {
    }

    public PurgeResults(Map<String, Set<Date>> timesKept,
            Map<String, Set<Date>> timesPurged) {
        this.timesKept = timesKept;
        this.timesPurged = timesPurged;
    }

    /**
     * @return true if any times were purged for any rule
     */
    public boolean didPurge() {
        if (timesPurged == null || timesPurged.isEmpty()) {
            return false;
        }
        for (String key : timesPurged.keySet()) {
            Set<Date> set = timesPurged.get(key);
            if (!set.isEmpty()) {
                return true;
            }
        }
        return false;
    }

    /**
     * @return the timesKept
     */
    public Map<String, Set<Date>> getTimesKept() {
        return timesKept;
    }

    /**
     * @param timesKept
     *            the timesKept to set
     */
    public void setTimesKept(Map<String, Set<Date>> timesKept) {
        this.timesKept = timesKept;
    }

    /**
     * @return the timesPurged
     */
    public Map<String, Set<Date>> getTimesPurged() {
        return timesPurged;
    }

    /**
     * @param timesPurged
     *            the timesPurged to set
     */
    public void setTimesPurged(Map<String, Set<Date>> timesPurged) {
        this.timesPurged = timesPurged;
    }

}
