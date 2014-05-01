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

import java.util.HashMap;
import java.util.Map;

import ch.qos.logback.classic.Level;

/**
 * An event representing a unique error that was originally reported as a
 * logging event, with a count of occurrences per machine.
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

public class LogReportEvent implements Comparable<LogReportEvent> {

    private int lineNumber;

    private String javaClass;

    private String sampleStacktrace;

    private Map<String, Integer> machineCount = new HashMap<String, Integer>();

    private String sampleMessage;

    private String level;

    private String threadName;

    public int getLineNumber() {
        return lineNumber;
    }

    public void setLineNumber(int lineNumber) {
        this.lineNumber = lineNumber;
    }

    public String getJavaClass() {
        return javaClass;
    }

    public void setJavaClass(String javaClass) {
        this.javaClass = javaClass;
    }

    public String getSampleStacktrace() {
        return sampleStacktrace;
    }

    public void setSampleStacktrace(String sampleStacktrace) {
        this.sampleStacktrace = sampleStacktrace;
    }

    public Map<String, Integer> getMachineCount() {
        return machineCount;
    }

    public void setMachineCount(Map<String, Integer> machineCount) {
        this.machineCount = machineCount;
    }

    public String getSampleMessage() {
        return sampleMessage;
    }

    public void setSampleMessage(String sampleMessage) {
        this.sampleMessage = sampleMessage;
    }

    public String getLevel() {
        return level;
    }

    public void setLevel(String level) {
        this.level = level;
    }

    public boolean receivedStack() {
        return lineNumber > 0;
    }

    public String getKey() {
        String key = null;
        if (receivedStack()) {
            key = javaClass + ":" + lineNumber;
        } else {
            key = sampleMessage;
            if (key == null) {
                key = "null";
            } else if (key.length() > 20) {
                // cut off the key to try and match it to others
                key = key.substring(0, 20);
            }

        }
        return key;
    }

    public int getTotalOccurences() {
        int count = 0;
        for (Integer i : machineCount.values()) {
            count += i;
        }
        return count;
    }

    /**
     * Compares the LogReportEvents to enable sorting. Sort order goes as
     * follows: warn, no stacktraces, and least number of occurrences error, no
     * stacktraces, and least number of occurrences stacktrace, warn, and least
     * number of occurrences stacktrace, error, and least number of occurrences
     * 
     * For example, the highest/last in the sort would be the ERROR with a
     * stacktrace that occurred the most.
     */
    @Override
    public int compareTo(LogReportEvent o) {
        int retVal = 0;
        if (!this.receivedStack() && o.receivedStack()) {
            retVal = -1;
        } else if (this.receivedStack() && !o.receivedStack()) {
            retVal = 1;
        } else {
            // both match on having a stacktrace or not, rank them
            // based on error or warning
            Level thisLevel = Level.valueOf(level);
            Level oLevel = Level.valueOf(o.getLevel());
            if (thisLevel.toInt() < oLevel.toInt()) {
                retVal = -1;
            } else if (thisLevel.toInt() > oLevel.toInt()) {
                retVal = 1;
            } else {
                // both match on having a stacktrace or not, and match
                // on being WARNs or ERRORs, so rank them on number
                // of times they occurred
                int count1 = this.getTotalOccurences();
                int count2 = o.getTotalOccurences();
                retVal = (count1 < count2) ? -1 : ((count1 == count2) ? 0 : 1);
            }
        }

        return retVal;
    }

    public String getThreadName() {
        return threadName;
    }

    public void setThreadName(String threadName) {
        this.threadName = threadName;
    }

}
