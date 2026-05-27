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
package com.raytheon.uf.edex.plugin.mpe.dqcpreprocessor.output;

/**
 * Lookup key used to keep track of and separate the information that will be
 * output across potentially multiple days.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 31, 2018 7184       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public class DataDateKey {

    private final String currentDay;

    private final String nextDay;

    public DataDateKey(final String currentDay) {
        this(currentDay, null);
    }

    public DataDateKey(final String currentDay, final String nextDay) {
        this.currentDay = currentDay;
        this.nextDay = nextDay;
    }

    /**
     * @return the currentDay
     */
    public String getCurrentDay() {
        return currentDay;
    }

    /**
     * @return the nextDay
     */
    public String getNextDay() {
        return nextDay;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((currentDay == null) ? 0 : currentDay.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        DataDateKey other = (DataDateKey) obj;
        if (currentDay == null) {
            if (other.currentDay != null)
                return false;
        } else if (!currentDay.equals(other.currentDay))
            return false;
        return true;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("DataDateKey [");
        sb.append("currentDay=").append(currentDay);
        sb.append(", nextDay=").append(nextDay);
        sb.append("]");
        return sb.toString();
    }
}