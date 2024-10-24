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
package com.raytheon.uf.edex.plugin.mpe.dqcpreprocessor;

/**
 * Lookup key used to segregate DailyPP and DailyPC data by lid and ts.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 6, 2018  7184       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public class LidTSKey {

    private final String lid;

    private final String ts;

    public LidTSKey(final String lid, final String ts) {
        this.lid = lid;
        this.ts = ts;
    }

    /**
     * @return the lid
     */
    public String getLid() {
        return lid;
    }

    /**
     * @return the ts
     */
    public String getTs() {
        return ts;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((lid == null) ? 0 : lid.hashCode());
        result = prime * result + ((ts == null) ? 0 : ts.hashCode());
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
        LidTSKey other = (LidTSKey) obj;
        if (lid == null) {
            if (other.lid != null)
                return false;
        } else if (!lid.equals(other.lid))
            return false;
        if (ts == null) {
            if (other.ts != null)
                return false;
        } else if (!ts.equals(other.ts))
            return false;
        return true;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("LidTSKey [");
        sb.append("lid=").append(lid);
        sb.append(", ts=").append(ts);
        sb.append("]");
        return sb.toString();
    }
}