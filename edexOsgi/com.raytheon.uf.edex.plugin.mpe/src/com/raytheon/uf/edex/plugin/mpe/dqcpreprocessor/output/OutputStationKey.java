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
 * Lookup key used to keep track of the output values that will be written to
 * the Level 1 Point files.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 31, 2018 7184       bkowal      Initial creation
 * Mar 14, 2018 7184       bkowal      Update {@link #compareTo(OutputStationKey)} to consider
 *                                     both the lid and source.
 *
 * </pre>
 *
 * @author bkowal
 */

public class OutputStationKey implements Comparable<OutputStationKey> {

    private final String lid;

    private final String source;

    public OutputStationKey(final String lid, final String source) {
        this.lid = lid;
        this.source = source;
    }

    /**
     * @return the lid
     */
    public String getLid() {
        return lid;
    }

    /**
     * @return the source
     */
    public String getSource() {
        return source;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((lid == null) ? 0 : lid.hashCode());
        result = prime * result + ((source == null) ? 0 : source.hashCode());
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
        OutputStationKey other = (OutputStationKey) obj;
        if (lid == null) {
            if (other.lid != null)
                return false;
        } else if (!lid.equals(other.lid))
            return false;
        if (source == null) {
            if (other.source != null)
                return false;
        } else if (!source.equals(other.source))
            return false;
        return true;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("OutputStationKey [");
        sb.append("lid=").append(lid);
        sb.append(", source=").append(source);
        sb.append("]");
        return sb.toString();
    }

    @Override
    public int compareTo(OutputStationKey o) {
        if (this.lid.equals(o.lid) && this.source.equals(o.source)) {
            return 0;
        } else if (this.lid.equals(o.lid)) {
            return this.source.compareTo(o.source);
        }
        return this.lid.compareTo(o.lid);
    }
}