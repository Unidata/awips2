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
package com.raytheon.uf.edex.plugin.mpe.rocchecker;

/**
 * A lookup key for {@link Observation} records. Used to group records by lid,
 * pe, and obstime.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 23, 2016 5699       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class ObservationKey {

    private final String lid;

    private final String pe;

    public ObservationKey(final String lid, final String pe) {
        if (lid == null) {
            throw new IllegalArgumentException(
                    "Required argument 'lid' cannot be NULL.");
        }
        if (pe == null) {
            throw new IllegalArgumentException(
                    "Required argument 'pe' cannot be NULL.");
        }
        this.lid = lid;
        this.pe = pe;
    }

    public String getLid() {
        return lid;
    }

    public String getPe() {
        return pe;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((lid == null) ? 0 : lid.hashCode());
        result = prime * result + ((pe == null) ? 0 : pe.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        ObservationKey other = (ObservationKey) obj;
        if (lid == null) {
            if (other.lid != null) {
                return false;
            }
        } else if (!lid.equals(other.lid)) {
            return false;
        }
        if (pe == null) {
            if (other.pe != null) {
                return false;
            }
        } else if (!pe.equals(other.pe)) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("ObservationKey [");
        sb.append("lid=").append(lid);
        sb.append(", pe=").append(pe);
        sb.append("]");
        return sb.toString();
    }
}