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

import com.raytheon.uf.common.dataplugin.shef.tables.Locdatalimits;

/**
 * A lookup key for {@link Locdatalimits} records.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 22, 2016 5699       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class LocDataLimitKey extends DataLimitKey {

    private final String lid;

    public LocDataLimitKey(String lid, String pe, int dur) {
        super(pe, dur);
        if (lid == null) {
            throw new IllegalArgumentException(
                    "Required argument 'lid' cannot be NULL.");
        }
        this.lid = lid;
    }

    public String getLid() {
        return lid;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((lid == null) ? 0 : lid.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (!super.equals(obj)) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        LocDataLimitKey other = (LocDataLimitKey) obj;
        if (lid == null) {
            if (other.lid != null) {
                return false;
            }
        } else if (!lid.equals(other.lid)) {
            return false;
        }
        return true;
    }
}