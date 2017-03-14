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

import com.raytheon.uf.common.dataplugin.shef.tables.Datalimits;

/**
 * A lookup key for {@link Datalimits} records.
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

public class DataLimitKey {

    private final String pe;

    private final int dur;

    public DataLimitKey(String pe, int dur) {
        if (pe == null) {
            throw new IllegalArgumentException(
                    "Required argument 'pe' cannot be NULL.");
        }
        this.pe = pe;
        this.dur = dur;
    }

    public String getPe() {
        return pe;
    }

    public int getDur() {
        return dur;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + dur;
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
        DataLimitKey other = (DataLimitKey) obj;
        if (dur != other.dur) {
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
}