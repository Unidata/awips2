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

import java.util.Calendar;

import com.raytheon.uf.edex.plugin.mpe.dqcpreprocessor.output.DataDateKey;
import com.raytheon.uf.edex.plugin.mpe.dqcpreprocessor.output.SynopticHour;

/**
 * Basic POJO lookup key identifying which synoptic day a given synoptic hour is
 * associated with. Synoptic days can span any of the following time periods:
 * 0z-0z, 6z-6z, 12z-12z, 18z-18z.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 15, 2018 7184       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public class SynopticIdentifier {

    private final SynopticHour synopticHour;

    private final Calendar synopticCalendar;

    private final DataDateKey dataDateKey;

    public SynopticIdentifier(final SynopticHour synopticHour,
            final Calendar synopticCalendar, final DataDateKey dataDateKey) {
        this.synopticHour = synopticHour;
        this.synopticCalendar = synopticCalendar;
        this.dataDateKey = dataDateKey;
    }

    /**
     * @return the synopticHour
     */
    public SynopticHour getSynopticHour() {
        return synopticHour;
    }

    /**
     * @return the synopticCalendar
     */
    public Calendar getSynopticCalendar() {
        return synopticCalendar;
    }

    /**
     * @return the dataDateKey
     */
    public DataDateKey getDataDateKey() {
        return dataDateKey;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("SynopticIdentifier [");
        sb.append("synopticHour=").append(synopticHour.name());
        sb.append(", dataDateKey=").append(dataDateKey.toString());
        sb.append("]");
        return sb.toString();
    }
}