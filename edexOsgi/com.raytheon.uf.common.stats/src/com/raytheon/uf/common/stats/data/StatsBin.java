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
package com.raytheon.uf.common.stats.data;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.stats.AggregateRecord;

/**
 * A bin of Statistical data.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 11, 2012   723      mpduff      Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0
 */
@DynamicSerialize
public class StatsBin {
    /** Millisecond value for this bin */
    @DynamicSerializeElement
    private long binMillis;

    /** List of AggregateRecords */
    @DynamicSerializeElement
    private final List<AggregateRecord> data = new ArrayList<AggregateRecord>();

    /** Constructor */
    public StatsBin() {

    }

    /**
     * @return the binMillis
     */
    public long getBinMillis() {
        return binMillis;
    }

    /**
     * @param binMillis
     *            the binMillis to set
     */
    public void setBinMillis(long binMillis) {
        this.binMillis = binMillis;
    }

    /**
     * Add an AggregateRecord object.
     *
     * @param record
     */
    public void setData(AggregateRecord record) {
        this.data.add(record);
    }

    /**
     * Get the AggregateRecord objects
     *
     * @return
     */
    public List<AggregateRecord> getData() {
        return data;
    }
}
