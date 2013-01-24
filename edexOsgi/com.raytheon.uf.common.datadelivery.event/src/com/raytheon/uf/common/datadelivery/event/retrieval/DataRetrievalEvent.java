package com.raytheon.uf.common.datadelivery.event.retrieval;

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

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/* 
 * DataRetrievalEvent
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY  
 * 
 * Date         Ticket#    Engineer    Description  
 * ------------ ---------- ----------- -------------------------- 
 * Aug 21, 2012            jsanchez     Made object serializable.
 *
 * </pre>       
 * 
 * @author jsanchez
 * @version 1.0 
 */
@DynamicSerialize
public class DataRetrievalEvent extends RetrievalEvent {
    private static final long serialVersionUID = 4554679419195442322L;

    /** Bytes per Kilobyte */
    private static final double BYTES_PER_KILOBYTE = 1024.0;

    /** Map of units -> conversion factors */
    private static final Map<String, Double> conversionMap;

    /**
     * Data size unit ordering. Used to determine which unit to use if one is
     * not provided.
     */
    private static final List<String> DATA_SIZE_UNIT_ORDER;

    static {
        Map<String, Double> m = new HashMap<String, Double>();
        m.put("KB", BYTES_PER_KILOBYTE);
        m.put("MB", BYTES_PER_KILOBYTE * BYTES_PER_KILOBYTE);
        m.put("GB", BYTES_PER_KILOBYTE * BYTES_PER_KILOBYTE
                * BYTES_PER_KILOBYTE);
        conversionMap = Collections.unmodifiableMap(m);

        List<String> l = new ArrayList<String>();
        l.add("KB");
        l.add("MB");
        l.add("GB");
        DATA_SIZE_UNIT_ORDER = Collections.unmodifiableList(l);
    }

    private static final Map<String, String> FIELD_UNIT_MAP;
    static {
        Map<String, String> m = new HashMap<String, String>();
        m.put("bytes", "bytes");
        m.put("numRecords", "count");
        FIELD_UNIT_MAP = Collections.unmodifiableMap(m);
    }

    /**
     * Get the conversion factor for the unit based on the max value of the
     * data.
     * 
     * @param maxValue
     *            The largest value in the data set
     * 
     * @return the conversion factor, or 1 if no conversion
     */
    public static double getDataSizeConversion(double maxValue) {
        double conversion = 1;
        for (int i = DATA_SIZE_UNIT_ORDER.size() - 1; i >= 0; i--) {
            String unit = DATA_SIZE_UNIT_ORDER.get(i);
            if (maxValue > conversionMap.get(unit)) {
                return 1 / conversionMap.get(unit);
            }
        }

        return conversion;
    }

    @DynamicSerializeElement
    protected int numRecords;

    @DynamicSerializeElement
    protected long bytes;

    public long getBytes() {
        return bytes;
    }

    @Override
    protected Map<String, String> getFieldUnitMap() {
        return FIELD_UNIT_MAP;
    }

    public int getNumRecords() {
        return numRecords;
    }

    // For dev
    public String getUnits() {
        return "MB";
    }

    public void setBytes(long bytes) {
        this.bytes = bytes;
    }

    public void setNumRecords(int numRecords) {
        this.numRecords = numRecords;
    }

    @Override
    public String toString() {
        return "DataRetrievalEvent " + super.toString() + " provider: "
                + provider + " numRecord: " + numRecords + " bytes: " + bytes;
    }
}