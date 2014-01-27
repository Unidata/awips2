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
package com.raytheon.uf.viz.datadelivery.system;

import java.util.Collection;
import java.util.List;
import java.util.Set;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Multimap;
import com.raytheon.uf.common.datadelivery.event.status.DataDeliverySystemStatus;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * System Status Data Object
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 18, 2013    1655    mpduff      Initial creation.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
@DynamicSerialize
public class SystemStatusData {
    /** The Multimap data structure */
    @DynamicSerializeElement
    private final Multimap<String, DataDeliverySystemStatus> systemTypesMap = ArrayListMultimap
            .create();

    /**
     * Constructor.
     */
    public SystemStatusData() {

    }

    /**
     * Constructor.
     * 
     * @param dataRecords
     *            Records to add
     */
    public SystemStatusData(List<DataDeliverySystemStatus> dataRecords) {
        addRecords(dataRecords);
    }

    /**
     * Add a list of records.
     * 
     * @param dataRecords
     *            List of DataDeliverySystemStatus records
     */
    public void addRecords(List<DataDeliverySystemStatus> dataRecords) {
        for (DataDeliverySystemStatus record : dataRecords) {
            this.addRecord(record);
        }
    }

    /**
     * Add a record.
     * 
     * @param record
     *            The record to add
     */
    public void addRecord(DataDeliverySystemStatus record) {
        systemTypesMap.put(record.getKey().getSystemType(), record);
    }

    /**
     * Get the data for the system type provided.
     * 
     * @param systemType
     *            the system type
     * @return the data
     */
    public List<DataDeliverySystemStatus> getRecords(String systemType) {
        return (List<DataDeliverySystemStatus>) systemTypesMap.get(systemType);
    }

    /**
     * Get the different System Types in the data structure
     * 
     * @return Set of system types
     */
    public Set<String> getSystemTypes() {
        return systemTypesMap.keySet();
    }

    /**
     * Get all the records.
     * 
     * @return Collection of DataDeliverySystemStatus records
     */
    public Collection<DataDeliverySystemStatus> getRecords() {
        return systemTypesMap.values();
    }
}
