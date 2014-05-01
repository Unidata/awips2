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
package com.raytheon.uf.common.datadelivery.event.status;

import java.util.List;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * The System Status Response object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 18, 2013   1655     mpduff     Initial creation.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
@DynamicSerialize
public class SystemStatusResponse implements ISerializableObject {
    @DynamicSerializeElement
    private List<DataDeliverySystemStatus> data;

    /**
     * Constructor.
     * 
     * @param data
     *            The system status data
     */
    public SystemStatusResponse(List<DataDeliverySystemStatus> data) {
        this.data = data;
    }

    /**
     * Constructor.
     */
    public SystemStatusResponse() {

    }

    /**
     * @return the data
     */
    public List<DataDeliverySystemStatus> getData() {
        return data;
    }

    /**
     * @param data
     *            the data to set
     */
    public void setData(List<DataDeliverySystemStatus> data) {
        this.data = data;
    }

    /**
     * Add a record.
     * 
     * @param data
     *            the record to add.
     */
    public void addData(DataDeliverySystemStatus data) {
        this.data.add(data);
    }
}
