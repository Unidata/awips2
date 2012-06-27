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
package com.raytheon.uf.common.dataplugin.satellite;

import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.ShortDataRecord;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 27, 2012            jkorman     Initial creation
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class SatelliteMessageData {

    private static final int DATA_DIMS = 2;

    private int nx;

    private int ny;

    private Object messageData;

    private Map<String, Object> dataAttributes;

    /**
     * 
     * @param messageData
     * @param numCols
     * @param numRows
     */
    public SatelliteMessageData(Object messageData, int numCols, int numRows) {
        this.messageData = messageData;
        nx = numCols;
        ny = numRows;
    }

    /**
     * 
     * @see com.raytheon.uf.common.dataplugin.satellite.SatelliteMessageData#setMessageData(java.lang.Object)
     */
    public void setMessageData(Object data) {
        messageData = data;
    }

    /**
     * 
     */
    public void setDataAttribute(String key, Object value) {
        if (dataAttributes == null) {
            dataAttributes = new HashMap<String, Object>();
        }
        dataAttributes.put(key, value);
    }

    /**
     * 
     * @see com.raytheon.uf.common.dataplugin.satellite.SatelliteMessageData#setDimensions(int,
     *      int)
     */
    public void setDimensions(int nx, int ny) {
        this.nx = nx;
        this.ny = ny;
    }

    /**
     * 
     * @see com.raytheon.uf.common.dataplugin.satellite.SatelliteMessageData#getStorageRecord()
     */
    public IDataRecord getStorageRecord(SatelliteRecord dataRec, String dataSetName) {
        IDataRecord storageRecord = null;
        if ((messageData != null) && (dataRec != null)) {
            long[] sizes = new long[] { nx, ny };
            if (messageData instanceof byte[]) {
                storageRecord = new ByteDataRecord(dataSetName,
                        dataRec.getDataURI(), (byte[]) messageData, DATA_DIMS,
                        sizes);
            } else if (messageData instanceof short[]) {
                storageRecord = new ShortDataRecord(dataSetName,
                        dataRec.getDataURI(), (short[]) messageData, DATA_DIMS,
                        sizes);
            }
        }
        if ((storageRecord != null) && (dataAttributes != null)) {
            storageRecord.setDataAttributes(dataAttributes);
        }
        return storageRecord;
    }

}
