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
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Encapsulate satellite image data as well as the dimensions of
 * the image grid. Attributes about the data may also be added. As an
 * example these attributes could include "scale factor" and/or "fill_value".
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 27, 2012        798     jkorman Initial creation
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
@DynamicSerialize
public class SatelliteMessageData {

    private static final int DATA_DIMS = 2;

    // Number of columns in the image data
    @DynamicSerializeElement
    private int nx;

    // Number of rows in the image data
    @DynamicSerializeElement
    private int ny;

    // The image grid data - Usually some type (T [])
    @DynamicSerializeElement
    private Object messageData;

    @DynamicSerializeElement
    private Map<String, Object> dataAttributes;

    /**
     * Create a message object containing the gridded image data as well as
     * its dimensions.
     * @param messageData The image grid data - Usually some type (T [])
     * @param numCols Number of columns in the image grid.
     * @param numRows Number of rows in the image grid.
     */
    public SatelliteMessageData(Object messageData, int numCols, int numRows) {
        this.messageData = messageData;
        nx = numCols;
        ny = numRows;
    }

    /**
     * Set the gridded image data.
     * @param data The image grid data - Usually some type (T [])
     * @see com.raytheon.uf.common.dataplugin.satellite.SatelliteMessageData#setMessageData(java.lang.Object)
     */
    public void setMessageData(Object data) {
        messageData = data;
    }

    /**
     * Set an attribute associated with the image data.
     * @param key Name to store the information against.
     * @param value The value to store against the given key.
     */
    public void setDataAttribute(String key, Object value) {
        if (dataAttributes == null) {
            dataAttributes = new HashMap<String, Object>();
        }
        dataAttributes.put(key, value);
    }

    /**
     * Set the dimensions of the data.
     * @param nx Number of columns in the image grid.
     * @param ny Number of rows in the image grid.
     * @see com.raytheon.uf.common.dataplugin.satellite.SatelliteMessageData#setDimensions(int,
     *      int)
     */
    public void setDimensions(int nx, int ny) {
        this.nx = nx;
        this.ny = ny;
    }

    /**
     * Create a data record that encapsulates the data in this class.
     * @param dataRec A satellite record that will supply the information needed to
     * populate the data record being built.
     * @param dataSetName The name that will be used to identify the data set.
     * @return The created data record.
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

    /**
     * Get the number of columns in the image grid.
     * @return The number of columns in the image grid.
     */
    public int getNx() {
        return nx;
    }

    /**
     * Set the number of columns in the image grid.
     * @param nx Number of columns in the image grid.
     */
    public void setNx(int nx) {
        this.nx = nx;
    }

    /**
     * Get the number of rows in the image grid.
     * @return The number of rows in the image grid.
     */
    public int getNy() {
        return ny;
    }

    /**
     * Set the number of rows in the image grid.
     * @param ny Number of rows in the image grid.
     */
    public void setNy(int ny) {
        this.ny = ny;
    }

    /**
     * Get the data attributes.
     * @return The data attributes.
     */
    public Map<String, Object> getDataAttributes() {
        return dataAttributes;
    }

    /**
     * Set the data attributes.
     * @param dataAttributes The data attributes.
     */
    public void setDataAttributes(Map<String, Object> dataAttributes) {
        this.dataAttributes = dataAttributes;
    }

    /**
     * Get the underlying message data object.
     * @return The underlying message data object.
     */
    public Object getMessageData() {
        return messageData;
    }
}
