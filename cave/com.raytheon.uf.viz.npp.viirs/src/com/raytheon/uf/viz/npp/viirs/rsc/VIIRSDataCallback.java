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
package com.raytheon.uf.viz.npp.viirs.rsc;

import java.awt.Rectangle;
import java.nio.Buffer;
import java.nio.ShortBuffer;

import com.raytheon.uf.common.dataplugin.npp.viirs.VIIRSDataRecord;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.ShortDataRecord;
import com.raytheon.uf.viz.core.HDF5Util;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 30, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class VIIRSDataCallback implements IColorMapDataRetrievalCallback {

    private VIIRSDataRecord dataRecord;

    private Rectangle validDataBounds;

    public VIIRSDataCallback(VIIRSDataRecord dataRecord,
            Rectangle validDataBounds) {
        this.dataRecord = dataRecord;
        this.validDataBounds = validDataBounds;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback#getColorMapData
     * ()
     */
    @Override
    public ColorMapData getColorMapData() throws VizException {
        try {
            int[] sizes = new int[] { validDataBounds.width,
                    validDataBounds.height };
            IDataStore dataStore = DataStoreFactory.getDataStore(HDF5Util
                    .findHDF5Location(dataRecord));
            IDataRecord rawData = dataStore.retrieve(dataRecord.getDataURI(),
                    VIIRSDataRecord.getDataSet(0),
                    Request.buildSlab(new int[] { 0, 0 }, sizes));

            Buffer shortData = ShortBuffer.wrap(((ShortDataRecord) rawData)
                    .getShortData());
            return new ColorMapData(shortData, sizes,
                    ColorMapDataType.UNSIGNED_SHORT);
        } catch (Throwable t) {
            throw new VizException(t);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((dataRecord == null) ? 0 : dataRecord.hashCode());
        result = prime * result
                + ((validDataBounds == null) ? 0 : validDataBounds.hashCode());
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        VIIRSDataCallback other = (VIIRSDataCallback) obj;
        if (dataRecord == null) {
            if (other.dataRecord != null)
                return false;
        } else if (!dataRecord.equals(other.dataRecord))
            return false;
        if (validDataBounds == null) {
            if (other.validDataBounds != null)
                return false;
        } else if (!validDataBounds.equals(other.validDataBounds))
            return false;
        return true;
    }

}
