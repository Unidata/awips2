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
package com.raytheon.uf.viz.core.data.prep;

import java.awt.Rectangle;
import java.io.File;
import java.nio.Buffer;
import java.nio.ByteBuffer;
import java.nio.FloatBuffer;
import java.nio.IntBuffer;
import java.nio.ShortBuffer;

import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.IntegerDataRecord;
import com.raytheon.uf.common.datastorage.records.ShortDataRecord;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;

/**
 * Retrieve data from hdf5 using the datastore
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 27, 2009            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class HDF5DataRetriever implements IColorMapDataRetrievalCallback {

    /** Hdf5 file to retreive data from */
    protected File hdf5File;

    /** Dataset in hdf5 to retrieve */
    protected String dataset;

    /** bounds of image to retrieve */
    protected Rectangle datasetBounds;

    public HDF5DataRetriever(File hdf5File, String dataset,
            Rectangle datasetBounds) {
        this.hdf5File = hdf5File;
        this.dataset = dataset;
        this.datasetBounds = datasetBounds;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.data.IDataRetrievalCallback#getData()
     */
    @Override
    public ColorMapData getColorMapData() {
        try {
            Buffer buffer = null;
            IDataStore ds = DataStoreFactory.getDataStore(hdf5File);
            Request req = Request.buildSlab(new int[] { this.datasetBounds.x,
                    this.datasetBounds.y }, new int[] {
                    this.datasetBounds.x + this.datasetBounds.width,
                    this.datasetBounds.y + this.datasetBounds.height });
            IDataRecord rec = ds.retrieve("", dataset, req);
            if (rec instanceof ByteDataRecord) {
                byte[] data = ((ByteDataRecord) rec).getByteData();
                buffer = ByteBuffer.wrap(data);
            } else if (rec instanceof FloatDataRecord) {
                float[] data = ((FloatDataRecord) rec).getFloatData();
                buffer = FloatBuffer.wrap(data);
            } else if (rec instanceof IntegerDataRecord) {
                int[] data = ((IntegerDataRecord) rec).getIntData();
                buffer = IntBuffer.wrap(data);
            } else if (rec instanceof ShortDataRecord) {
                short[] data = ((ShortDataRecord) rec).getShortData();
                buffer = ShortBuffer.wrap(data);
            }
            return new ColorMapData(buffer, new int[] { datasetBounds.width,
                    datasetBounds.height });
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((dataset == null) ? 0 : dataset.hashCode());
        result = prime * result
                + ((datasetBounds == null) ? 0 : datasetBounds.hashCode());
        result = prime * result
                + ((hdf5File == null) ? 0 : hdf5File.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        HDF5DataRetriever other = (HDF5DataRetriever) obj;
        if (dataset == null) {
            if (other.dataset != null)
                return false;
        } else if (!dataset.equals(other.dataset))
            return false;
        if (datasetBounds == null) {
            if (other.datasetBounds != null)
                return false;
        } else if (!datasetBounds.equals(other.datasetBounds))
            return false;
        if (hdf5File == null) {
            if (other.hdf5File != null)
                return false;
        } else if (!hdf5File.equals(other.hdf5File))
            return false;
        return true;
    }

}
