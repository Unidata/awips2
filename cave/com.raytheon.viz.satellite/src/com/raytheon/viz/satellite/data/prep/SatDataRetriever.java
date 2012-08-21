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
package com.raytheon.viz.satellite.data.prep;

import java.awt.Rectangle;
import java.nio.ByteBuffer;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.datastructure.VizDataCubeException;

/**
 * Data retrieval callback for satellite data, uses the DataCubeContainer
 * instead of the IDataStore and wraps the ByteBuffer in an UnsignedByteBuffer
 * object so the preparer knows the bytes are all unsigned
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 28, 2009            mschenke     Initial creation
 * - AWIPS2 Baseline Repository --------
 * Jul 18, 2012        798 jkorman      Modified constructor to remove hard-coded dataset name.
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class SatDataRetriever implements IColorMapDataRetrievalCallback {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SatDataRetriever.class);

    protected Rectangle datasetBounds;

    protected PluginDataObject pdo;

    protected String dataset;

    protected boolean signed = false;

    protected ByteBuffer retreivedBuffer;

    public SatDataRetriever(PluginDataObject pdo, int level,
            Rectangle dataSetBounds, boolean signed, ByteBuffer retreivedBuffer) {
        this.pdo = pdo;
        this.datasetBounds = dataSetBounds;
        
        dataset = DataStoreFactory.createDataSetName(null, DataStoreFactory.DEF_DATASET_NAME, level);

        this.signed = signed;
        this.retreivedBuffer = retreivedBuffer;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.data.IDataRetrievalCallback#getData()
     */
    @Override
    public ColorMapData getColorMapData() {
        ByteBuffer satBuffer = null;
        if (retreivedBuffer != null) {
            // Buffer was passed in on construction, use it and discard
            satBuffer = retreivedBuffer;
            retreivedBuffer = null;
        } else {
            try {
                satBuffer = ByteBuffer.wrap(getRawData());
            } catch (Exception e) {
                statusHandler.handle(Priority.SIGNIFICANT,
                        "Error retrieving satellite data", e);
            }
        }
        ColorMapDataType dataType = ColorMapDataType.BYTE;
        if (signed) {
            dataType = ColorMapDataType.SIGNED_BYTE;
        }
        return new ColorMapData(satBuffer, new int[] { datasetBounds.width,
                datasetBounds.height }, dataType);
    }

    public byte[] getRawData() {
        byte [] retData = null;
        
        Request req = Request.buildSlab(new int[] { this.datasetBounds.x,
                this.datasetBounds.y }, new int[] {
                this.datasetBounds.x + this.datasetBounds.width,
                this.datasetBounds.y + this.datasetBounds.height });
        IDataRecord[] dataRecord = null;
        try {
            dataRecord = DataCubeContainer
                    .getDataRecord(pdo, req, this.dataset);
            if (dataRecord != null && dataRecord.length == 1) {
                retData = ((ByteDataRecord) dataRecord[0]).getByteData();
            }
        } catch (VizDataCubeException e) {
            statusHandler.handle(Priority.SIGNIFICANT,
                    "Error retrieving satellite data", e);
        }
        return retData;
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
        result = prime * result + ((dataset == null) ? 0 : dataset.hashCode());
        result = prime * result
                + ((datasetBounds == null) ? 0 : datasetBounds.hashCode());
        result = prime * result + ((pdo == null) ? 0 : pdo.hashCode());
        result = prime * result + (signed ? 1231 : 1237);
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
        SatDataRetriever other = (SatDataRetriever) obj;
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
        if (pdo == null) {
            if (other.pdo != null)
                return false;
        } else if (!pdo.equals(other.pdo))
            return false;
        if (signed != other.signed)
            return false;
        return true;
    }

}
