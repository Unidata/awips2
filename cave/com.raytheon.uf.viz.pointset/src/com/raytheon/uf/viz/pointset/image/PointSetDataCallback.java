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
package com.raytheon.uf.viz.pointset.image;

import java.io.FileNotFoundException;
import java.nio.DoubleBuffer;
import java.nio.FloatBuffer;

import com.raytheon.uf.common.colormap.image.ColorMapData;
import com.raytheon.uf.common.colormap.image.ColorMapData.ColorMapDataType;
import com.raytheon.uf.common.dataplugin.pointset.PointSetRecord;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * {@link IColorMapDataRetrievalCallback} implementation for loading point set
 * data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Aug 28, 2015  4709     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class PointSetDataCallback implements
        IColorMapDataRetrievalCallback {

    private final PointSetRecord record;

    public PointSetDataCallback(PointSetRecord record) {
        this.record = record;
    }

    @Override
    public ColorMapData getColorMapData() throws VizException {
        IDataStore store = DataStoreFactory.getDataStore(record
                .getStoragePath().toFile());
        try {
            IDataRecord record = store.retrieve(this.record.getDataURI(),
                    DataStoreFactory.DEF_DATASET_NAME, Request.ALL);
            Object data = record.getDataObject();
            if (data instanceof float[]) {
                float[] fdata = (float[]) data;
                return new ColorMapData(FloatBuffer.wrap(fdata),
                        new int[] { fdata.length }, ColorMapDataType.FLOAT,
                        this.record.getParameter().getUnit());
            } else if (data instanceof double[]) {
                double[] fdata = (double[]) data;
                return new ColorMapData(DoubleBuffer.wrap(fdata),
                        new int[] { fdata.length }, ColorMapDataType.DOUBLE,
                        this.record.getParameter().getUnit());
            } else {
                throw new VizException("Unsupported data of type "
                        + data.getClass().getSimpleName());
            }
        } catch (StorageException | FileNotFoundException e) {
            throw new VizException(e);
        }
    }

}