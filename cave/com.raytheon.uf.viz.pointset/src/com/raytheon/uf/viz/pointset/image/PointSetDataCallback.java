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
import java.nio.ByteBuffer;
import java.nio.DoubleBuffer;
import java.nio.FloatBuffer;
import java.nio.IntBuffer;
import java.nio.ShortBuffer;
import java.util.Map;

import javax.measure.unit.Unit;

import com.raytheon.uf.common.colormap.image.ColorMapData;
import com.raytheon.uf.common.colormap.image.ColorMapData.ColorMapDataType;
import com.raytheon.uf.common.dataplugin.pointset.PointSetConstants;
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
 * ------------- -------- --------- -------------------------------------
 * Aug 28, 2015  4709     bsteffen  Initial creation
 * Jan 25, 2016  5208     bsteffen  Support scale, offset and int types.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class PointSetDataCallback implements
        IColorMapDataRetrievalCallback {

    private final PointSetRecord record;

    private Unit<?> dataUnit;

    public PointSetDataCallback(PointSetRecord record) {
        this.record = record;
    }

    public Unit<?> getDataUnit() {
        return dataUnit;
    }

    @Override
    public ColorMapData getColorMapData() throws VizException {
        IDataStore store = DataStoreFactory.getDataStore(record
                .getStoragePath().toFile());
        try {
            IDataRecord record = store.retrieve(this.record.getDataURI(),
                    DataStoreFactory.DEF_DATASET_NAME, Request.ALL);
            dataUnit = this.record.getParameter().getUnit();
            Map<String, Object> attrs = record.getDataAttributes();
            if (attrs != null) {
                Number offset = (Number) attrs
                        .get(PointSetConstants.ADD_OFFSET);
                Number scale = (Number) attrs
                        .get(PointSetConstants.SCALE_FACTOR);

                if (offset != null) {
                    double offsetVal = offset.doubleValue();
                    if (offsetVal != 0.0) {
                        dataUnit = dataUnit.plus(offsetVal);
                    }
                }
                if (scale != null) {
                    double scaleVal = scale.doubleValue();
                    if (scaleVal != 0.0) {
                        dataUnit = dataUnit.times(scaleVal);
                    }
                }
            }
            Object data = record.getDataObject();
            if (data instanceof float[]) {
                float[] fdata = (float[]) data;
                return new ColorMapData(FloatBuffer.wrap(fdata),
                        new int[] { fdata.length }, ColorMapDataType.FLOAT,
                        dataUnit);
            } else if (data instanceof double[]) {
                double[] ddata = (double[]) data;
                return new ColorMapData(DoubleBuffer.wrap(ddata),
                        new int[] { ddata.length }, ColorMapDataType.DOUBLE,
                        dataUnit);
            } else if (data instanceof byte[]) {
                byte[] bdata = (byte[]) data;
                return new ColorMapData(ByteBuffer.wrap(bdata),
                        new int[] { bdata.length },
                        ColorMapDataType.SIGNED_BYTE, dataUnit);
            } else if (data instanceof short[]) {
                short[] sdata = (short[]) data;
                return new ColorMapData(ShortBuffer.wrap(sdata),
                        new int[] { sdata.length }, ColorMapDataType.SHORT,
                        dataUnit);
            } else if (data instanceof int[]) {
                int[] idata = (int[]) data;
                return new ColorMapData(IntBuffer.wrap(idata),
                        new int[] { idata.length }, ColorMapDataType.INT,
                        dataUnit);
            } else {
                throw new VizException("Unsupported data of type "
                        + data.getClass().getSimpleName());
            }
        } catch (StorageException | FileNotFoundException e) {
            throw new VizException(e);
        }
    }

}