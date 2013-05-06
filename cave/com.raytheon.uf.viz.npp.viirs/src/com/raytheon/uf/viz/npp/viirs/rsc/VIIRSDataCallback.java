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
import java.nio.FloatBuffer;
import java.nio.ShortBuffer;

import com.raytheon.uf.common.colormap.image.ColorMapData;
import com.raytheon.uf.common.colormap.image.ColorMapData.ColorMapDataType;
import com.raytheon.uf.common.dataplugin.npp.viirs.VIIRSDataRecord;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.ShortDataRecord;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * VIIRS Colormap data callback, requests data for VIIRSDataRecord
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 30, 2011            mschenke     Initial creation
 * Apr 04, 2013            djohnson     Remove color import.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class VIIRSDataCallback implements IColorMapDataRetrievalCallback {

    private VIIRSDataRecord dataRecord;

    private int level;

    private Rectangle dataArea;

    public VIIRSDataCallback(VIIRSDataRecord dataRecord, int level,
            Rectangle dataArea) {
        this.dataRecord = dataRecord;
        this.level = level;
        this.dataArea = dataArea;
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
            IDataRecord[] records = DataCubeContainer.getDataRecord(dataRecord,
                    Request.buildSlab(new int[] { dataArea.x, dataArea.y },
                            new int[] { dataArea.x + dataArea.width,
                                    dataArea.y + dataArea.height }),
                    VIIRSDataRecord.getDataSet(level));
            IDataRecord rawData = records[0];
            if (rawData instanceof ShortDataRecord) {
                Buffer shortData = ShortBuffer.wrap(((ShortDataRecord) rawData)
                        .getShortData());
                return new ColorMapData(shortData, new int[] {
                        (int) rawData.getSizes()[0],
                        (int) rawData.getSizes()[1] },
                        ColorMapDataType.UNSIGNED_SHORT);
            } else if (rawData instanceof FloatDataRecord) {
                Buffer floatData = FloatBuffer.wrap(((FloatDataRecord) rawData)
                        .getFloatData());
                return new ColorMapData(floatData, new int[] {
                        (int) rawData.getSizes()[0],
                        (int) rawData.getSizes()[1] }, ColorMapDataType.FLOAT);
            } else {
                throw new VizException("Could not handle IDataRecord: "
                        + rawData);
            }
        } catch (Throwable t) {
            throw new VizException("Error requesting viirs slab: "
                    + t.getLocalizedMessage(), t);
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
                + ((dataArea == null) ? 0 : dataArea.hashCode());
        result = prime * result
                + ((dataRecord == null) ? 0 : dataRecord.hashCode());
        result = prime * result + level;
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
        if (dataArea == null) {
            if (other.dataArea != null)
                return false;
        } else if (!dataArea.equals(other.dataArea))
            return false;
        if (dataRecord == null) {
            if (other.dataRecord != null)
                return false;
        } else if (!dataRecord.equals(other.dataRecord))
            return false;
        if (level != other.level)
            return false;
        return true;
    }

}
