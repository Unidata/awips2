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
package com.raytheon.viz.satellite.tileset;

import java.awt.Rectangle;
import java.nio.Buffer;
import java.nio.ByteBuffer;
import java.nio.FloatBuffer;
import java.nio.ShortBuffer;
import java.text.ParseException;
import java.text.ParsePosition;
import java.util.Map;

import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;

import com.raytheon.uf.common.colormap.image.ColorMapData;
import com.raytheon.uf.common.colormap.image.ColorMapData.ColorMapDataType;
import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.dataplugin.satellite.units.generic.GenericPixel;
import com.raytheon.uf.common.dataplugin.satellite.units.goes.PolarPrecipWaterPixel;
import com.raytheon.uf.common.dataplugin.satellite.units.water.BlendedTPWPixel;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.ShortDataRecord;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.datacube.DataCubeContainer;
import com.raytheon.viz.satellite.SatelliteConstants;

/**
 * {@link IColorMapDataRetrievalCallback} for satellite imagery data. Supports
 * signed and unsigned byte and short data
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jun 20, 2013  2122     mschenke    Initial creation
 * Nov 13, 2013  2492     mschenke    Added extraction of scale/offset from
 *                                    data record attributes for unit
 * Apr 09, 2014  2947     bsteffen    Improve flexibility of sat derived
 *                                    parameters.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
public class SatDataRetriever implements IColorMapDataRetrievalCallback {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SatDataRetriever.class);

    protected Rectangle datasetBounds;

    protected SatelliteRecord record;

    protected String dataset;

    public SatDataRetriever(SatelliteRecord record, int level,
            Rectangle dataSetBounds) {
        this.record = record;
        this.datasetBounds = dataSetBounds;
        dataset = DataStoreFactory.createDataSetName(null,
                SatelliteRecord.SAT_DATASET_NAME, level);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.data.IDataRetrievalCallback#getData()
     */
    @Override
    public ColorMapData getColorMapData() {
        Buffer data = null;
        Unit<?> dataUnit = Unit.ONE;
        boolean signed = false;
        Request req = Request.buildSlab(new int[] { this.datasetBounds.x,
                this.datasetBounds.y }, new int[] {
                this.datasetBounds.x + this.datasetBounds.width,
                this.datasetBounds.y + this.datasetBounds.height });
        IDataRecord[] dataRecord = null;
        try {
            dataRecord = DataCubeContainer.getDataRecord(record, req,
                    this.dataset);
            if (dataRecord != null && dataRecord.length == 1) {
                IDataRecord record = dataRecord[0];
                if (record instanceof ByteDataRecord) {
                    data = ByteBuffer.wrap((byte[]) record.getDataObject());
                } else if (record instanceof ShortDataRecord) {
                    data = ShortBuffer.wrap((short[]) record.getDataObject());
                } else if (record instanceof FloatDataRecord) {
                    data = FloatBuffer.wrap((float[]) record.getDataObject());
                }
                Unit<?> recordUnit = getRecordUnit(this.record);
                signed = recordUnit instanceof GenericPixel;
                dataUnit = getDataUnit(recordUnit, record);
            }
        } catch (DataCubeException e) {
            statusHandler.handle(Priority.SIGNIFICANT,
                    "Error retrieving satellite data", e);
        }

        if (data == null) {
            return null;
        }

        ColorMapDataType dataType = null;
        if (data instanceof ByteBuffer) {
            dataType = signed ? ColorMapDataType.SIGNED_BYTE
                    : ColorMapDataType.BYTE;
        } else if (data instanceof ShortBuffer) {
            dataType = signed ? ColorMapDataType.SHORT
                    : ColorMapDataType.UNSIGNED_SHORT;
        } else {
            dataType = ColorMapData.getDataType(data);
        }

        return new ColorMapData(data, new int[] { datasetBounds.width,
                datasetBounds.height }, dataType, dataUnit);
    }

    /**
     * @param record2
     * @return
     */
    public static Unit<?> getRecordUnit(SatelliteRecord record) {
        Unit<?> recordUnit = null;
        String physicalElement = record.getPhysicalElement();

        if (record.getUnits() != null && record.getUnits().isEmpty() == false) {
            try {
                recordUnit = UnitFormat.getUCUMInstance().parseProductUnit(
                        record.getUnits(), new ParsePosition(0));
            } catch (ParseException e) {
                statusHandler
                        .handle(Priority.PROBLEM,
                                "Unable to parse satellite units: "
                                        + record.getUnits(), e);
            }
        }

        if (physicalElement.equals(SatelliteConstants.PRECIP)) {
            String creatingEntity = record.getCreatingEntity();
            if (creatingEntity.equals(SatelliteConstants.DMSP)
                    || creatingEntity.equals(SatelliteConstants.POES)) {
                recordUnit = new PolarPrecipWaterPixel();
            } else if (creatingEntity.equals(SatelliteConstants.MISC)) {
                recordUnit = new BlendedTPWPixel();
            }
        }

        return recordUnit;
    }

    /**
     * Extracts the data units for the data record given the PDO's base unit
     * 
     * @param recordUnit
     * @param dataRecord
     * @return
     */
    public static Unit<?> getDataUnit(Unit<?> recordUnit,
            IDataRecord dataRecord) {
        Unit<?> units = recordUnit != null ? recordUnit : Unit.ONE;
        Map<String, Object> attrs = dataRecord.getDataAttributes();
        if (attrs != null) {
            Number offset = (Number) attrs.get(SatelliteRecord.SAT_ADD_OFFSET);
            Number scale = (Number) attrs.get(SatelliteRecord.SAT_SCALE_FACTOR);

            if (offset != null) {
                double offsetVal = offset.doubleValue();
                if (offsetVal != 0.0) {
                    units = units.plus(offsetVal);
                }
            }
            if (scale != null) {
                double scaleVal = scale.doubleValue();
                if (scaleVal != 0.0) {
                    units = units.times(scaleVal);
                }
            }
        }
        return units;
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
        result = prime * result + ((record == null) ? 0 : record.hashCode());
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
        if (record == null) {
            if (other.record != null)
                return false;
        } else if (!record.equals(other.record))
            return false;
        return true;
    }

}
