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
import java.text.ParsePosition;
import java.util.Map;

import javax.measure.Unit;

import com.raytheon.uf.common.colormap.image.ColorMapData;
import com.raytheon.uf.common.colormap.image.ColorMapData.ColorMapDataType;
import com.raytheon.uf.common.dataplugin.npp.viirs.VIIRSDataRecord;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.ShortDataRecord;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.datacube.DataCubeContainer;

import tec.uom.se.AbstractUnit;
import tec.uom.se.format.SimpleUnitFormat;

/**
 * VIIRS Colormap data callback, requests data for VIIRSDataRecord
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- ----------------------------------------
 * Nov 30, 2011           mschenke  Initial creation
 * Apr 04, 2013           djohnson  Remove color import.
 * Nov 29, 2018  7605     bsteffen  Set the data unit.
 * Apr 15, 2019  7596     lsingh    Updated units framework to JSR-363.
 * Mar 24, 2020  8076     randerso  Fix ClassCastException in getDataUnit()
 *
 * </pre>
 *
 * @author mschenke
 */
public class VIIRSDataCallback implements IColorMapDataRetrievalCallback {

    protected static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(VIIRSDataCallback.class);

    private static String ERROR_MSG = "Non-numeric value for %s in VIIRSDataRecord: \"%s\". Value ignored.";

    private VIIRSDataRecord dataRecord;

    private int level;

    private Rectangle dataArea;

    public VIIRSDataCallback(VIIRSDataRecord dataRecord, int level,
            Rectangle dataArea) {
        this.dataRecord = dataRecord;
        this.level = level;
        this.dataArea = dataArea;
    }

    @Override
    public ColorMapData getColorMapData() throws VizException {
        try {
            IDataRecord[] records = DataCubeContainer.getDataRecord(dataRecord,
                    Request.buildSlab(new int[] { dataArea.x, dataArea.y },
                            new int[] { dataArea.x + dataArea.width,
                                    dataArea.y + dataArea.height }),
                    VIIRSDataRecord.getDataSet(level));
            IDataRecord rawData = records[0];
            int[] dims = { (int) rawData.getSizes()[0],
                    (int) rawData.getSizes()[1] };
            Unit<?> unit = getDataUnit(rawData);
            if (rawData instanceof ShortDataRecord) {
                Buffer shortData = ShortBuffer
                        .wrap(((ShortDataRecord) rawData).getShortData());
                return new ColorMapData(shortData, dims,
                        ColorMapDataType.UNSIGNED_SHORT, unit);
            } else if (rawData instanceof FloatDataRecord) {
                Buffer floatData = FloatBuffer
                        .wrap(((FloatDataRecord) rawData).getFloatData());
                return new ColorMapData(floatData, dims, ColorMapDataType.FLOAT,
                        unit);
            } else {
                throw new VizException(
                        "Could not handle IDataRecord: " + rawData);
            }
        } catch (Throwable t) {
            throw new VizException(
                    "Error requesting viirs slab: " + t.getLocalizedMessage(),
                    t);
        }
    }

    public static Unit<?> getDataUnit(IDataRecord record) {
        Unit<?> dataUnit = AbstractUnit.ONE;

        Map<String, Object> attrs = record.getDataAttributes();
        if (attrs != null) {
            double offset = getNumericAttribute(attrs,
                    VIIRSDataRecord.OFFSET_ID, 0.0);
            double scale = getNumericAttribute(attrs, VIIRSDataRecord.SCALE_ID,
                    1.0);

            String unitStr = (String) attrs.get(VIIRSDataRecord.UNIT_ID);
            if (unitStr != null) {
                try {
                    dataUnit = SimpleUnitFormat
                            .getInstance(SimpleUnitFormat.Flavor.ASCII)
                            .parseObject(unitStr, new ParsePosition(0));
                } catch (Exception e) {
                    statusHandler.debug("Viirs unit conversion error", e);
                }
            }
            dataUnit = dataUnit.shift(offset);
            dataUnit = dataUnit.multiply(scale);
        }
        return dataUnit;
    }

    private static double getNumericAttribute(Map<String, Object> attrs,
            String key, double defaultValue) {
        double value = defaultValue;
        Object obj = attrs.get(key);
        if (obj instanceof Number) {
            value = ((Number) obj).doubleValue();
        } else {
            statusHandler.warn(String.format(ERROR_MSG, key, obj));
        }

        return value;
    }

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

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        VIIRSDataCallback other = (VIIRSDataCallback) obj;
        if (dataArea == null) {
            if (other.dataArea != null) {
                return false;
            }
        } else if (!dataArea.equals(other.dataArea)) {
            return false;
        }
        if (dataRecord == null) {
            if (other.dataRecord != null) {
                return false;
            }
        } else if (!dataRecord.equals(other.dataRecord)) {
            return false;
        }
        if (level != other.level) {
            return false;
        }
        return true;
    }

}
