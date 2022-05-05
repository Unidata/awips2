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

import javax.measure.Unit;

import com.raytheon.uf.common.colormap.image.ColorMapData;
import com.raytheon.uf.common.colormap.image.ColorMapData.ColorMapDataType;
import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.dataplugin.satellite.units.SatelliteUnitsUtil;
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

import tec.uom.se.AbstractUnit;

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
 * Apr 15, 2014  4388     bsteffen    Add method to get signed vs. unsigned from data record.
 * Jun 06, 2018  7310     mapeters    Units methods extracted to SatelliteUnitsUtil
 *
 * </pre>
 *
 * @author mschenke
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

    @Override
    public ColorMapData getColorMapData() {
        Buffer data = null;
        Unit<?> dataUnit = AbstractUnit.ONE;
        boolean signed = false;
        Request req = Request.buildSlab(
                new int[] { this.datasetBounds.x, this.datasetBounds.y },
                new int[] { this.datasetBounds.x + this.datasetBounds.width,
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
                Unit<?> recordUnit = SatelliteUnitsUtil
                        .getRecordUnit(this.record);
                signed = SatelliteUnitsUtil.isSigned(record);
                dataUnit = SatelliteUnitsUtil.getDataUnit(recordUnit, record);
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

        return new ColorMapData(data,
                new int[] { datasetBounds.width, datasetBounds.height },
                dataType, dataUnit);
    }

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
        SatDataRetriever other = (SatDataRetriever) obj;
        if (dataset == null) {
            if (other.dataset != null) {
                return false;
            }
        } else if (!dataset.equals(other.dataset)) {
            return false;
        }
        if (datasetBounds == null) {
            if (other.datasetBounds != null) {
                return false;
            }
        } else if (!datasetBounds.equals(other.datasetBounds)) {
            return false;
        }
        if (record == null) {
            if (other.record != null) {
                return false;
            }
        } else if (!record.equals(other.record)) {
            return false;
        }
        return true;
    }

}
