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
package com.raytheon.viz.grid.rsc.general;

import java.nio.FloatBuffer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.measure.unit.Unit;

import org.geotools.coverage.grid.GeneralGridGeometry;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.grid.util.GridStyleUtil;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.style.ParamLevelMatchCriteria;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

/**
 * 
 * A version of AbstractGridResource that displays data from GridRecords.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- ----------------------------------------
 * Mar 09, 2011           bsteffen    Initial creation
 * Sep 24, 2013  2404     bclement    match criteria built using GridStyleUtil
 * Jan 14, 2014  2661     bsteffen    Switch vectors to u,v only.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class GridResource<T extends AbstractResourceData> extends
        AbstractGridResource<T> {

    public GridResource(T resourceData, LoadProperties loadProperties) {
        super(resourceData, loadProperties);
    }

    @Override
    public List<GeneralGridData> getData(DataTime time,
            List<PluginDataObject> pdos) throws VizException {
        if (pdos == null) {
            return null;
        }
        List<GeneralGridData> dataList = new ArrayList<GeneralGridData>(
                pdos.size());
        for (PluginDataObject pdo : pdos) {
            GeneralGridData data = getData((GridRecord) pdo);
            if (data != null) {
                dataList.add(data);
            }
        }
        return dataList;
    }

    protected GeneralGridData getData(GridRecord gridRecord)
            throws VizException {
        Unit<?> dataUnit = gridRecord.getParameter().getUnit();
        IDataRecord[] dataRecs = DataCubeContainer.getDataRecord(gridRecord);
        if (dataRecs == null) {
            return null;
        }
        return getData(dataRecs, gridRecord.getLocation().getGridGeometry(),
                dataUnit);
    }

    protected GeneralGridData getData(IDataRecord[] dataRecs,
            GeneralGridGeometry gridGeometry, Unit<?> dataUnit) {
        if (dataRecs.length == 1) {
            if (dataRecs[0] instanceof FloatDataRecord) {
                return GeneralGridData.createScalarData(gridGeometry,
                        wrapDataRecord(dataRecs[0]), dataUnit);

            }
        } else if (dataRecs.length == 2) {
            FloatBuffer mag = wrapDataRecord(dataRecs[0]);
            FloatBuffer dir = wrapDataRecord(dataRecs[1]);
            return GeneralGridData.createVectorDataUV(gridGeometry, mag, dir,
                    dataUnit);
        }
        return null;
    }

    protected FloatBuffer wrapDataRecord(IDataRecord record) {
        if (record instanceof FloatDataRecord) {
            float[] fdata = ((FloatDataRecord) record).getFloatData();
            fdata = Arrays.copyOf(fdata, fdata.length);
            for (int i = 0; i < fdata.length; i++) {
                if (fdata[i] <= -9999) {
                    fdata[i] = Float.NaN;
                }
            }
            return FloatBuffer.wrap(fdata);
        }
        return null;
    }

    @Override
    public ParamLevelMatchCriteria getMatchCriteria() {
        GridRecord record = getAnyGridRecord();
        if (record == null) {
            return null;
        }
        return GridStyleUtil.getMatchCriteria(record);
    }

    @Override
    public String getName() {
        GridRecord record = getCurrentGridRecord();
        if (record == null) {
            return "Grib Data";
        }
        return record.getDatasetId() + " " + record.getLevel().toString() + " "
                + record.getParameter().getName();
    }

    protected GridRecord getCurrentGridRecord() {
        List<PluginDataObject> pdos = getCurrentPluginDataObjects();
        if (pdos == null || pdos.isEmpty()) {
            return null;
        }
        return (GridRecord) pdos.get(0);
    }

    protected GridRecord getAnyGridRecord() {
        GridRecord record = getCurrentGridRecord();
        if (record == null) {
            for (DataTime time : getDataTimes()) {
                List<PluginDataObject> pdos = getPluginDataObjects(time);
                if (pdos != null && !pdos.isEmpty()) {
                    record = (GridRecord) pdos.get(0);
                    break;
                }
            }
        }
        return record;
    }

}
