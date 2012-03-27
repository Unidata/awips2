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

import org.geotools.coverage.grid.GridGeometry2D;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grib.GribModel;
import com.raytheon.uf.common.dataplugin.grib.GribRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.style.ParamLevelMatchCriteria;
import com.raytheon.uf.viz.core.style.level.Level;
import com.raytheon.uf.viz.core.style.level.SingleLevel;
import com.raytheon.viz.grid.GridLevelTranslator;

/**
 * 
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 9, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class GribGridResource<T extends AbstractResourceData> extends
        AbstractGridResource<T> {

    protected GribModel gribModel;

    public GribGridResource(T resourceData, LoadProperties loadProperties) {
        super(resourceData, loadProperties);
    }

    @Override
    public void addDataObject(PluginDataObject pdo) {
        if (pdo instanceof GribRecord) {
            if (gribModel == null) {
                GribRecord gribRecord = (GribRecord) pdo;
                gribModel = gribRecord.getModelInfo();
            }
            super.addDataObject(pdo);
        }
    }

    @Override
    public GridGeometry2D getGridGeometry() {
        return gribModel.getLocation().getGridGeometry();
    }

    @Override
    public GeneralGridData getData(DataTime time, List<PluginDataObject> pdos)
            throws VizException {
        if (pdos == null) {
            return null;
        }
        GribRecord gribRecord = (GribRecord) pdos.get(0);
        Unit<?> dataUnit = gribRecord.getModelInfo().getParameterUnitObject();
        IDataRecord[] dataRecs = DataCubeContainer.getDataRecord(gribRecord);
        return getData(dataRecs, dataUnit);
    }

    protected GeneralGridData getData(IDataRecord[] dataRecs, Unit<?> dataUnit) {
        if (dataRecs.length == 1) {
            if (dataRecs[0] instanceof FloatDataRecord) {
                return GeneralGridData.createScalarData(
                        wrapDataRecord(dataRecs[0]), dataUnit);
            }
        } else if (dataRecs.length == 2) {
            FloatBuffer mag = wrapDataRecord(dataRecs[0]);
            FloatBuffer dir = wrapDataRecord(dataRecs[1]);
            return GeneralGridData.createVectorData(mag, dir, dataUnit);
        } else if (dataRecs.length == 4) {
            FloatBuffer mag = wrapDataRecord(dataRecs[0]);
            FloatBuffer dir = wrapDataRecord(dataRecs[1]);
            FloatBuffer u = wrapDataRecord(dataRecs[2]);
            FloatBuffer v = wrapDataRecord(dataRecs[3]);
            return GeneralGridData.createVectorData(mag, dir, u, v, dataUnit);
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
        if (gribModel == null) {
            return null;
        }
        ParamLevelMatchCriteria criteria = new ParamLevelMatchCriteria();
        criteria.setParameterName(new ArrayList<String>());
        criteria.setLevels(new ArrayList<Level>());
        criteria.setCreatingEntityNames(new ArrayList<String>());
        String parameter = gribModel.getParameterAbbreviation();
        SingleLevel level = GridLevelTranslator.constructMatching(gribModel
                .getLevel());
        String creatingEntity = gribModel.getModelName();
        if (!criteria.getParameterNames().contains(parameter)) {
            criteria.getParameterNames().add(parameter);
        }
        if (!criteria.getLevels().contains(level)) {
            criteria.getLevels().add(level);
        }
        if (!criteria.getCreatingEntityNames().contains(creatingEntity)) {
            criteria.getCreatingEntityNames().add(creatingEntity);
        }
        return criteria;
    }

    @Override
    public String getName() {
        if (gribModel == null) {
            return "Grib Data";
        } else {
            return gribModel.getModelTitle() + " "
                    + gribModel.getLevel().toString() + " "
                    + gribModel.getParameterName();
        }
    }

}
