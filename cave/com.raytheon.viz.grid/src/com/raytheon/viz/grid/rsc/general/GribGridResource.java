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
import java.util.List;

import javax.measure.Measure;
import javax.measure.unit.Unit;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grib.GribModel;
import com.raytheon.uf.common.dataplugin.grib.GribRecord;
import com.raytheon.uf.common.dataplugin.grib.spatial.projections.GridCoverage;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.interpolation.AbstractInterpolation;
import com.raytheon.uf.common.geospatial.interpolation.BilinearInterpolation;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.style.ParamLevelMatchCriteria;
import com.raytheon.uf.viz.core.style.level.Level;
import com.raytheon.uf.viz.core.style.level.SingleLevel;
import com.raytheon.viz.grid.GridLevelTranslator;
import com.raytheon.viz.grid.rsc.GridNameGenerator.IGridNameResource;
import com.raytheon.viz.grid.rsc.GridNameGenerator.LegendParameters;
import com.raytheon.viz.grid.rsc.GridResourceData;

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
public class GribGridResource extends AbstractGridResource<GridResourceData>
        implements IGridNameResource {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractGridResource.class);

    private GribModel gribModel;

    private AbstractInterpolation reprojectionInterpolation;

    protected GribGridResource(GridResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
    }

    @Override
    protected void addDataObject(PluginDataObject pdo) {
        if (pdo instanceof GribRecord) {
            if (gribModel == null) {
                GribRecord gribRecord = (GribRecord) pdo;
                gribModel = gribRecord.getModelInfo();
                calculateReprojection();
            }
            super.addDataObject(pdo);
        }
    }

    @Override
    public GridGeometry2D getGridGeometry() {
        if (reprojectionInterpolation == null) {
            return gribModel.getLocation().getGridGeometry();
        } else {
            return GridGeometry2D.wrap(reprojectionInterpolation
                    .getTargetGeometry());
        }
    }

    @Override
    public GeneralGridData getData(DataTime time, List<PluginDataObject> pdos)
            throws VizException {
        GribRecord gribRecord = (GribRecord) pdos.get(0);
        GeneralGridData data = getData(gribRecord);
        if (reprojectionInterpolation != null) {
            try {
                if (data.isVector()) {
                    reprojectionInterpolation.setData(data.getUComponent()
                            .array());
                    float[] udata = reprojectionInterpolation
                            .getReprojectedGrid();
                    reprojectionInterpolation.setData(data.getVComponent()
                            .array());
                    float[] vdata = reprojectionInterpolation
                            .getReprojectedGrid();
                    data = GeneralGridData.createVectorDataUV(
                            FloatBuffer.wrap(udata), FloatBuffer.wrap(vdata),
                            data.getDataUnit());
                } else {
                    reprojectionInterpolation.setData(data.getScalarData()
                            .array());
                    float[] fdata = reprojectionInterpolation
                            .getReprojectedGrid();
                    data = GeneralGridData.createScalarData(
                            FloatBuffer.wrap(fdata), data.getDataUnit());
                }
            } catch (TransformException e) {
                throw new VizException(e);
            } catch (FactoryException e) {
                throw new VizException(e);
            }
        }
        return data;
    }

    private GeneralGridData getData(GribRecord gribRecord) throws VizException {
        Unit<?> dataUnit = gribRecord.getModelInfo().getParameterUnitObject();
        IDataRecord[] dataRecs = DataCubeContainer.getDataRecord(gribRecord);
        if (dataRecs.length == 1) {
            if (dataRecs[0] instanceof FloatDataRecord) {
                float[] fdata = ((FloatDataRecord) dataRecs[0]).getFloatData();
                return GeneralGridData.createScalarData(
                        FloatBuffer.wrap(fdata), dataUnit);
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

    private FloatBuffer wrapDataRecord(IDataRecord record) {
        if (record instanceof FloatDataRecord) {
            float[] fdata = ((FloatDataRecord) record).getFloatData();
            return FloatBuffer.wrap(fdata);
        }
        return null;
    }

    @Override
    public ParamLevelMatchCriteria getMatchCriteria() {
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
    public LegendParameters getLegendParameters() {
        LegendParameters legendParams = new LegendParameters();
        legendParams.model = gribModel;
        if (stylePreferences != null) {
            legendParams.unit = stylePreferences.getDisplayUnitLabel();
        }
        if (legendParams.unit == null) {
            legendParams.unit = gribModel.getParameterUnit();
        }
        legendParams.type = DisplayType.getAbbreviation(getDisplayType());

        return legendParams;
    }

    private void calculateReprojection() {
        GridCoverage location = gribModel.getLocation();
        if (descriptor != null && location != null
                && location.getSpacingUnit().equals("degree")) {
            double dx = location.getDx();
            Integer nx = location.getNx();
            if (dx * nx >= 360) {
                try {
                    GridGeometry2D sourceGeometry = location.getGridGeometry();
                    GeneralGridGeometry targetGeometry = sourceGeometry;
                    if (descriptor != null) {
                        targetGeometry = MapUtil.reprojectGeometry(
                                sourceGeometry, descriptor.getGridGeometry()
                                        .getEnvelope(), true);
                    }
                    reprojectionInterpolation = new BilinearInterpolation(
                            sourceGeometry, targetGeometry);
                    clearRequestedData();
                } catch (Exception e) {
                    reprojectionInterpolation = null;
                    statusHandler
                            .handle(Priority.PROBLEM,
                                    "Error reprojecting grid, grid will not be reprojected",
                                    e);
                }
            }
        }
    }

    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        super.project(crs);
        calculateReprojection();
    }

    @Override
    public void setDescriptor(IMapDescriptor descriptor) {
        super.setDescriptor(descriptor);
        calculateReprojection();
    }

    @Override
    protected String inspect(Measure<Float, ?> value) {
        if (resourceData.isSampling()) {
            return super.inspect(value);
        } else {
            return null;
        }
    }

}
