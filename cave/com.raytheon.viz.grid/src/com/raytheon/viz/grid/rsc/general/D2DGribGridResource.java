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
import java.util.List;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridEnvelope2D;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.DirectPosition2D;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grib.GribRecord;
import com.raytheon.uf.common.dataplugin.grib.spatial.projections.GridCoverage;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.geospatial.interpolation.AbstractInterpolation;
import com.raytheon.uf.common.geospatial.interpolation.BilinearInterpolation;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.grid.rsc.GridNameGenerator;
import com.raytheon.viz.grid.rsc.GridNameGenerator.IGridNameResource;
import com.raytheon.viz.grid.rsc.GridNameGenerator.LegendParameters;
import com.raytheon.viz.grid.rsc.GridResourceData;
import com.raytheon.viz.grid.xml.FieldDisplayTypesFactory;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * A much more complex grib grid resource that attempts to match A1 displays
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
public class D2DGribGridResource extends GribGridResource<GridResourceData>
        implements IGridNameResource {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(D2DGribGridResource.class);

    private AbstractInterpolation reprojectionInterpolation;

    public D2DGribGridResource(GridResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        if (resourceData.getNameGenerator() == null) {
            resourceData.setNameGenerator(new GridNameGenerator());
        }

    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        for (GribRecord record : resourceData.getRecords()) {
            addDataObject(record);
        }
        super.initInternal(target);
    }

    @Override
    public void addDataObject(PluginDataObject pdo) {
        if (gribModel == null) {
            super.addDataObject(pdo);
            calculateReprojection();
        } else {
            super.addDataObject(pdo);
        }
        requestData(pdo.getDataTime());
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
        GeneralGridData data = null;
        if (pdos == null) {
            return null;
        }
        // Attempt to figure out if there is a special tilt data request.
        GribRecord gribRecord = (GribRecord) pdos.get(0);
        IDataRecord[] recs = GridResourceData.getDataRecordsForTilt(gribRecord,
                descriptor);
        if (recs != null) {
            data = getData(recs, gribRecord.getModelInfo()
                    .getParameterUnitObject());
        }
        // If there was no special tilt data just request data from super.
        if (data == null) {
            data = super.getData(time, pdos);
        }
        // Now reproject if we need to.
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
                    // When reprojecting it is necessary to recalculate the
                    // direction of vectors based off the change in the "up"
                    // direction
                    GridGeometry2D sourceGeometry = GridGeometry2D
                            .wrap(reprojectionInterpolation.getSourceGeometry());
                    GridGeometry2D targetGeometry = GridGeometry2D
                            .wrap(reprojectionInterpolation.getTargetGeometry());
                    GridEnvelope2D targetRange = targetGeometry
                            .getGridRange2D();

                    MathTransform grid2crs = targetGeometry.getGridToCRS();
                    MathTransform crs2ll = MapUtil
                            .getTransformToLatLon(targetGeometry
                                    .getCoordinateReferenceSystem());

                    for (int i = 0; i < targetRange.width; i++) {
                        for (int j = 0; j < targetRange.height; j++) {
                            int index = i + j * targetRange.width;
                            if (udata[index] > -9999) {
                                DirectPosition2D dp = new DirectPosition2D(i, j);
                                grid2crs.transform(dp, dp);
                                crs2ll.transform(dp, dp);
                                Coordinate ll = new Coordinate(dp.x, dp.y);
                                double rot = MapUtil.rotation(ll,
                                        targetGeometry);
                                double rot2 = MapUtil.rotation(ll,
                                        sourceGeometry);
                                double cos = Math.cos(Math.toRadians(180 + rot
                                        - rot2));
                                double sin = Math.sin(Math.toRadians(180 + rot
                                        - rot2));
                                double u = udata[index];
                                double v = vdata[index];
                                udata[index] = (float) (cos * u - sin * v);
                                vdata[index] = (float) (sin * u + cos * v);
                            }
                        }
                    }
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
        // TODO before contours can be switched, we need to handle vector
        // direction style rule and reprojection of this data.
        // TODO before imaging can be switched over data mapping needs to be
        // incorporated.
        return data;
    }

    @Override
    public LegendParameters getLegendParameters() {
        LegendParameters legendParams = new LegendParameters();
        List<PluginDataObject> pdos = getCurrentPluginDataObjects();
        if (pdos != null && !pdos.isEmpty()) {
            gribModel = ((GribRecord) pdos.get(0)).getModelInfo();
        }
        legendParams.model = gribModel;
        if (stylePreferences != null) {
            legendParams.unit = stylePreferences.getDisplayUnitLabel();
        }

        List<DisplayType> displayTypes = null;
        if (gribModel != null) {
            if (legendParams.unit == null || legendParams.unit.isEmpty()) {
                legendParams.unit = gribModel.getParameterUnit();
            }
            displayTypes = FieldDisplayTypesFactory.getInstance()
                    .getDisplayTypes(gribModel.getParameterAbbreviation());
        }
        DisplayType displayType = getDisplayType();
        if (displayTypes != null && !displayTypes.isEmpty()
                && displayTypes.get(0).equals(displayType)) {
            // The default type does not display in the legend
            legendParams.type = "";
        } else if (displayType == DisplayType.STREAMLINE) {
            legendParams.type = "Streamlines";
        } else if (displayType == DisplayType.BARB) {
            legendParams.type = "Wind Barbs";
        } else if (displayType == DisplayType.ARROW) {
            legendParams.type = "Arrows";
        } else if (displayType == DisplayType.IMAGE) {
            legendParams.type = "Img";
        }

        return legendParams;
    }

    private void calculateReprojection() {
        if (descriptor == null || gribModel == null) {
            return;
        }
        GridCoverage location = gribModel.getLocation();
        if (location != null && location.getSpacingUnit().equals("degree")) {
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
    public String inspect(ReferencedCoordinate coord) throws VizException {
        if (resourceData.isSampling()) {
            return gribModel.getParameterAbbreviation() + " = "
                    + super.inspect(coord);
        } else {
            return null;
        }
    }

    @Override
    public String getName() {
        if (resourceData == null) {
            return super.getName();
        }
        AbstractNameGenerator generator = resourceData.getNameGenerator();
        if (generator == null) {
            return super.getName();
        }
        return generator.getName(this);
    }

    @Override
    public boolean isLoadableAsImage() {
        if (super.isLoadableAsImage()) {
            DisplayType displayType = getDisplayType();
            List<DisplayType> displayTypes = FieldDisplayTypesFactory
                    .getInstance().getDisplayTypes(
                            gribModel.getParameterAbbreviation());
            if (displayTypes == null || displayTypes.isEmpty()) {
                return displayType == DisplayType.CONTOUR;
            }
            return displayTypes.contains(DisplayType.IMAGE);
        }
        return false;
    }

    @Override
    public boolean isStreamlineVector() {
        if (super.isStreamlineVector()) {
            List<DisplayType> displayTypes = FieldDisplayTypesFactory
                    .getInstance().getDisplayTypes(
                            gribModel.getParameterAbbreviation());
            if (displayTypes == null || displayTypes.isEmpty()) {
                return true;
            }
            return displayTypes.contains(DisplayType.STREAMLINE);
        }
        return false;
    }

    @Override
    public boolean isArrowVector() {
        if (super.isArrowVector()) {
            List<DisplayType> displayTypes = FieldDisplayTypesFactory
                    .getInstance().getDisplayTypes(
                            gribModel.getParameterAbbreviation());
            if (displayTypes == null || displayTypes.isEmpty()) {
                return true;
            }
            return displayTypes.contains(DisplayType.ARROW);
        }
        return false;
    }

    @Override
    public boolean isWindVector() {
        if (super.isWindVector()) {
            List<DisplayType> displayTypes = FieldDisplayTypesFactory
                    .getInstance().getDisplayTypes(
                            gribModel.getParameterAbbreviation());
            if (displayTypes == null || displayTypes.isEmpty()) {
                return true;
            }
            return displayTypes.contains(DisplayType.BARB);
        }
        return false;
    }

}
