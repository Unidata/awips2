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

import java.util.List;

import javax.measure.unit.Unit;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.coverage.grid.InvalidGridGeometryException;
import org.geotools.geometry.DirectPosition2D;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.grid.dataset.DatasetInfo;
import com.raytheon.uf.common.dataplugin.grid.dataset.DatasetInfoLookup;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.geospatial.interpolation.BilinearInterpolation;
import com.raytheon.uf.common.geospatial.interpolation.Interpolation;
import com.raytheon.uf.common.geospatial.interpolation.NearestNeighborInterpolation;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.DisplayTypeCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.viz.grid.rsc.GridNameGenerator;
import com.raytheon.viz.grid.rsc.GridNameGenerator.IGridNameResource;
import com.raytheon.viz.grid.rsc.GridNameGenerator.LegendParameters;
import com.raytheon.viz.grid.rsc.GridResourceData;
import com.raytheon.viz.grid.util.ConformalityUtil;
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
public class D2DGridResource extends GridResource<GridResourceData> implements
        IGridNameResource {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(D2DGridResource.class);

    private boolean reprojectedData = false;

    public D2DGridResource(GridResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        if (resourceData.getNameGenerator() == null) {
            resourceData.setNameGenerator(new GridNameGenerator());
        }

    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        String paramAbbrev = "";
        for (GridRecord record : resourceData.getRecords()) {
            paramAbbrev = record.getParameter().getAbbreviation();
            addDataObject(record);
        }
        this.getCapability(DisplayTypeCapability.class)
                .setAlternativeDisplayTypes(
                        FieldDisplayTypesFactory.getInstance().getDisplayTypes(
                                paramAbbrev));
        super.initInternal(target);
    }

    @Override
    public void addDataObject(PluginDataObject pdo) {
        super.addDataObject(pdo);
        requestData(pdo.getDataTime());
    }

    @Override
    protected GeneralGridData getData(GridRecord gridRecord)
            throws VizException {
        Unit<?> dataUnit = gridRecord.getParameter().getUnit();
        // Reqest data for tilts if this is Std Env sampling.
        IDataRecord[] dataRecs = GridResourceData.getDataRecordsForTilt(
                gridRecord, descriptor);
        if (dataRecs == null) {
            dataRecs = DataCubeContainer.getDataRecord(gridRecord);
            if (dataRecs == null) {
                return null;
            }
        }
        // For world wide lat lon grids we reproject, this is done to match A1,
        // but it also makes the wind barbs look more evenly spaced near the
        // pole.
        GridCoverage location = gridRecord.getLocation();
        GeneralGridData data = getData(dataRecs, location.getGridGeometry(),
                dataUnit);
        if (location != null && location.getSpacingUnit().equals("degree")) {
            if (!ConformalityUtil.testConformality(location.getGridGeometry(),
                    descriptor.getGridGeometry())) {
                data = reprojectData(data);
            }
        }
        // Wind Direction(and possibly others) can be set so that we rotate the
        // direction to be relative to the north pole instead of grid relative.
        if (stylePreferences != null
                && stylePreferences.getDisplayFlags()
                        .hasFlag("RotateVectorDir")) {
            GridGeometry2D geom = GridGeometry2D.wrap(location
                    .getGridGeometry());
            MathTransform grid2crs = geom.getGridToCRS();
            try {
                MathTransform crs2ll = MapUtil.getTransformToLatLon(geom
                        .getCoordinateReferenceSystem());
                for (int i = 0; i < geom.getGridRange2D().width; i++) {
                    for (int j = 0; j < geom.getGridRange2D().height; j++) {
                        int index = i + j * geom.getGridRange2D().width;
                        float dir = data.getScalarData().get(index);
                        if (dir > -9999) {
                            DirectPosition2D dp = new DirectPosition2D(i, j);
                            grid2crs.transform(dp, dp);
                            crs2ll.transform(dp, dp);
                            Coordinate ll = new Coordinate(dp.x, dp.y);
                            float rot = (float) MapUtil.rotation(ll, geom);
                            dir = (dir + rot + 180) % 360;
                            data.getScalarData().put(index, dir);
                        }
                    }
                }
            } catch (TransformException e) {
                throw new VizException(e);
            } catch (InvalidGridGeometryException e) {
                throw new VizException(e);
            } catch (FactoryException e) {
                throw new VizException(e);
            }
        }
        return data;
    }

    public GeneralGridData reprojectData(GeneralGridData data) {
        if (descriptor == null) {
            return data;
        }
        try {
            GeneralGridGeometry targetGeometry = MapUtil.reprojectGeometry(data
                    .getGridGeometry(), descriptor.getGridGeometry()
                    .getEnvelope(), true, 2);
            reprojectedData = true;
            Interpolation interpolation = null;
            if (this.hasCapability(ImagingCapability.class)
                    && !this.getCapability(ImagingCapability.class)
                            .isInterpolationState()) {
                interpolation = new NearestNeighborInterpolation();
            } else {
                BilinearInterpolation bilinear = new BilinearInterpolation();
                bilinear.setMissingThreshold(1.0f);
                interpolation = bilinear;
            }
            data = data.reproject(targetGeometry, interpolation);
        } catch (FactoryException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        } catch (TransformException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
        // If exceptions happened just return the original data, the display
        // should still work just fine.
        return data;
    }

    @Override
    public LegendParameters getLegendParameters() {
        GridRecord record = getCurrentGridRecord();
        if (record == null) {
            record = getAnyGridRecord();
            if (record == null) {
                return null;
            }
        }
        LegendParameters legendParams = new LegendParameters();
        DatasetInfo info = DatasetInfoLookup.getInstance().getInfo(
                record.getDatasetId());
        if (info == null) {
            legendParams.model = record.getDatasetId();
        } else {
            legendParams.model = info.getTitle();
        }
        legendParams.level = record.getLevel();
        legendParams.parameter = record.getParameter().getName();
        legendParams.ensembleId = record.getEnsembleId();
        legendParams.dataTime = descriptor.getFramesInfo().getTimeForResource(
                this);

        if (stylePreferences != null) {
            legendParams.unit = stylePreferences.getDisplayUnitLabel();
        }

        if (legendParams.unit == null || legendParams.unit.isEmpty()) {
            if (record.getParameter().getUnit().equals(Unit.ONE)) {
                legendParams.unit = "";
            } else {
                legendParams.unit = record.getParameter().getUnitString();
            }
        }
        List<DisplayType> displayTypes = FieldDisplayTypesFactory.getInstance()
                .getDisplayTypes(record.getParameter().getAbbreviation());
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

    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        if (getDisplayType() == DisplayType.IMAGE) {
            return super.inspect(coord);
        } else if (resourceData.isSampling()) {
            GridRecord record = getCurrentGridRecord();
            if (record == null) {
                return super.inspect(coord);
            }
            return record.getParameter().getAbbreviation() + "="
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
    public void project(CoordinateReferenceSystem crs) throws VizException {
        if (reprojectedData) {
            clearRequestedData();
        }
        super.project(crs);
    }

}
