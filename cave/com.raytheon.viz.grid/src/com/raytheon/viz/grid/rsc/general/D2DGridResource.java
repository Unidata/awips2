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
import java.util.Map;

import javax.measure.unit.Unit;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridEnvelope2D;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.coverage.grid.InvalidGridGeometryException;
import org.geotools.geometry.DirectPosition2D;
import org.geotools.geometry.Envelope2D;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.grid.dataset.DatasetInfo;
import com.raytheon.uf.common.dataplugin.grid.dataset.DatasetInfoLookup;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.geospatial.interpolation.BilinearInterpolation;
import com.raytheon.uf.common.geospatial.interpolation.Interpolation;
import com.raytheon.uf.common.geospatial.interpolation.NearestNeighborInterpolation;
import com.raytheon.uf.common.geospatial.util.EnvelopeIntersection;
import com.raytheon.uf.common.geospatial.util.GridGeometryWrapChecker;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.gridcoverage.LatLonGridCoverage;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.DisplayTypeCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.viz.grid.rsc.GridNameGenerator;
import com.raytheon.viz.grid.rsc.GridNameGenerator.IGridNameResource;
import com.raytheon.viz.grid.rsc.GridNameGenerator.LegendParameters;
import com.raytheon.viz.grid.rsc.GridResourceData;
import com.raytheon.viz.grid.util.ReprojectionUtil;
import com.raytheon.viz.grid.xml.FieldDisplayTypesFactory;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;

/**
 * 
 * A much more complex grib grid resource that attempts to match A1 displays
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Mar 09, 2011           bsteffen    Initial creation
 * Feb 25, 2013  1659     bsteffen    Add PDOs to D2DGridResource in
 *                                    constructor to avoid duplicate data
 *                                    requests.
 * Jul 15, 2013  2107     bsteffen    Fix sampling of grid vector arrows.
 * Aug 27, 2013  2287     randerso    Removed 180 degree adjustment required by
 *                                    error in Maputil.rotation
 * Sep 12, 2013  2309     bsteffen    Request subgrids whenever possible.
 * Sep 24, 2013  15972    D. Friedman Make reprojection of grids configurable.
 * Nov 19, 2013  2532     bsteffen    Special handling of grids larger than the
 *                                    world.
 * Mar 27, 2014  2945     bsteffen    Enable omitting the plane from the legend
 *                                    based off style rules.
 * 
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

    /*
     * Flag indicating if the resource is displaying the full unaltered data or
     * if some of the data has been modified for the descriptor(such as
     * modification. If modification has occurred then a reproject will require
     * requesting the data again so it can be reformatted for the new display.
     */
    private boolean dataModified = false;

    private Boolean lastInterpolationState = null;

    public D2DGridResource(GridResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        if (resourceData.getNameGenerator() == null) {
            resourceData.setNameGenerator(new GridNameGenerator());
        }
        for (GridRecord record : resourceData.getRecords()) {
            addDataObject(record);
        }
        if (this.hasCapability(ImagingCapability.class)) {
            lastInterpolationState = this
                    .getCapability(ImagingCapability.class)
                    .isInterpolationState();
        }
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        for (DataTime time : getDataTimes()) {
            requestData(time);
        }
        GridRecord randomRec = getAnyGridRecord();
        if (randomRec != null) {
            String paramAbbrev = randomRec.getParameter().getAbbreviation();
            this.getCapability(DisplayTypeCapability.class)
                    .setAlternativeDisplayTypes(
                            FieldDisplayTypesFactory.getInstance()
                                    .getDisplayTypes(paramAbbrev));
        }
        super.initInternal(target);
    }

    @Override
    public void addDataObject(PluginDataObject pdo) {
        super.addDataObject(pdo);
        if (descriptor != null) {
            requestData(pdo.getDataTime());
        }
    }

    @Override
    protected GeneralGridData getData(GridRecord gridRecord)
            throws VizException {
        Unit<?> dataUnit = gridRecord.getParameter().getUnit();
        GridCoverage location = gridRecord.getLocation();
        /*
         * Detect a special case. Several of the D2D specific "enhancements" do
         * not handle grids larger than the world. Since this is a rare edge
         * case, and fixing it would be very complicated just don't apply the
         * "enhancements".
         */
        boolean gridLargerThanWorld = location instanceof LatLonGridCoverage
                && location.getNx() * location.getDx() > 360;
        GridGeometry2D gridGeometry = location.getGridGeometry();

        /* Request data for tilts if this is Std Env sampling. */
        IDataRecord[] dataRecs = GridResourceData.getDataRecordsForTilt(
                gridRecord, descriptor);
        if (dataRecs == null) {
            GridGeometry2D subGridGeometry = gridGeometry;
            if (!gridLargerThanWorld) {
                subGridGeometry = calculateSubgrid(gridGeometry);
            }
            if (subGridGeometry == null) {
                return null;
            } else if (subGridGeometry.equals(gridGeometry)) {
                dataRecs = DataCubeContainer.getDataRecord(gridRecord);
            } else if (subGridGeometry != null) {
                /* transform subgrid envelope into a slab request. */
                GridEnvelope2D subGridRange = subGridGeometry.getGridRange2D();
                int[] min = { subGridRange.getLow(0), subGridRange.getLow(1) };
                int[] max = { subGridRange.getHigh(0) + 1,
                        subGridRange.getHigh(1) + 1 };
                Request request = Request.buildSlab(min, max);
                dataRecs = DataCubeContainer.getDataRecord(gridRecord, request,
                        null);
                /*
                 * gridGeometries used in renderables are expected to have min
                 * x,y be 0.
                 */
                subGridRange.x = 0;
                subGridRange.y = 0;
                gridGeometry = new GridGeometry2D(subGridRange,
                        subGridGeometry.getEnvelope());
                dataModified = true;
            }
            if (dataRecs == null) {
                return null;
            }
        }

        GeneralGridData data = getData(dataRecs, gridGeometry, dataUnit);
        // For some grids, we may reproject (e.g., world-wide lat/lon grids),
        // this is done to match A1, but it also makes the wind barbs look
        // more evenly spaced near the pole.
        if (ReprojectionUtil.shouldReproject(gridRecord, gridGeometry,
                getDisplayType(), descriptor.getGridGeometry())) {
            if (!gridLargerThanWorld
                    || GridGeometryWrapChecker.checkForWrapping(gridGeometry) != -1) {
                data = reprojectData(data);
            }
        }
        /*
         * Wind Direction(and possibly others) can be set so that we rotate the
         * direction to be relative to the north pole instead of grid relative.
         */
        if ((stylePreferences != null)
                && stylePreferences.getDisplayFlags()
                        .hasFlag("RotateVectorDir")) {
            GridEnvelope2D gridRange = gridGeometry.getGridRange2D();
            MathTransform grid2crs = gridGeometry.getGridToCRS();
            try {
                MathTransform crs2ll = MapUtil
                        .getTransformToLatLon(gridGeometry
                                .getCoordinateReferenceSystem());
                for (int i = 0; i < gridRange.width; i++) {
                    for (int j = 0; j < gridRange.height; j++) {
                        int index = i + (j * gridRange.width);
                        float dir = data.getScalarData().get(index);
                        if (dir > -9999) {
                            DirectPosition2D dp = new DirectPosition2D(i, j);
                            grid2crs.transform(dp, dp);
                            crs2ll.transform(dp, dp);
                            Coordinate ll = new Coordinate(dp.x, dp.y);
                            float rot = (float) MapUtil.rotation(ll,
                                    gridGeometry);
                            dir = (dir + rot) % 360;
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

    protected GridGeometry2D calculateSubgrid(GridGeometry2D dataGeometry) {
        try {
            CoordinateReferenceSystem dataCRS = dataGeometry
                    .getCoordinateReferenceSystem();
            org.opengis.geometry.Envelope descEnv = descriptor
                    .getGridGeometry().getEnvelope();
            Envelope2D dataEnv = dataGeometry.getEnvelope2D();
            int dataWidth = dataGeometry.getGridRange2D().width;
            int dataHeight = dataGeometry.getGridRange2D().height;
            /*
             * Use grid spacing to determine a threshold for
             * EnvelopeIntersection. This guarantees the result is within one
             * grid cell.
             */
            double dx = dataEnv.width / dataWidth;
            double dy = dataEnv.height / dataHeight;
            double threshold = Math.max(dx, dy);
            Geometry geom = EnvelopeIntersection.createEnvelopeIntersection(
                    descEnv, dataEnv, threshold, dataWidth, dataHeight);
            /* Convert from jts envelope to geotools envelope. */
            Envelope env = geom.getEnvelopeInternal();
            Envelope2D subEnv = new Envelope2D(dataCRS, env.getMinX(),
                    env.getMinY(), env.getWidth(), env.getHeight());
            GridEnvelope2D subRange = dataGeometry.worldToGrid(subEnv);
            /* Add a 1 pixel border so interpolation near the edges is nice */
            subRange.grow(1, 1);
            /* Make sure not to grow bigger than original grid. */
            subRange = new GridEnvelope2D(subRange.intersection(dataGeometry
                    .getGridRange2D()));
            if (subRange.isEmpty()) {
                return null;
            }
            return new GridGeometry2D(subRange, dataGeometry.getGridToCRS(),
                    dataCRS);
        } catch (FactoryException e) {
            /* Not a big deal, just request all data. */
            statusHandler.handle(Priority.DEBUG, "Unable to request subgrid.",
                    e);
        } catch (TransformException e) {
            /* Not a big deal, just request all data. */
            /* Java 7 multiple exception catches are going to be amazing! */
            statusHandler.handle(Priority.DEBUG, "Unable to request subgrid.",
                    e);
        }
        return dataGeometry;
    }

    public GeneralGridData reprojectData(GeneralGridData data) {
        if (descriptor == null) {
            return data;
        }
        try {
            GeneralGridGeometry targetGeometry = MapUtil.reprojectGeometry(data
                    .getGridGeometry(), descriptor.getGridGeometry()
                    .getEnvelope(), true, 2);
            dataModified = true;
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
            if (stylePreferences.getDisplayFlags() != null) {
                legendParams.isPlaneLabelDisplayed = !stylePreferences
                        .getDisplayFlags().hasFlag("NoPlane");
            }
        }

        if ((legendParams.unit == null) || legendParams.unit.isEmpty()) {
            if (record.getParameter().getUnit().equals(Unit.ONE)) {
                legendParams.unit = "";
            } else {
                legendParams.unit = record.getParameter().getUnitString();
            }
        }
        List<DisplayType> displayTypes = FieldDisplayTypesFactory.getInstance()
                .getDisplayTypes(record.getParameter().getAbbreviation());
        DisplayType displayType = getDisplayType();
        if ((displayTypes != null) && !displayTypes.isEmpty()
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
        if (resourceData.isSampling()) {
            if (getDisplayType() == DisplayType.ARROW) {
                Map<String, Object> map = interrogate(coord);
                if (map == null) {
                    return "NO DATA";
                }
                double value = (Double) map.get(INTERROGATE_VALUE);
                return sampleFormat.format(value) + map.get(INTERROGATE_UNIT);
            } else if (getDisplayType() == DisplayType.CONTOUR) {
                GridRecord record = getCurrentGridRecord();
                if (record != null) {
                    return record.getParameter().getAbbreviation() + "="
                            + super.inspect(coord);
                }
            }
        } else if (getDisplayType() != DisplayType.IMAGE) {
            return null;
        }
        return super.inspect(coord);
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
        if (dataModified) {
            clearRequestedData();
            dataModified = false;
        }
        super.project(crs);
    }

    @Override
    protected void resourceDataChanged(ChangeType type, Object updateObject) {
        super.resourceDataChanged(type, updateObject);
        if (type == ChangeType.CAPABILITY) {
            if ((updateObject instanceof ImagingCapability) && dataModified) {
                ImagingCapability capability = (ImagingCapability) updateObject;
                if ((lastInterpolationState == null)
                        || (capability.isInterpolationState() != lastInterpolationState)) {
                    lastInterpolationState = capability.isInterpolationState();
                    clearRequestedData();
                    dataModified = false;
                }
            }
        }
    }

}
