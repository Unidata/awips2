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
package com.raytheon.viz.grid.rsc;

import java.awt.Point;
import java.io.FileNotFoundException;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.Set;

import javax.measure.unit.Unit;
import javax.media.jai.Interpolation;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.coverage.grid.InvalidGridGeometryException;
import org.geotools.geometry.DirectPosition2D;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grib.CombinedGribRecord;
import com.raytheon.uf.common.dataplugin.grib.GribModel;
import com.raytheon.uf.common.dataplugin.grib.GribRecord;
import com.raytheon.uf.common.dataplugin.grib.spatial.projections.GridCoverage;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.PointUtil;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.geospatial.interpolation.AbstractInterpolation;
import com.raytheon.uf.common.geospatial.interpolation.BilinearInterpolation;
import com.raytheon.uf.common.geospatial.interpolation.NearestNeighborInterpolation;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.style.ParamLevelMatchCriteria;
import com.raytheon.uf.viz.core.style.StyleManager;
import com.raytheon.uf.viz.core.style.StyleManager.StyleType;
import com.raytheon.uf.viz.core.style.StyleRule;
import com.raytheon.viz.core.contours.ContourRenderable;
import com.raytheon.viz.core.contours.rsc.displays.AbstractGriddedDisplay;
import com.raytheon.viz.core.contours.rsc.displays.GriddedVectorDisplay;
import com.raytheon.viz.core.rsc.ICombinedResourceData.CombineOperation;
import com.raytheon.viz.core.rsc.ICombinedResourceData.CombineUtil;
import com.raytheon.viz.grid.GridLevelTranslator;
import com.raytheon.viz.grid.rsc.GridNameGenerator.IGridNameResource;
import com.raytheon.viz.grid.rsc.GridNameGenerator.LegendParameters;
import com.raytheon.viz.grid.util.CoverageUtils;
import com.raytheon.viz.grid.util.RemappedImage;
import com.raytheon.viz.pointdata.PointWindDisplay.DisplayType;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * GridContourResource
 * 
 * Implements contouring for grid data
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Oct 23, 2007             chammack    Initial Creation.
 *    02/16/09                 njensen     Refactored to new rsc architecture
 *    01/07/11     7948        bkowal      wind direction will be displayed in
 *                                         addition to wind speed when sampling
 *                                         is on.
 *    01/31/12   14306        kshresth     Cursor readout as you sample the dispay
 *    02/10/12     14472       mhuang      Fixed VB 'Height' field legend display error
 *                                         when click 'Diff' button.
 *    05/08/2012   14828       D. Friedman Use nearest-neighbor interpolation for
 *                                         reprojected grids.
 *    05/16/2012   14993       D. Friedman Fix "blocky" contours  
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class GridVectorResource extends AbstractMapVectorResource implements
        IResourceDataChanged, IGridNameResource {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GridVectorResource.class);

    private GridGeometry2D remappedImageGeometry;

    private LegendParameters legendParams;

    public GridVectorResource(GridResourceData data, LoadProperties props) {
        super(data, props);

        data.addChangeListener(this);
        GribRecord emptyRecord = new GribRecord();
        for (GribRecord rec : data.getRecords()) {
            try {
                // don't add empty records
                if (!emptyRecord.equals(rec)) {
                    this.addRecord(rec);
                    Collections.sort(this.dataTimes);
                }
            } catch (VizException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
        if (resourceData.getNameGenerator() == null) {
            resourceData.setNameGenerator(new GridNameGenerator());
        }
    }

    @Override
    public String getName() {

        if (combineOperation != null
                && combineOperation != CombineOperation.NONE) {
            return getCombinedName();
        } else {
            return super.getName();
        }

    }

    private String getCombinedName() {
        GridNameGenerator secondaryGenerator = (GridNameGenerator) ((GridResourceData) resourceData).secondaryResourceData
                .getNameGenerator();
        if (secondaryGenerator == null) {
            secondaryGenerator = new GridNameGenerator();
        }
        String secondaryName;
        try {
            LegendParameters legendParams = new LegendParameters();
            GribRecord secondaryGribRecord = ((CombinedGribRecord) ((GridResourceData) resourceData).records[0])
                    .getSecondaryGribRecord();
            GribModel modelInfo = secondaryGribRecord.getModelInfo();
            modelInfo.getParameterAbbreviation();
            String secondaryUnits = modelInfo.getParameterUnit();
            ParamLevelMatchCriteria match = new ParamLevelMatchCriteria();
            match.setLevel(GridLevelTranslator
                    .constructMatching(secondaryGribRecord));
            match.setParameterName(Arrays.asList(modelInfo
                    .getParameterAbbreviation()));
            match.setCreatingEntityNames(Arrays.asList(modelInfo.getModelName()));
            String parameterName = modelInfo.getParameterName();
            StyleType st = null;
            if (parameterName.equals("Height")) {
                st = StyleType.CONTOUR;
            } else if (parameterName.equals("Wind")
                    || parameterName.equals("Total Wind")
                    || parameterName.equals("Total Wind (Vector)")) {
                st = StyleType.ARROW;
            } else {
                st = StyleType.IMAGERY;
            }
            StyleRule secondaryStyleRule = StyleManager.getInstance()
                    .getStyleRule(st, match);
            if (secondaryStyleRule != null
                    && secondaryStyleRule.getPreferences()
                            .getDisplayUnitLabel() != null) {
                secondaryUnits = secondaryStyleRule.getPreferences()
                        .getDisplayUnitLabel();
            }
            legendParams.model = modelInfo;
            legendParams.unit = secondaryUnits;
            legendParams.dataTime = getDisplayedDataTime();
            secondaryName = secondaryGenerator.getName(legendParams,
                    ((GridResourceData) resourceData).secondaryResourceData);
        } catch (Exception e) {
            return null;
        }

        return CombineUtil.getName(super.getName(), secondaryName,
                combineOperation);
    }

    public GridVectorResource(GridResourceData data, LoadProperties props,
            DisplayType type) {
        this(data, props);
        displayType = type;
    }

    @Override
    protected IDataRecord[] getDataRecord(PluginDataObject pdo,
            StyleRule styleRule) throws FileNotFoundException,
            StorageException, VizException {
        IDataRecord[] results = super.getDataRecord(pdo, styleRule);
        GribRecord gribRecord = (GribRecord) pdo;

        // We need to reproject global data to prevent a gap in the data
        boolean reproject = false;
        GridCoverage location = gribRecord.getModelInfo().getLocation();
        if (location != null && location.getSpacingUnit().equals("degree")) {
            double dx = location.getDx();
            Integer nx = location.getNx();
            if (dx * nx >= 360) {
                reproject = true;
            }
        }
        if (reproject == true) {
            GeneralGridGeometry gridGeometry = location.getGridGeometry();

            try {

                GridGeometry2D remappedImageGeometry = GridGeometry2D
                        .wrap(MapUtil.reprojectGeometry(gridGeometry,
                                descriptor.getGridGeometry().getEnvelope(),
                                true, 2));
                IDataRecord[] newData = new IDataRecord[results.length];
                BilinearInterpolation interp = new BilinearInterpolation(
                        gridGeometry, remappedImageGeometry, -9998,
                        Float.POSITIVE_INFINITY, -999999);
                interp.setMissingThreshold(1.0f);

                /*
                 * Convert speed/dirs into U, V before interpolation.
                 */
                int len = ((FloatDataRecord) results[0]).getFloatData().length;
                float[] uu = new float[len];
                float[] vv = new float[len];

                boolean isVector = false;
                if (displayType == DisplayType.BARB
                        || displayType == DisplayType.ARROW) {
                    isVector = true;

                    for (int i = 0; i < len; i++) {
                        float spd = ((FloatDataRecord) results[0])
                                .getFloatData()[i];
                        float dir = ((FloatDataRecord) results[1])
                                .getFloatData()[i];

                        if (spd > -999999.0f && dir > -999999.0f) {
                            uu[i] = (float) (-spd * Math.sin(dir * Math.PI
                                    / 180));
                            vv[i] = (float) (-spd * Math.cos(dir * Math.PI
                                    / 180));
                        } else {
                            uu[i] = -999999.0f;
                            vv[i] = -999999.0f;
                        }
                    }
                }

                for (int i = 0; i < results.length; i++) {
                    if (results[i] instanceof FloatDataRecord) {
                        float[] data = new float[len];
                        if (isVector) {
                            data = i == 0 ? uu : vv;
                        } else {
                            data = ((FloatDataRecord) results[i])
                                    .getFloatData();
                        }

                        interp.setData(data);
                        data = interp.getReprojectedGrid();
                        newData[i] = results[i].clone();
                        newData[i]
                                .setIntSizes(new int[] {
                                        remappedImageGeometry.getGridRange2D().width,
                                        remappedImageGeometry.getGridRange2D().height });
                        ((FloatDataRecord) newData[i]).setFloatData(data);
                    }
                }
                uu = null;
                vv = null;

                if (isVector) {
                    /*
                     * Convert U, V back to speed/dirs
                     */
                    len = ((FloatDataRecord) newData[0]).getFloatData().length;
                    float[] new_spds = new float[len];
                    float[] new_dirs = new float[len];
                    for (int i = 0; i < len; i++) {
                        float u = ((FloatDataRecord) newData[0]).getFloatData()[i];
                        float v = ((FloatDataRecord) newData[1]).getFloatData()[i];

                        if (u > -999999.0f && v > -999999.0f) {
                            new_spds[i] = (float) Math.hypot(u, v);
                            new_dirs[i] = (float) (Math.atan2(u, v) * 180 / Math.PI) + 180;

                            if (new_dirs[i] > 360)
                                new_dirs[i] -= 360;
                            if (new_dirs[i] < 0)
                                new_dirs[i] += 360;

                        } else {
                            new_spds[i] = new_dirs[i] = -999999.0f;
                        }
                    }
                    ((FloatDataRecord) newData[0]).setFloatData(new_spds);
                    new_spds = null;

                    // When reprojecting it is necessary to recalculate the
                    // direction of vectors based off the change in the "up"
                    // direction
                    MathTransform grid2crs = remappedImageGeometry
                            .getGridToCRS();
                    MathTransform crs2ll = MapUtil
                            .getTransformToLatLon(remappedImageGeometry
                                    .getCoordinateReferenceSystem());

                    for (int i = 0; i < remappedImageGeometry.getGridRange2D().width; i++) {
                        for (int j = 0; j < remappedImageGeometry
                                .getGridRange2D().height; j++) {
                            int index = i
                                    + j
                                    * remappedImageGeometry.getGridRange2D().width;
                            if (new_dirs[index] > -9999) {
                                DirectPosition2D dp = new DirectPosition2D(i, j);
                                grid2crs.transform(dp, dp);
                                crs2ll.transform(dp, dp);
                                Coordinate ll = new Coordinate(dp.x, dp.y);
                                double rot = MapUtil.rotation(ll,
                                        remappedImageGeometry);
                                double rot2 = MapUtil.rotation(ll,
                                        GridGeometry2D.wrap(gridGeometry));
                                new_dirs[index] -= rot += rot2;
                            }
                        }
                    }

                    ((FloatDataRecord) newData[1]).setFloatData(new_dirs);
                    new_dirs = null;

                }
                this.remappedImageGeometry = remappedImageGeometry;
                results = newData;
            } catch (FactoryException e) {
                throw new VizException(e);
            } catch (TransformException e) {
                throw new VizException(e);
            }

        }
        if (styleRule != null
                && styleRule.getPreferences().getDisplayFlags()
                        .hasFlag("RotateVectorDir")) {
            // Rotate the direction of contours to be relative to the North pole
            // instead of the up direction of the grid
            if (results.length == 1 && results[0] instanceof FloatDataRecord) {
                FloatDataRecord oldRec = (FloatDataRecord) results[0];
                FloatDataRecord newRec = (FloatDataRecord) oldRec.clone();

                GridGeometry2D geom = GridGeometry2D.wrap(location
                        .getGridGeometry());
                MathTransform grid2crs = geom.getGridToCRS();
                try {
                    MathTransform crs2ll = MapUtil.getTransformToLatLon(geom
                            .getCoordinateReferenceSystem());
                    for (int i = 0; i < geom.getGridRange2D().width; i++) {
                        for (int j = 0; j < geom.getGridRange2D().height; j++) {
                            int index = i + j * geom.getGridRange2D().width;
                            float dir = newRec.getFloatData()[index];
                            if (dir > -9999) {
                                DirectPosition2D dp = new DirectPosition2D(i, j);
                                grid2crs.transform(dp, dp);
                                crs2ll.transform(dp, dp);
                                Coordinate ll = new Coordinate(dp.x, dp.y);
                                float rot = (float) MapUtil.rotation(ll, geom);
                                dir = (dir + rot + 180) % 360;
                                newRec.getFloatData()[index] = dir;
                            }
                        }
                    }
                    results = new FloatDataRecord[] { newRec };
                } catch (TransformException e) {
                    throw new VizException(e);
                } catch (InvalidGridGeometryException e) {
                    throw new VizException(e);
                } catch (FactoryException e) {
                    throw new VizException(e);
                }
            }
        }
        return results;
    }

    @Override
    protected void getAllData() throws VizException {
        System.out.println("Retrieving All Data");

        this.vcrManagerJob.reset();
        this.gdManagerJob.reset();

        Set<DataTime> keySet = this.getDataObjectMap().keySet();
        Iterator<DataTime> keySetItr = keySet.iterator();
        GribRecord emptyRecord = new GribRecord();
        while (keySetItr.hasNext()) {
            DataTime dataTime = keySetItr.next();
            GribRecord pdo = (GribRecord) this.getDataObjectMap().get(dataTime);
            if (emptyRecord.equals(pdo)) {
                // don't request for empty records
                continue;
            }
            this.addJobRequest(dataTime);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.contours.AbstractMapContourResource#getGridGeometry
     * (com.raytheon.edex.db.objects.PluginDataObject)
     */
    @Override
    protected GeneralGridGeometry getGridGeometry(PluginDataObject obj) {
        GribRecord gribRecord = (GribRecord) obj;
        if (remappedImageGeometry != null) {
            return remappedImageGeometry;
        }
        return gribRecord.getModelInfo().getLocation().getGridGeometry();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.contours.AbstractMapContourResource#validateRecord
     * (com.raytheon.edex.db.objects.PluginDataObject)
     */
    @Override
    protected boolean validateRecord(PluginDataObject obj) {
        return (obj instanceof GribRecord);
    }

    public LegendParameters getLegendParameters() {
        setDisplayedDataTime(descriptor.getTimeForResource(this));
        PluginDataObject pdo = getDataObjectMap().get(getDisplayedDataTime());
        if (pdo == null || pdo.equals(new GribRecord())) {
            ((GridResourceData) resourceData).getModelInfo();
            if (legendParams == null) {
                legendParams = new LegendParameters();
            }
            legendParams.model = ((GridResourceData) resourceData)
                    .getModelInfo();
            legendParams.dataTime = getDisplayedDataTime();

            if (displayType == DisplayType.STREAMLINE) {
                legendParams.type = " Streamlines";
            } else if (displayType == DisplayType.BARB) {
                legendParams.type = "Wind Barbs";
            } else if (displayType == DisplayType.ARROW) {
                legendParams.type = "Arrows";
            }
            return legendParams;
        }
        GribRecord record = (GribRecord) pdo;
        if (legendParams == null) {
            legendParams = new LegendParameters();
            try {
                StyleRule styleRule = getStyleRule(pdo);
                if (styleRule != null
                        && styleRule.getPreferences().getDisplayUnits() != null
                        && (styleRule.getPreferences()).getDisplayUnits()
                                .toString() != null) {
                    legendParams.isPlaneLabelDisplayed = !styleRule
                            .getPreferences().getDisplayFlags()
                            .hasFlag("NoPlane");
                    legendParams.unit = styleRule.getPreferences()
                            .getDisplayUnitLabel();
                } else {
                    legendParams.unit = record.getModelInfo()
                            .getParameterUnit();
                }
            } catch (VizException e) {
                statusHandler.handle(Priority.VERBOSE, e.getLocalizedMessage(),
                        e);
            }
        }
        legendParams.model = record.getModelInfo();
        legendParams.dataTime = getDisplayedDataTime();

        if (displayType == DisplayType.STREAMLINE) {
            legendParams.type = " Streamlines";
        } else if (displayType == DisplayType.BARB) {
            legendParams.type = "Wind Barbs";
        } else if (displayType == DisplayType.ARROW) {
            legendParams.type = "Arrows";
        }
        return legendParams;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.contours.rsc.AbstractMapContourResource#getStyleRule
     * (com.raytheon.edex.db.objects.PluginDataObject)
     */
    @Override
    public synchronized StyleRule getStyleRule(PluginDataObject obj)
            throws VizException {
        GribRecord record = (GribRecord) obj;

        com.raytheon.uf.viz.core.style.level.Level level = GridLevelTranslator
                .constructMatching(record);

        if (level == null) {
            throw new VizException("Unhandled layer type: "
                    + record.getModelInfo().getLevelName());
        }

        ParamLevelMatchCriteria match = new ParamLevelMatchCriteria();
        match.setLevel(level);
        match.setParameterName(Arrays.asList(record.getModelInfo()
                .getParameterAbbreviation()));
        match.setCreatingEntityNames(Arrays.asList(record.getModelInfo()
                .getModelName()));
        StyleRule sr = null;
        if (displayType == DisplayType.ARROW || displayType == DisplayType.BARB
                || displayType == DisplayType.DUALARROW) {
            sr = StyleManager.getInstance().getStyleRule(
                    StyleManager.StyleType.ARROW, match);
        } else {
            sr = StyleManager.getInstance().getStyleRule(
                    StyleManager.StyleType.CONTOUR, match);
        }
        return sr;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.contours.rsc.AbstractMapContourResource#getDataUnits
     * (com.raytheon.edex.db.objects.PluginDataObject)
     */
    @Override
    protected Unit<?> getDataUnits(PluginDataObject obj) {
        return ((GribRecord) obj).getModelInfo().getParameterUnitObject();
    }

    @SuppressWarnings("unchecked")
    @Override
    public AbstractVizResource<AbstractRequestableResourceData, MapDescriptor> getImageryResource()
            throws VizException {
        Collection<PluginDataObject> pdo = getDataObjectMap().values();
        GribRecord[] recs = new GribRecord[pdo.size()];
        Iterator<PluginDataObject> pdoIter = pdo.iterator();
        int i = 0;
        while (pdoIter.hasNext()) {
            recs[i] = (GribRecord) pdoIter.next();
            i++;
        }

        return (AbstractVizResource<AbstractRequestableResourceData, MapDescriptor>) resourceData
                .construct(new GridLoadProperties(
                        com.raytheon.uf.viz.core.rsc.DisplayType.IMAGE),
                        descriptor);
    }

    @SuppressWarnings("unchecked")
    @Override
    public AbstractVizResource<AbstractRequestableResourceData, MapDescriptor> getStreamlineResource()
            throws VizException {
        return (AbstractVizResource<AbstractRequestableResourceData, MapDescriptor>) resourceData
                .construct(new GridLoadProperties(
                        com.raytheon.uf.viz.core.rsc.DisplayType.STREAMLINE),
                        descriptor);
    }

    @Override
    public boolean isStreamlineVector() {
        return false;
    }

    @SuppressWarnings("unchecked")
    @Override
    public AbstractVizResource<AbstractRequestableResourceData, MapDescriptor> getWindBarbResource()
            throws VizException {
        return (AbstractVizResource<AbstractRequestableResourceData, MapDescriptor>) resourceData
                .construct(new GridLoadProperties(
                        com.raytheon.uf.viz.core.rsc.DisplayType.BARB),
                        descriptor);
    }

    @Override
    public boolean isWindVector() {
        return false;
    }

    @SuppressWarnings("unchecked")
    @Override
    public AbstractVizResource<AbstractRequestableResourceData, MapDescriptor> getArrowResource()
            throws VizException {
        return (AbstractVizResource<AbstractRequestableResourceData, MapDescriptor>) resourceData
                .construct(new GridLoadProperties(
                        com.raytheon.uf.viz.core.rsc.DisplayType.ARROW),
                        descriptor);
    }

    @Override
    public boolean isArrowVector() {
        return false;
    }

    @Override
    protected FloatDataRecord remapGrid(GridCoverage location,
            GridCoverage location2, FloatDataRecord dataRecord,
            Interpolation interpolation) {
        try {
            RemappedImage remappedImage = CoverageUtils.getInstance()
                    .remapGrid(location, location2, dataRecord, interpolation);
            FloatDataRecord remapGrid = remappedImage.getFloatDataRecord();
            this.setRemappedImageGeometry(remappedImage.getGridGeometry());
            return remapGrid;
        } catch (VizException e) {
            throw new RuntimeException(
                    "Unable to remap secondary resource grid data from "
                            + location.getName() + " to" + location2.getName(),
                    e);
        }
    }

    private void setRemappedImageGeometry(GridGeometry2D remappedImageGeometry) {
        this.remappedImageGeometry = remappedImageGeometry;
    }

    @Override
    public void resourceChanged(ChangeType type, Object object) {
        if (type.equals(ChangeType.DATA_UPDATE)) {
            PluginDataObject[] pdos = (PluginDataObject[]) object;

            for (PluginDataObject pdo : pdos) {
                if (CombineOperation.DIFFERENCE.equals(combineOperation)
                        && !(pdo instanceof CombinedGribRecord)) {
                    // Do nothing, timematcher will take care of it.
                } else {
                    try {
                        if (pdo != null) {
                            addRecord(pdo);
                        }
                    } catch (VizException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                "Error updating grid resource", e);
                    }
                }
            }
        }
        issueRefresh();
    }

    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        if (!((GridResourceData) resourceData).isSampling()) {
            if (displayType != DisplayType.ARROW) {
                return super.inspect(coord);
            }
        }
        GribRecord record = (GribRecord) getDataObjectMap().get(
                getDisplayedDataTime());
        // If we have no record then there is no data
        if (record == null) {
            return "No Data";
        }

        ISpatialObject spatialArea = record.getSpatialObject();
        GridGeometry2D dataGeom = MapUtil.getGridGeometry(spatialArea);
        String extension = "";
        Number value = null;
        try {
            if (displayType == DisplayType.ARROW
                    || displayType == DisplayType.BARB
                    || displayType == DisplayType.DUALARROW) {
                Point position = PointUtil.determineIndex(coord.asLatLon(),
                        spatialArea.getCrs(), dataGeom);
                // Outside the grid boundaries
                if (position.y < 0 || position.y >= spatialArea.getNy()
                        || position.x < 0 || position.x >= spatialArea.getNx()) {
                    return "No Data";
                }
                int index = position.y * spatialArea.getNx() + position.x;
                Number direction = null;

                AbstractGriddedDisplay<?> vectorDisplay = gdManagerJob
                        .request(getDisplayedDataTime());
                if (vectorDisplay != null
                        && vectorDisplay instanceof GriddedVectorDisplay) {
                    value = ((GriddedVectorDisplay) vectorDisplay)
                            .getMagnitude().get(index);
                    direction = ((GriddedVectorDisplay) vectorDisplay)
                            .getDirection().get(index);
                    if (direction != null
                            && !Float.isNaN(direction.floatValue())) {
                        /* For Wind Direction In Degrees */
                        extension = String.format("%.0f\u00B0 ", direction);
                    }
                }
            } else {
                ContourRenderable contourGroup;
                contourGroup = this.vcrManagerJob.request(this
                        .getDisplayedDataTime());
                if (contourGroup == null || contourGroup.getData() == null) {
                    return "No Data";
                }

                AbstractInterpolation interp = new BilinearInterpolation(
                        ((FloatDataRecord) contourGroup.getData()[0])
                                .getFloatData(),
                        dataGeom, descriptor.getGridGeometry(), -9998f,
                        Float.POSITIVE_INFINITY, Float.NaN);
                Coordinate pixel = coord.asPixel(descriptor.getGridGeometry());
                value = interp.getReprojectedGridCell((int) pixel.x,
                        (int) pixel.y);

            }
            // No data here
            if (value == null || value.intValue() <= -9999) {
                return "No Data";
            }
            // Everything is good
            String unitString = legendParams.unit == null ? record
                    .getModelInfo().getParameterUnit() : legendParams.unit;
            if (value.intValue() < 99 || value.intValue() > 99999) {
                return String.format("%s = %s%.3g%s", record.getModelInfo()
                        .getParameterAbbreviation(), extension, value,
                        unitString);
            } else if (value.intValue() < 999) {
                return String.format("%s = %s%.1f%s", record.getModelInfo()
                        .getParameterAbbreviation(), extension, value,
                        unitString);
            } else {
                return String.format("%s = %s%.0f%s", record.getModelInfo()
                        .getParameterAbbreviation(), extension, value,
                        unitString);
            }
        } catch (Exception e) {
            throw new VizException("Error occured during Sampling", e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.contours.rsc.AbstractMapVectorResource#project(
     * org.opengis.referencing.crs.CoordinateReferenceSystem)
     */
    @Override
    public void project(CoordinateReferenceSystem mapData) throws VizException {
        remappedImageGeometry = null;
        super.project(mapData);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.contours.ILoadableAsImage#isLoadableAsImage()
     */
    @Override
    public boolean isLoadableAsImage() {
        return displayType == null;
    }

}
