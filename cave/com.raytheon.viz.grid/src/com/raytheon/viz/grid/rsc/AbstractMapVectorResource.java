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

import java.io.FileNotFoundException;
import java.nio.FloatBuffer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.Unit;
import javax.media.jai.Interpolation;

import org.apache.commons.lang.Validate;
import org.eclipse.swt.graphics.RGB;
import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.coverage.grid.InvalidGridGeometryException;
import org.geotools.referencing.CRS;
import org.geotools.referencing.crs.DefaultGeographicCRS;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.edex.util.Util;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grib.CombinedGribRecord;
import com.raytheon.uf.common.dataplugin.grib.GribModel;
import com.raytheon.uf.common.dataplugin.grib.GribRecord;
import com.raytheon.uf.common.dataplugin.grib.spatial.projections.GridCoverage;
import com.raytheon.uf.common.dataplugin.grib.spatial.projections.LambertConformalGridCoverage;
import com.raytheon.uf.common.dataplugin.grib.spatial.projections.LatLonGridCoverage;
import com.raytheon.uf.common.dataplugin.grib.spatial.projections.MercatorGridCoverage;
import com.raytheon.uf.common.dataplugin.grib.spatial.projections.PolarStereoGridCoverage;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.interpolation.AbstractInterpolation;
import com.raytheon.uf.common.geospatial.interpolation.BilinearInterpolation;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.AbstractCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.DensityCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.uf.viz.core.style.StyleRule;
import com.raytheon.viz.core.contours.ContourRenderable;
import com.raytheon.viz.core.contours.rsc.displays.AbstractGriddedDisplay;
import com.raytheon.viz.core.contours.rsc.displays.GriddedVectorDisplay;
import com.raytheon.viz.core.rsc.ICombinedResourceData;
import com.raytheon.viz.core.rsc.ICombinedResourceData.CombineOperation;
import com.raytheon.viz.core.style.arrow.ArrowPreferences;
import com.raytheon.viz.core.style.contour.ContourPreferences;
import com.raytheon.viz.pointdata.PointWindDisplay.DisplayType;

/**
 * AbstractMapContourResource
 * 
 * An abstract class that provides contouring support for map-based resources
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Oct 22, 2007             chammack    Initial Creation.
 *    02/16/09                 njensen     Refactored to new rsc architecture
 *    06/25/10     #1691       bkowal      The data will now be retrieved for
 *                                         every frame initially instead of
 *                                         only retrieving data frame-by-frame
 *                                         as the user switches frames.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public abstract class AbstractMapVectorResource extends
        AbstractVizResource<AbstractRequestableResourceData, MapDescriptor> {
    protected static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractMapVectorResource.class);

    protected DisplayType displayType;

    private DataTime displayedDataTime;

    protected IGraphicsTarget lastTarget;

    protected boolean retrievedAllData = false;

    private Map<DataTime, PluginDataObject> dataObjectMap;

    protected String uuid;

    protected StyleRule styleRule;

    protected CombineOperation combineOperation;

    protected VectorContourRenderableManagerJob vcrManagerJob = null;

    protected GriddedDisplayManagerJob gdManagerJob = null;

    public class VectorContourRenderable extends ContourRenderable {

        private IDataRecord[] cache;

        private boolean dataRequested = false;

        private Exception exception = null;

        private final PluginDataObject pdo;

        private final StyleRule styleRule;

        public VectorContourRenderable(IMapDescriptor descriptor,
                PluginDataObject pdo, StyleRule styleRule) {
            super(descriptor);
            this.pdo = pdo;
            this.styleRule = styleRule;
        }

        @Override
        public IDataRecord[] getData() throws VizException {
            try {
                if (exception != null) {
                    throw exception;
                }
                if (cache == null && !dataRequested) {
                    dataRequested = true;
                    if (pdo instanceof CombinedGribRecord) {
                        cache = getCombinedDataRecord(
                                ((CombinedGribRecord) pdo), styleRule);
                    } else {
                        cache = getDataRecord(pdo, styleRule);
                    }
                    issueRefresh();
                }

                return cache;

            } catch (Throwable e) {
                throw new VizException("Error retrieving data for contouring",
                        e);
            }
        }

        @Override
        public GeneralGridGeometry getGridGeometry() throws VizException {
            return AbstractMapVectorResource.this.getGridGeometry(pdo);
        }

        @Override
        public ContourPreferences getPreferences() throws VizException {
            if (styleRule == null) {
                return null;
            }

            return (ContourPreferences) styleRule.getPreferences();
        }

        public boolean hasData() {
            return (cache != null);
        }
    }

    public class GriddedDisplayRequest {
        private FloatBuffer mag;

        private FloatBuffer dir;

        private final PluginDataObject pdo;

        private final StyleRule styleRule;

        public GriddedDisplayRequest(PluginDataObject pdo, StyleRule styleRule) {
            this.pdo = pdo;
            this.styleRule = styleRule;
        }

        public AbstractGriddedDisplay<?> getData() throws VizException {
            if (mag == null || dir == null) {
                try {
                    IDataRecord[] recs;

                    if (pdo instanceof CombinedGribRecord) {
                        recs = getCombinedDataRecord((CombinedGribRecord) pdo,
                                styleRule);
                    } else {
                        recs = getDataRecord(pdo, getStyleRule());
                    }
                    mag = FloatBuffer.wrap(((FloatDataRecord) recs[0])
                            .getFloatData());
                    dir = FloatBuffer.wrap(((FloatDataRecord) recs[1])
                            .getFloatData());
                    issueRefresh();
                } catch (Throwable e) {
                    throw new VizException("Error retrieving data for vector",
                            e);
                }
            }

            AbstractGriddedDisplay<?> display = null;
            if (AbstractMapVectorResource.this.displayType == DisplayType.ARROW
                    || AbstractMapVectorResource.this.displayType == DisplayType.BARB
                    || AbstractMapVectorResource.this.displayType == DisplayType.DUALARROW) {
                com.raytheon.uf.viz.core.rsc.DisplayType displayType = null;
                switch (AbstractMapVectorResource.this.displayType) {
                case ARROW:
                    displayType = com.raytheon.uf.viz.core.rsc.DisplayType.ARROW;
                    break;
                case BARB:
                    displayType = com.raytheon.uf.viz.core.rsc.DisplayType.BARB;
                    break;
                case DUALARROW:
                    displayType = com.raytheon.uf.viz.core.rsc.DisplayType.DUALARROW;
                    break;
                }
                GriddedVectorDisplay arrowDisplay = new GriddedVectorDisplay(
                        mag, dir, descriptor, getGridGeometry(pdo), 64,
                        displayType);

                display = arrowDisplay;
                if (styleRule != null
                        && styleRule.getPreferences() instanceof ArrowPreferences) {
                    arrowDisplay.setScale(((ArrowPreferences) styleRule
                            .getPreferences()).getScale());
                }
                arrowDisplay
                        .setLineStyle(getCapability(OutlineCapability.class)
                                .getLineStyle());
                arrowDisplay
                        .setLineWidth(getCapability(OutlineCapability.class)
                                .getOutlineWidth());
            } else {
                GriddedVectorDisplay vectorDisplay = new GriddedVectorDisplay(
                        mag, dir, descriptor,
                        AbstractMapVectorResource.this.getGridGeometry(pdo),
                        64, com.raytheon.uf.viz.core.rsc.DisplayType.STREAMLINE);

                display = vectorDisplay;
                if (styleRule != null
                        && styleRule.getPreferences() instanceof ArrowPreferences) {
                    vectorDisplay.setScale(((ArrowPreferences) styleRule
                            .getPreferences()).getScale());
                }
                vectorDisplay.setLineWidth(getCapability(
                        OutlineCapability.class).getOutlineWidth());
            }
            if (styleRule != null
                    && styleRule.getPreferences() instanceof ArrowPreferences) {
                if (display instanceof GriddedVectorDisplay) {
                    ((GriddedVectorDisplay) display)
                            .setScale(((ArrowPreferences) styleRule
                                    .getPreferences()).getScale());
                } else if (display instanceof GriddedVectorDisplay) {
                    ((GriddedVectorDisplay) display)
                            .setScale(((ArrowPreferences) styleRule
                                    .getPreferences()).getScale());
                }
            }
            display.setColor(getCapability(ColorableCapability.class)
                    .getColor());
            display.setDensity(getCapability(DensityCapability.class)
                    .getDensity());
            display.setMagnification(getCapability(
                    MagnificationCapability.class).getMagnification());
            return display;
        }

        public boolean hasData() {
            return (mag != null && dir != null);
        }
    }

    /**
     * Constructor
     */
    public AbstractMapVectorResource(AbstractRequestableResourceData data,
            LoadProperties props) {
        super(data, props);

        try {
            ICombinedResourceData combinedResourceData = (ICombinedResourceData) getResourceData();
            this.combineOperation = combinedResourceData.getCombineOperation();
        } catch (ClassCastException e) {
            // do nothing
        }

        this.vcrManagerJob = new VectorContourRenderableManagerJob();
        this.gdManagerJob = new GriddedDisplayManagerJob();

        setDataObjectMap(new HashMap<DataTime, PluginDataObject>());
        uuid = UUID.randomUUID().toString();
        resourceData.addChangeListener(new IResourceDataChanged() {
            @Override
            public void resourceChanged(ChangeType type, Object object) {
                if (type == ChangeType.CAPABILITY) {
                    if (!hasCapability(((AbstractCapability) object).getClass())
                            || getCapability(((AbstractCapability) object)
                                    .getClass()) != object) {
                        return;
                    }
                    if (object instanceof MagnificationCapability) {
                        Double mag = ((MagnificationCapability) object)
                                .getMagnification();

                        for (ContourRenderable display : vcrManagerJob
                                .getRequestMapAsCollection()) {
                            display.setMagnification(mag);
                        }
                        for (ContourRenderable display : vcrManagerJob
                                .getResponseMapAsCollection()) {
                            display.setMagnification(mag);
                        }
                        for (AbstractGriddedDisplay<?> display : gdManagerJob
                                .getResponseMapAsCollection()) {
                            display.setMagnification(mag);
                        }
                    } else if (object instanceof DensityCapability) {
                        Double density = ((DensityCapability) object)
                                .getDensity();

                        for (ContourRenderable display : vcrManagerJob
                                .getRequestMapAsCollection()) {
                            display.setDensity(density);
                        }
                        for (ContourRenderable display : vcrManagerJob
                                .getResponseMapAsCollection()) {
                            display.setDensity(density);
                        }
                        for (AbstractGriddedDisplay<?> display : gdManagerJob
                                .getResponseMapAsCollection()) {
                            display.setDensity(density);
                        }
                    } else if (object instanceof ColorableCapability) {
                        RGB color = ((ColorableCapability) object).getColor();

                        for (ContourRenderable display : vcrManagerJob
                                .getRequestMapAsCollection()) {
                            display.setColor(color);
                        }
                        for (ContourRenderable display : vcrManagerJob
                                .getResponseMapAsCollection()) {
                            display.setColor(color);
                        }
                        for (AbstractGriddedDisplay<?> display : gdManagerJob
                                .getResponseMapAsCollection()) {
                            display.setColor(color);
                        }
                    } else if (object instanceof OutlineCapability) {
                        int lineWidth = ((OutlineCapability) object)
                                .getOutlineWidth();
                        LineStyle lineStyle = ((OutlineCapability) object)
                                .getLineStyle();
                        for (ContourRenderable display : vcrManagerJob
                                .getRequestMapAsCollection()) {
                            display.setOutlineWidth(lineWidth);
                            display.setLineStyle(lineStyle);
                        }
                        for (ContourRenderable display : vcrManagerJob
                                .getResponseMapAsCollection()) {
                            display.setOutlineWidth(lineWidth);
                            display.setLineStyle(lineStyle);
                        }
                        for (AbstractGriddedDisplay<?> display : gdManagerJob
                                .getResponseMapAsCollection()) {
                            if (display instanceof GriddedVectorDisplay) {
                                ((GriddedVectorDisplay) display)
                                        .setLineWidth(lineWidth);
                                ((GriddedVectorDisplay) display)
                                        .setLineStyle(lineStyle);
                            }
                        }

                    }
                }

            }

        });
    }

    protected abstract StyleRule getStyleRule(PluginDataObject obj)
            throws VizException;

    /**
     * Validate the record
     * 
     * @param obj
     *            the record to validate
     * @return
     */
    protected abstract boolean validateRecord(PluginDataObject obj);

    /**
     * Get the grid geometry
     * 
     * @param obj
     * @return
     */
    protected abstract GeneralGridGeometry getGridGeometry(PluginDataObject obj);

    /**
     * Return the units of the data
     * 
     * @param obj
     *            the object
     * @return the unit
     */
    protected abstract Unit<?> getDataUnits(PluginDataObject obj);

    /**
     * Return the imagery resource
     */
    public abstract AbstractVizResource<AbstractRequestableResourceData, MapDescriptor> getImageryResource()
            throws VizException;

    /**
     * Retrieve the data record
     * 
     * @param obj
     * @param styleRule
     * @return
     * @throws FileNotFoundException
     * @throws StorageException
     * @throws VizException
     */
    protected synchronized IDataRecord[] getCombinedDataRecord(
            CombinedGribRecord pdo, StyleRule styleRule1)
            throws FileNotFoundException, StorageException, VizException {
        if (combineOperation == null
                || CombineOperation.NONE.equals(combineOperation)) {
            return getDataRecord(pdo.getPrimaryGribRecord(), styleRule1);
        }
        IDataRecord rec1 = null;
        IDataRecord rec2 = null;
        IDataRecord[] records1 = DataCubeContainer.getDataRecord(pdo
                .getPrimaryGribRecord());
        IDataRecord[] records2 = DataCubeContainer.getDataRecord(pdo
                .getSecondaryGribRecord());

        if (records1 != null && records1.length > 0) {
            rec1 = records1[0].clone();
        }
        if (records2 != null && records2.length > 0) {
            rec2 = records2[0].clone();
        }
        if (rec1 != null && rec1.getSizes().length != 2) {
            throw new VizException("Two dimensional space required to contour");
        }
        if (rec2 != null && rec2.getSizes().length != 2) {
            throw new VizException("Two dimensional space required to contour");
        }
        for (int i = 0; i < records1.length; i++) {
            records1[i] = getCombinedData((FloatDataRecord) records1[i],
                    (FloatDataRecord) records2[i], (CombinedGribRecord) pdo);
        }
        rec1 = records1[0];

        if (styleRule != null) {
            convertDataRecord(pdo, styleRule, rec1);
        }

        if ((this.displayType == DisplayType.STREAMLINE
                || this.displayType == DisplayType.ARROW
                || this.displayType == DisplayType.BARB || this.displayType == DisplayType.DUALARROW)
                && pdo instanceof GribRecord) {
            if (records1.length == 4) {
                if (this.displayType == DisplayType.STREAMLINE) {
                    return new IDataRecord[] { records1[2], records1[3] };
                } else {
                    float[] floatData = ((FloatDataRecord) records1[1])
                            .getFloatData();
                    // Need to ensure diff'd directions still in the 0-360 range
                    for (int i = 0; i < floatData.length; i++) {
                        if (floatData[i] < 0) {
                            floatData[i] = floatData[i] + 360;
                        }
                    }
                    return new IDataRecord[] { records1[0], records1[1] };
                }

            }
        }

        return new IDataRecord[] { rec1 };
    }

    /**
     * Retrieve the data record
     * 
     * @param obj
     * @param styleRule
     * @return
     * @throws FileNotFoundException
     * @throws StorageException
     * @throws VizException
     */
    protected IDataRecord[] getDataRecord(PluginDataObject pdo,
            StyleRule styleRule) throws FileNotFoundException,
            StorageException, VizException {

        IDataRecord rec = null;
        IDataRecord[] records = null;
        if (pdo instanceof GribRecord) {
            records = GridResourceData.getDataRecordsForTilt((GribRecord) pdo,
                    descriptor);
        }
        if (records == null) {
            records = DataCubeContainer.getDataRecord(pdo);
        }
        if (records != null && records.length > 0) {
            rec = records[0].clone();
        }
        if (rec != null && rec.getSizes().length != 2) {
            throw new VizException("Two dimensional space required to contour");
        }
        if (styleRule != null) {
            convertDataRecord(pdo, styleRule, rec);
        }

        if ((this.displayType == DisplayType.STREAMLINE
                || this.displayType == DisplayType.ARROW
                || this.displayType == DisplayType.BARB || this.displayType == DisplayType.DUALARROW)
                && pdo instanceof GribRecord) {
            if (records.length == 4) {
                if (this.displayType == DisplayType.STREAMLINE) {
                    return new IDataRecord[] { records[2], records[3] };
                } else {
                    return new IDataRecord[] { rec, records[1] };
                }

            }
        }

        return new IDataRecord[] { rec };
    }

    private void convertDataRecord(PluginDataObject pdo, StyleRule styleRule,
            IDataRecord rec) {
        Unit<?> desiredUnit = styleRule.getPreferences().getDisplayUnits();
        if (desiredUnit != null) {
            Unit<?> dataUnit = getDataUnits(pdo);
            if (dataUnit != null && !dataUnit.equals(Unit.ONE)) {
                if (!dataUnit.isCompatible(desiredUnit)) {
                    String message = "Data unit: " + dataUnit
                            + " cannot be converted to desired unit: "
                            + desiredUnit
                            + " Data displayed will be displayed with unit: "
                            + dataUnit;
                    statusHandler.handle(Priority.VERBOSE, message);
                } else {
                    UnitConverter converter = dataUnit
                            .getConverterTo(desiredUnit);
                    if (converter != null) {

                        if (rec instanceof FloatDataRecord) {
                            float[] fd = ((FloatDataRecord) rec).getFloatData();
                            for (int i = 0; i < fd.length; i++) {
                                if (fd[i] != Util.GRID_FILL_VALUE) {
                                    fd[i] = (float) converter.convert(fd[i]);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * @param pdo
     * @param rec
     * @return
     * @throws VizException
     * @throws StorageException
     * @throws FileNotFoundException
     */
    private synchronized IDataRecord getCombinedData(
            FloatDataRecord sourceDataRecord, FloatDataRecord targetDataRecord,
            CombinedGribRecord pdo) throws FileNotFoundException,
            StorageException, VizException {
        GribRecord pdo1 = pdo.getPrimaryGribRecord();
        GribRecord pdo2 = pdo.getSecondaryGribRecord();

        GribRecord record1 = (GribRecord) pdo1;
        GribRecord record2 = (GribRecord) pdo2;
        GridCoverage sourceCoverage = record1.getModelInfo().getLocation();
        GridCoverage targetCoverage = record2.getModelInfo().getLocation();
        Unit<?> primaryDataUnits = getDataUnits(pdo1);
        Unit<?> secondaryDataUnits = getDataUnits(pdo2);

        if (sourceCoverage.equals(targetCoverage)) {
            return diffData(sourceDataRecord, targetDataRecord,
                    primaryDataUnits, secondaryDataUnits);
        }
        try {
            GridGeometry2D newTarget = targetCoverage.getGridGeometry();
            double sourcePixeWidth = getPixelWidth(
                    sourceCoverage.getGridGeometry(), descriptor);
            double targetPixeWidth = getPixelWidth(
                    targetCoverage.getGridGeometry(), descriptor);
            if (sourcePixeWidth >= targetPixeWidth) {
                double sizeMultiplier = sourcePixeWidth / targetPixeWidth;
                sizeMultiplier = sizeMultiplier > 4 ? 2 : 4;
                long[] newSizes = {
                        (long) (targetDataRecord.getSizes()[0] * sizeMultiplier),
                        (long) (targetDataRecord.getSizes()[1] * sizeMultiplier) };
                newTarget = MapUtil.createFineIntersectingGeometry(
                        sourceCoverage.getGridGeometry().getEnvelope(),
                        targetCoverage.getGridGeometry().getEnvelope(),
                        newSizes);
            } else {
                double sizeMultiplier = targetPixeWidth / sourcePixeWidth;
                sizeMultiplier = sizeMultiplier > 4 ? 2 : 4;
                long[] newSizes = {
                        (long) (sourceDataRecord.getSizes()[0] * sizeMultiplier),
                        (long) (sourceDataRecord.getSizes()[1] * sizeMultiplier) };
                newTarget = MapUtil.createFineIntersectingGeometry(
                        targetCoverage.getGridGeometry().getEnvelope(),
                        sourceCoverage.getGridGeometry().getEnvelope(),
                        newSizes);
            }

            targetDataRecord = remapGrid(targetDataRecord, newTarget,
                    targetCoverage.getGridGeometry());
            sourceDataRecord = remapGrid(sourceDataRecord, newTarget,
                    sourceCoverage.getGridGeometry());
            GridCoverage coverage = null;
            if (sourceCoverage instanceof LambertConformalGridCoverage) {
                coverage = new LambertConformalGridCoverage(
                        (LambertConformalGridCoverage) sourceCoverage);
            } else if (sourceCoverage instanceof LatLonGridCoverage) {
                coverage = new LatLonGridCoverage(
                        (LatLonGridCoverage) sourceCoverage);
            } else if (sourceCoverage instanceof MercatorGridCoverage) {
                coverage = new MercatorGridCoverage(
                        (MercatorGridCoverage) sourceCoverage);
            } else if (sourceCoverage instanceof PolarStereoGridCoverage) {
                coverage = new PolarStereoGridCoverage(
                        (PolarStereoGridCoverage) sourceCoverage);
            }
            coverage.setGridGeometry(newTarget);
            GribModel modelInfo = new GribModel(pdo.getModelInfo());
            modelInfo.setLocation(coverage);
            pdo.setModelInfo(modelInfo);

            diffData(sourceDataRecord, targetDataRecord, primaryDataUnits,
                    secondaryDataUnits);
        } catch (Exception e) {
            throw new VizException(e);
        }

        return sourceDataRecord;
    }

    private FloatDataRecord remapGrid(FloatDataRecord targetDataRecord,
            GridGeometry2D newTarget, GeneralGridGeometry gridGeometry)
            throws FactoryException, TransformException {
        GridGeometry2D remappedImageGeometry = newTarget;
        AbstractInterpolation interp = new BilinearInterpolation(gridGeometry,
                remappedImageGeometry, -9998, Float.POSITIVE_INFINITY, -999999);
        float[] data = targetDataRecord.getFloatData();
        interp.setData(data);
        data = interp.getReprojectedGrid();
        FloatDataRecord remapGrid = (FloatDataRecord) targetDataRecord.clone();
        remapGrid.setIntSizes(new int[] {
                remappedImageGeometry.getGridRange2D().width,
                remappedImageGeometry.getGridRange2D().height });
        remapGrid.setFloatData(data);
        return remapGrid;
    }

    public double getPixelWidth(GridGeometry2D gridGeometry,
            IMapDescriptor descriptor) throws VizException {
        double pixelWidth = 0;
        try {
            double xCenter = gridGeometry.getGridRange().getSpan(0) / 2.0;
            double yCenter = gridGeometry.getGridRange().getSpan(1) / 2.0;

            double[] input = new double[] { xCenter, yCenter, xCenter + 1,
                    yCenter };
            double[] output = new double[input.length];

            MathTransform mathTransform = gridGeometry
                    .getGridToCRS(PixelInCell.CELL_CORNER);
            // convert the point s to lat/lon
            mathTransform.transform(input, 0, output, 0, input.length / 2);
            MathTransform localProjToLL = CRS.findMathTransform(
                    gridGeometry.getCoordinateReferenceSystem(),
                    DefaultGeographicCRS.WGS84);
            localProjToLL.transform(output, 0, output, 0, 2);

            double[] p1 = descriptor.worldToPixel(new double[] { output[0],
                    output[1] });
            double[] p2 = descriptor.worldToPixel(new double[] { output[2],
                    output[3] });

            // compute the number of map pixels per tile pixel
            pixelWidth = Math.abs(p2[0] - p1[0]);
        }

        catch (org.opengis.referencing.operation.NoninvertibleTransformException e) {
            throw new VizException(e);

        } catch (TransformException e) {
            throw new VizException(e);

        } catch (InvalidGridGeometryException e) {
            throw new VizException(e);

        } catch (FactoryException e) {
            throw new VizException(e);
        }
        return pixelWidth;
    }

    private synchronized FloatDataRecord diffData(
            FloatDataRecord primaryDataRecord,
            FloatDataRecord secondaryDataRecord, Unit<?> primaryDataUnits,
            Unit<?> secondaryDataUnits) {
        if (this.getStyleRule() != null) {
            UnitConverter primaryUnitConverter = primaryDataUnits
                    .getConverterTo(this.getStyleRule().getPreferences()
                            .getDisplayUnits());
            UnitConverter secondaryUnitConverter = secondaryDataUnits
                    .getConverterTo(this.getStyleRule().getPreferences()
                            .getDisplayUnits());

            for (int i = 0; i < secondaryDataRecord.getFloatData().length; i++) {

                float n1 = (float) primaryUnitConverter
                        .convert(primaryDataRecord.getFloatData()[i] == -999999 ? Float.NaN
                                : primaryDataRecord.getFloatData()[i]);
                // convert secondary data to primary
                float n2 = (float) secondaryUnitConverter
                        .convert(secondaryDataRecord.getFloatData()[i] == -999999 ? Float.NaN
                                : secondaryDataRecord.getFloatData()[i]);
                primaryDataRecord.getFloatData()[i] = (float) primaryUnitConverter
                        .inverse().convert(n1 - n2);
                if (Float.compare(primaryDataRecord.getFloatData()[i],
                        Float.NaN) == 0) {
                    primaryDataRecord.getFloatData()[i] = -999999;
                }

            }
        } else {
            for (int i = 0; i < secondaryDataRecord.getFloatData().length; i++) {

                float n1 = (float) primaryDataRecord.getFloatData()[i] == -999999 ? Float.NaN
                        : primaryDataRecord.getFloatData()[i];
                // convert secondary data to primary
                float n2 = (float) secondaryDataRecord.getFloatData()[i] == -999999 ? Float.NaN
                        : secondaryDataRecord.getFloatData()[i];
                primaryDataRecord.getFloatData()[i] = (n1 - n2);
                if (Float.compare(primaryDataRecord.getFloatData()[i],
                        Float.NaN) == 0) {
                    primaryDataRecord.getFloatData()[i] = -999999;
                }

            }
        }
        return primaryDataRecord;
    }

    /**
     * Remap the data in dataRecord from location to location2
     * 
     * This method is meant to be overriden by child classes, by default the
     * dataRecord will simply be returned without being re-mapped.
     * 
     * @param location
     * @param location2
     * @param dataRecord
     * @return a remapped dataRecord
     */
    protected FloatDataRecord remapGrid(GridCoverage location,
            GridCoverage location2, FloatDataRecord dataRecord,
            Interpolation interpolation) {
        return dataRecord;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#dispose()
     */
    @Override
    protected void disposeInternal() {
        if (this.vcrManagerJob != null) {
            this.vcrManagerJob.shutdown();
            this.vcrManagerJob.disposeInternal();
        }

        if (this.gdManagerJob != null) {
            this.gdManagerJob.shutdown();
            this.gdManagerJob.disposeInternal();
        }

        getDataObjectMap().clear();
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.viz.core.rsc.IVizResource#init(com.raytheon.viz.core.
     * IGraphicsTarget)
     */
    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        // Initialize capabilities for resources that aresn't painting
        getCapability(DensityCapability.class);
        getCapability(MagnificationCapability.class);
        getCapability(OutlineCapability.class);

        this.lastTarget = target;
    }

    /**
     * This method will retrieve all of the data for a set of contours for every
     * frame. This will ensure that CAVE will not display any empty frames for
     * one or a few iterations if the user begins looping through the frames.
     * 
     * @throws VizException
     */
    protected void getAllData() throws VizException {
        System.out.println("Retrieving All Data");

        this.vcrManagerJob.reset();
        this.gdManagerJob.reset();
        DataTime dataTime = null;
        Set<DataTime> keySet = this.getDataObjectMap().keySet();
        Iterator<DataTime> keySetItr = keySet.iterator();

        while (keySetItr.hasNext()) {
            dataTime = keySetItr.next();

            this.addJobRequest(dataTime);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.drawables.IRenderable#paint(com.raytheon.viz.core
     * .IGraphicsTarget, com.raytheon.viz.core.drawables.PaintProperties)
     */
    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        boolean useVCR = this.displayType != DisplayType.ARROW
                && this.displayType != DisplayType.BARB
                && this.displayType != DisplayType.DUALARROW;

        if (!this.retrievedAllData) {
            this.getAllData();
            this.retrievedAllData = true;
        } else {
            if (useVCR) {
                this.vcrManagerJob.makeHighestPriority(this.displayedDataTime);
            } else {
                this.gdManagerJob.makeHighestPriority(this.displayedDataTime);
            }
        }

        synchronized (this) {
            this.setDisplayedDataTime(paintProps.getDataTime());

            // Pull the record out
            PluginDataObject pdo = this.getDataObjectMap().get(
                    this.getDisplayedDataTime());

            if (pdo == null) {
                // Don't have data for this frame
                return;
            }

            if (this.getStyleRule() == null) {
                this.setStyleRule(this.getStyleRule(pdo));
            }

            if (useVCR) {
                ContourRenderable contourGroup = this.vcrManagerJob
                        .request(this.getDisplayedDataTime());

                if (contourGroup != null) {
                    if (combineOperation != CombineOperation.NONE) {
                        contourGroup.paint(target, paintProps);
                    }
                }
            } else {
                AbstractGriddedDisplay<?> display = this.gdManagerJob
                        .request(this.getDisplayedDataTime());

                if (display != null) {
                    if (combineOperation != CombineOperation.NONE) {
                        display.paint(target, paintProps);
                    }
                }
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.rsc.capabilities.IProjectableResource#project(org
     * .opengis.referencing.crs.CoordinateReferenceSystem)
     */
    @Override
    public void project(CoordinateReferenceSystem mapData) throws VizException {
        if (gdManagerJob != null) {
            for (AbstractGriddedDisplay<?> display : gdManagerJob
                    .getResponseMapAsCollection()) {
                if (display != null) {
                    display.reproject();
                }
            }
        }
        initInternal(this.lastTarget);
        this.retrievedAllData = false;
        if (vcrManagerJob != null) {
            vcrManagerJob.shutdown();
            vcrManagerJob.clearResponseMap();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#remove(com.raytheon.
     * uf.common.time.DataTime)
     */
    @Override
    public void remove(DataTime dataTime) {
        this.getDataObjectMap().remove(dataTime);
        this.vcrManagerJob.removeDataTime(dataTime);
        this.gdManagerJob.removeDataTime(dataTime);
        recreateDataTimes();
    }

    /**
     * Recreate all the datatimes from the objects
     */
    protected void recreateDataTimes() {
        this.dataTimes = new ArrayList<DataTime>(getDataObjectMap().size());
        dataTimes.addAll(this.getDataObjectMap().keySet());
    }

    public void addRecord(PluginDataObject record) throws VizException {
        Validate.notNull(record);
        boolean good = validateRecord(record);
        if (!good) {
            throw new IllegalArgumentException(
                    "Wrong type of data provided to "
                            + this.getClass().getName() + " :: "
                            + record.getClass().getName());
        }
        DataTime dt = record.getDataTime();
        getDataObjectMap().put(dt, record);

        if (this.retrievedAllData) {
            this.addJobRequest(dt);
        }

        recreateDataTimes();
        if (this.getDisplayedDataTime() == null) {
            this.setDisplayedDataTime(dt);
        }
    }

    public void addJobRequest(DataTime dt) throws VizException {
        PluginDataObject pdo = this.getDataObjectMap().get(dt);
        if (pdo == null) {
            return;
        }

        StyleRule sr = getStyleRule();
        if (sr == null) {
            sr = getStyleRule(pdo);
            if (sr != null) {
                this.setStyleRule(sr);
            }
        }

        if (displayType == DisplayType.ARROW || displayType == DisplayType.BARB
                || displayType == DisplayType.DUALARROW) {
            GriddedDisplayRequest req = new GriddedDisplayRequest(pdo, sr);
            this.gdManagerJob.addRequest(dt, req);
        } else {
            VectorContourRenderable cr = new VectorContourRenderable(
                    this.descriptor, pdo, sr);
            cr.setColor(this.getCapability(ColorableCapability.class)
                    .getColor());
            cr.setLineStyle(this.getCapability(OutlineCapability.class)
                    .getLineStyle());
            cr.setOutlineWidth(this.getCapability(OutlineCapability.class)
                    .getOutlineWidth());
            cr.setDensity(getCapability(DensityCapability.class).getDensity());
            cr.setMagnification(getCapability(MagnificationCapability.class)
                    .getMagnification());
            this.vcrManagerJob.addRequest(dt, cr);
        }
    }

    public synchronized StyleRule getStyleRule() {
        return styleRule;
    }

    public synchronized void setStyleRule(StyleRule styleRule) {
        this.styleRule = styleRule;
    }

    public synchronized Map<DataTime, PluginDataObject> getDataObjectMap() {
        return dataObjectMap;
    }

    /**
     * @param dataObjectMap
     *            the dataObjectMap to set
     */
    protected synchronized void setDataObjectMap(
            Map<DataTime, PluginDataObject> dataObjectMap) {
        this.dataObjectMap = dataObjectMap;
    }

    public synchronized DataTime getDisplayedDataTime() {
        return displayedDataTime;
    }

    /**
     * @param displayedDataTime
     *            the displayedDataTime to set
     */
    protected synchronized void setDisplayedDataTime(DataTime displayedDataTime) {
        this.displayedDataTime = displayedDataTime;
    }
}
