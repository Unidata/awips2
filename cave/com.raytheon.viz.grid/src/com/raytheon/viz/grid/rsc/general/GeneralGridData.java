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

import java.awt.geom.Rectangle2D;
import java.nio.FloatBuffer;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.Unit;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridEnvelope2D;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.DirectPosition2D;
import org.geotools.geometry.Envelope2D;
import org.opengis.coverage.grid.GridEnvelope;
import org.opengis.geometry.Envelope;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.data.GeographicDataSource;
import com.raytheon.uf.common.geospatial.data.UnitConvertingDataFilter;
import com.raytheon.uf.common.geospatial.interpolation.GridReprojection;
import com.raytheon.uf.common.geospatial.interpolation.GridReprojectionDataSource;
import com.raytheon.uf.common.geospatial.interpolation.GridSampler;
import com.raytheon.uf.common.geospatial.interpolation.Interpolation;
import com.raytheon.uf.common.geospatial.interpolation.PrecomputedGridReprojection;
import com.raytheon.uf.common.numeric.buffer.FloatBufferWrapper;
import com.raytheon.uf.common.numeric.source.DataSource;
import com.raytheon.uf.common.numeric.source.OffsetDataSource;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * A class which holds data for a grid. Includes FloatBuffers for holding scalar
 * or vector data as well as the dataUnits.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- -----------------------------------------
 * Mar 09, 2011           bsteffen    Initial creation
 * Jul 17, 2013  2185     bsteffen    Cache computed grid reprojections.
 * Aug 27, 2013  2287     randerso    Removed 180 degree adjustment required by
 *                                    error in Maputil.rotation
 * Dec 09, 2013  2617     bsteffen    Added 180 degree rotation into reproject
 *                                    so wind direction is calculated as
 *                                    direction wind is coming from.
 * Jan 14, 2014  2661     bsteffen    For vectors only keep uComponent and
 *                                    vComponent, calculate magnitude and
 *                                    direction on demand.
 * Feb 03, 2014  2764     bsteffen    Ensure that internal buffers are array
 *                                    backed heap buffers.
 * Feb 28, 2013  2791     bsteffen    Use DataSource instead of FloatBuffers
 *                                    for data access
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class GeneralGridData {

    private GridGeometry2D gridGeometry;

    private GeographicDataSource scalarData;

    private GeographicDataSource uComponent = null;

    private GeographicDataSource vComponent = null;

    private Unit<?> dataUnit;

    /**
     * Create a scalar grid data object from float data.
     * 
     * @param scalarData
     * @param dataUnit
     * @return
     */
    public static GeneralGridData createScalarData(
            GeneralGridGeometry gridGeometry, FloatBuffer scalarData,
            Unit<?> dataUnit) {
        DataSource scalarSource = new GeographicDataSource(scalarData,
                gridGeometry);
        return createScalarData(gridGeometry, scalarSource, dataUnit);
    }

    /**
     * Create a scalar grid data object from any data source
     * 
     * @param scalarData
     * @param dataUnit
     * @return
     */
    public static GeneralGridData createScalarData(
            GeneralGridGeometry gridGeometry, DataSource scalarData,
            Unit<?> dataUnit) {
        return new GeneralGridData(gridGeometry, scalarData, dataUnit);
    }

    /**
     * Create gridData for a vector by providing the magnitude and direction of
     * the vector as floats.
     * 
     * @param magnitude
     * @param direction
     * @param dataUnit
     * @return
     */
    public static GeneralGridData createVectorData(
            GeneralGridGeometry gridGeometry, FloatBuffer magnitude,
            FloatBuffer direction, Unit<?> dataUnit) {
        magnitude.rewind();
        direction.rewind();
        FloatBuffer vComponent = FloatBuffer.allocate(magnitude.capacity());
        FloatBuffer uComponent = FloatBuffer.allocate(magnitude.capacity());
        while (magnitude.hasRemaining()) {
            double angle = Math.toRadians(direction.get());
            double mag = magnitude.get();
            vComponent.put((float) (Math.cos(angle) * mag));
            uComponent.put((float) (Math.sin(angle) * mag));
        }
        return createVectorDataUV(gridGeometry, uComponent, vComponent,
                dataUnit);
    }

    /**
     * Create gridData for a vector by providing the u and v components of the
     * vector as floats.
     * 
     * @param uComponent
     * @param vComponent
     * @param dataUnit
     * @return
     */
    public static GeneralGridData createVectorDataUV(
            GeneralGridGeometry gridGeometry, FloatBuffer uComponent,
            FloatBuffer vComponent, Unit<?> dataUnit) {
        DataSource uSource = new GeographicDataSource(uComponent, gridGeometry);
        DataSource vSource = new GeographicDataSource(vComponent, gridGeometry);
        return createVectorDataUV(gridGeometry, uSource, vSource, dataUnit);
    }

    /**
     * Create gridData for a vector by providing the u and v components of the
     * vector as any data source.
     * 
     * @param uComponent
     * @param vComponent
     * @param dataUnit
     * @return
     */
    public static GeneralGridData createVectorDataUV(
            GeneralGridGeometry gridGeometry, DataSource uComponent,
            DataSource vComponent, Unit<?> dataUnit) {
        return new GeneralGridData(gridGeometry, uComponent, vComponent,
                dataUnit);
    }

    private GeneralGridData(GeneralGridGeometry gridGeometry,
            DataSource scalarData, Unit<?> dataUnit) {
        this.gridGeometry = GridGeometry2D.wrap(gridGeometry);
        this.scalarData = GeographicDataSource.wrap(scalarData,
                this.gridGeometry);
        this.dataUnit = dataUnit;
    }

    private GeneralGridData(GeneralGridGeometry gridGeometry,
            DataSource uComponent, DataSource vComponent, Unit<?> dataUnit) {
        this.gridGeometry = GridGeometry2D.wrap(gridGeometry);
        this.uComponent = GeographicDataSource.wrap(uComponent,
                this.gridGeometry);
        this.vComponent = GeographicDataSource.wrap(vComponent,
                this.gridGeometry);
        this.dataUnit = dataUnit;
    }

    /**
     * Attempt to convert this data to the new unit. If this is successful then
     * the dataUnit and data will be changed.
     * 
     * @param unit
     * @return true if units are compatible, false if data is unchanged.
     */
    public boolean convert(Unit<?> unit) {
        if (dataUnit == null && unit == null) {
            return true;
        } else if (dataUnit == null || unit == null) {
            return false;
        }
        if (!dataUnit.isCompatible(unit)) {
            return false;
        }
        UnitConverter converter = dataUnit.getConverterTo(unit);
        if (converter.equals(UnitConverter.IDENTITY)) {
            // no need to actually convert if they are the same.
            return true;
        }
        UnitConvertingDataFilter filter = new UnitConvertingDataFilter(
                converter);
        if (scalarData != null) {
            scalarData = scalarData.applyFilters(filter);
        }
        if (uComponent != null) {
            uComponent = uComponent.applyFilters(filter);

        }
        if (vComponent != null) {
            vComponent = vComponent.applyFilters(filter);
        }
        dataUnit = unit;
        return true;
    }

    /**
     * Create a new GeneralGridData that is a reprojected version of this data.
     * 
     * @param newGridGeometry
     * @param interpolation
     * @return
     * @throws FactoryException
     * @throws TransformException
     */
    public GeneralGridData reproject(GeneralGridGeometry newGridGeometry,
            Interpolation interpolation) throws FactoryException,
            TransformException {
        GridGeometry2D newGeom = GridGeometry2D.wrap(newGridGeometry);
        GridReprojection reproj = PrecomputedGridReprojection.getReprojection(
                gridGeometry, newGeom);
        GridSampler sampler = new GridSampler(interpolation);
        if (isVector()) {
            sampler.setSource(getUComponent());
            float[] udata = reproj.reprojectedGrid(sampler,
                    new FloatBufferWrapper(newGeom.getGridRange2D()))
                    .getArray();
            sampler.setSource(getVComponent());
            float[] vdata = reproj.reprojectedGrid(sampler,
                    new FloatBufferWrapper(newGeom.getGridRange2D()))
                    .getArray();
            // When reprojecting it is necessary to recalculate the
            // direction of vectors based off the change in the "up"
            // direction
            GridEnvelope2D targetRange = newGeom.getGridRange2D();

            MathTransform grid2crs = newGeom.getGridToCRS();
            MathTransform crs2ll = MapUtil.getTransformToLatLon(newGeom
                    .getCoordinateReferenceSystem());

            for (int i = 0; i < targetRange.width; i++) {
                for (int j = 0; j < targetRange.height; j++) {
                    int index = i + j * targetRange.width;
                    if (udata[index] > -9999) {
                        DirectPosition2D dp = new DirectPosition2D(i, j);
                        grid2crs.transform(dp, dp);
                        crs2ll.transform(dp, dp);
                        Coordinate ll = new Coordinate(dp.x, dp.y);
                        double rot = MapUtil.rotation(ll, newGeom);
                        double rot2 = MapUtil.rotation(ll, gridGeometry);
                        double cos = Math.cos(Math.toRadians(rot - rot2));
                        double sin = Math.sin(Math.toRadians(rot - rot2));
                        double u = udata[index];
                        double v = vdata[index];
                        udata[index] = (float) (cos * u - sin * v);
                        vdata[index] = (float) (sin * u + cos * v);
                    }
                }
            }
            return createVectorDataUV(newGridGeometry, FloatBuffer.wrap(udata),
                    FloatBuffer.wrap(vdata), dataUnit);
        } else {
            sampler.setSource(getScalarData());
            return createScalarData(newGridGeometry,
                    new GridReprojectionDataSource(reproj, sampler), dataUnit);
        }

    }

    public GridGeometry2D getGridGeometry() {
        return gridGeometry;
    }

    public boolean isVector() {
        return (uComponent != null && vComponent != null);
    }

    public GeographicDataSource getMagnitude() {
        DataSource rawSource = new MagnitudeDataSource(uComponent, vComponent);
        return new GeographicDataSource(rawSource, this.gridGeometry);
    }

    public GeographicDataSource getScalarData() {
        if (isVector()) {
            return getMagnitude();
        } else {
            return scalarData;
        }
    }

    /**
     * @return the direction from which the vector originates. This is commonly
     *         used in meteorology, espesially for winds. For example if a
     *         meteorologist says "The wind direction is North" it means the
     *         wind is coming from the north and moving to the south.
     * @see #getDirectionTo()
     */
    public GeographicDataSource getDirectionFrom() {
        DataSource rawSource = new DirectionFromDataSource(uComponent,
                vComponent);
        return new GeographicDataSource(rawSource, this.gridGeometry);
    }

    /**
     * @return the direction a vector is going towards. This is the common
     *         mathematical deffinition of a vector.
     * @see #getDirectionFrom()
     */
    public GeographicDataSource getDirectionTo() {
        DataSource rawSource = new DirectionToDataSource(uComponent, vComponent);
        return new GeographicDataSource(rawSource, this.gridGeometry);
    }

    public GeographicDataSource getUComponent() {
        return uComponent;
    }

    public GeographicDataSource getVComponent() {
        return vComponent;
    }

    public Unit<?> getDataUnit() {
        return dataUnit;
    }

    /**
     * Given two grid data with compatible geometries and compatible units, this
     * will combine them into a single data object. Compatible geometries will
     * have the same CRS and grid spacing. This function will create a larger
     * grid geometry which incorporates the data from both sources and fills in
     * NaN for any areas which are not covered by either source.
     * 
     * 
     * @param data1
     * @param data2
     * @return merged data or null if they are not compatible
     */
    public static GeneralGridData mergeData(GeneralGridData data1,
            GeneralGridData data2) {
        GridGeometry2D geometry1 = data1.getGridGeometry();
        GridGeometry2D geometry2 = data2.getGridGeometry();
        CoordinateReferenceSystem crs = geometry1
                .getCoordinateReferenceSystem();
        CoordinateReferenceSystem crs2 = geometry2
                .getCoordinateReferenceSystem();
        if (!crs.equals(crs2)) {
            // Coordinate System is different, incompatible
            return null;
        }
        Envelope2D envelope1 = geometry1.getEnvelope2D();
        GridEnvelope2D range1 = geometry1.getGridRange2D();
        double dx = envelope1.width / range1.width;

        Envelope2D envelope2 = geometry2.getEnvelope2D();
        GridEnvelope2D range2 = geometry2.getGridRange2D();
        double dx2 = envelope2.width / range2.width;
        if (Math.abs(dx - dx2) > 0.00001) {
            // X Spacing is different, incompatible
            return null;
        }
        double dy = envelope1.height / range1.height;
        double dy2 = envelope2.height / range2.height;
        if (Math.abs(dy - dy2) > 0.00001) {
            // Y Spacing is different, incompatible
            return null;
        }
        double xShift = (envelope1.getMinX() - envelope2.getMinX()) / dx;
        if (Math.abs(xShift - Math.round(xShift)) > 0.00001) {
            // grids are not aligned in the x direction
            return null;
        }
        double yShift = (envelope1.getMinY() - envelope2.getMinY()) / dy;
        if (Math.abs(yShift - Math.round(yShift)) > 0.00001) {
            // grids are not aligned in the y direction
            return null;
        }
        if (!data2.convert(data1.getDataUnit())) {
            // units are not compatible
            return null;
        }
        Rectangle2D rectangle = envelope1.createUnion(envelope2);
        Envelope2D envelope = new Envelope2D(crs, rectangle);
        int nx = (int) Math.round(rectangle.getWidth() / dx);
        int ny = (int) Math.round(rectangle.getHeight() / dy);
        GridEnvelope2D range = new GridEnvelope2D(0, 0, nx, ny);

        GridGeometry2D geometry = new GridGeometry2D((GridEnvelope) range,
                (Envelope) envelope);
        // Shift the ranges to be relative to the new geometry
        range1.x = (int) Math.round((envelope1.x - envelope.x) / dx);
        range2.x = (int) Math.round((envelope2.x - envelope.x) / dx);
        // y axis is swapped, our grids start at upper left and y increases down
        // and y axis increases up.
        range1.y = (int) Math.round((envelope.getMaxY() - envelope1.getMaxY())
                / dy);
        range2.y = (int) Math.round((envelope.getMaxY() - envelope2.getMaxY())
                / dy);
        if (data1.isVector() && data2.isVector()) {
            DataSource newU = mergeData(data1.getUComponent(), range1,
                    data2.getUComponent(), range2);
            DataSource newV = mergeData(data1.getVComponent(), range1,
                    data2.getVComponent(), range2);
            return createVectorDataUV(geometry, newU, newV, data1.getDataUnit());
        } else {
            DataSource newData = mergeData(data1.getScalarData(), range1,
                    data2.getScalarData(), range2);
            return createScalarData(geometry, newData, data1.getDataUnit());
        }
    }

    private static DataSource mergeData(DataSource data1, GridEnvelope2D env1,
            DataSource data2, GridEnvelope2D env2) {
        if (env1.x != 0 || env1.y != 0) {
            data1 = new OffsetDataSource(data1, -env1.x, -env1.y);
        }
        if (env2.x != 0 || env2.y != 0) {
            data2 = new OffsetDataSource(data2, -env2.x, -env2.y);
        }
        return new MergedDataSource(data1, data2);
    }

    private static abstract class VectorDataSource implements DataSource {

        protected final DataSource uComponent;

        protected final DataSource vComponent;

        public VectorDataSource(DataSource uComponent, DataSource vComponent) {
            this.uComponent = uComponent;
            this.vComponent = vComponent;
        }

    }

    private static final class MagnitudeDataSource extends VectorDataSource {

        public MagnitudeDataSource(DataSource uComponent, DataSource vComponent) {
            super(uComponent, vComponent);
        }

        @Override
        public double getDataValue(int x, int y) {
            return Math.hypot(uComponent.getDataValue(x, y),
                    vComponent.getDataValue(x, y));
        }
    }

    private static final class DirectionFromDataSource extends VectorDataSource {

        public DirectionFromDataSource(DataSource uComponent,
                DataSource vComponent) {
            super(uComponent, vComponent);
        }

        @Override
        public double getDataValue(int x, int y) {
            return Math.toDegrees(Math.atan2(-uComponent.getDataValue(x, y),
                    -vComponent.getDataValue(x, y)));
        }
    }

    private static final class DirectionToDataSource extends VectorDataSource {

        public DirectionToDataSource(DataSource uComponent,
                DataSource vComponent) {
            super(uComponent, vComponent);
        }

        @Override
        public double getDataValue(int x, int y) {
            return Math.toDegrees(Math.atan2(uComponent.getDataValue(x, y),
                    vComponent.getDataValue(x, y)));
        }
    }

    private static final class MergedDataSource implements DataSource {

        DataSource[] sources;

        public MergedDataSource(DataSource... sources) {
            this.sources = sources;
        }

        @Override
        public double getDataValue(int x, int y) {
            int count = 0;
            double total = 0;
            for (DataSource source : sources) {
                double val = source.getDataValue(x, y);
                if (Double.isNaN(val) == false) {
                    total += val;
                    count += 1;
                }
            }
            if (count == 0) {
                return Double.NaN;
            } else {
                return total / count;
            }
        }
    }
}
