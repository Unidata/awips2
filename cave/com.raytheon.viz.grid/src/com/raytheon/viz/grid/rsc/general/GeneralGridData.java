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
import com.raytheon.uf.common.geospatial.interpolation.GridReprojection;
import com.raytheon.uf.common.geospatial.interpolation.GridSampler;
import com.raytheon.uf.common.geospatial.interpolation.Interpolation;
import com.raytheon.uf.common.geospatial.interpolation.data.FloatArrayWrapper;
import com.raytheon.uf.common.geospatial.interpolation.data.FloatBufferWrapper;
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
 * ------------- -------- ----------- --------------------------
 * Mar  9, 2011           bsteffen    Initial creation
 * Aug 27, 2013  2287     randerso    Removed 180 degree adjustment required by error
 *                                    in Maputil.rotation
 * Dec 09, 2013  2617     bsteffen    Added 180 degree rotation into reproject
 *                                    so wind direction is calculated as
 *                                    direction wind is coming from.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class GeneralGridData {

    private GridGeometry2D gridGeometry;

    private FloatBuffer scalarData;

    private FloatBuffer direction = null;

    private FloatBuffer uComponent = null;

    private FloatBuffer vComponent = null;

    private Unit<?> dataUnit;

    /**
     * Create a scalar grid Data object.
     * 
     * @param scalarData
     * @param dataUnit
     * @return
     */
    public static GeneralGridData createScalarData(
            GeneralGridGeometry gridGeometry, FloatBuffer scalarData,
            Unit<?> dataUnit) {
        return new GeneralGridData(gridGeometry, scalarData, dataUnit);
    }

    /**
     * Create GridData for a vector. Providing (u,v) and (mag,dir) is redundant
     * and it will be assumed that these are equivalent. This should only be
     * used when both these representations are readily available to save time
     * if one or the other is needed later.
     * 
     * @param magnitude
     * @param direction
     * @param uComponent
     * @param vComponent
     * @param dataUnit
     * @return
     */
    public static GeneralGridData createVectorData(
            GeneralGridGeometry gridGeometry, FloatBuffer magnitude,
            FloatBuffer direction, FloatBuffer uComponent,
            FloatBuffer vComponent, Unit<?> dataUnit) {
        return new GeneralGridData(gridGeometry, magnitude, direction,
                uComponent, vComponent, dataUnit);
    }

    /**
     * Create gridData for a vector by providing the magnitude and direction of
     * the vector.
     * 
     * @param magnitude
     * @param direction
     * @param dataUnit
     * @return
     */
    public static GeneralGridData createVectorData(
            GeneralGridGeometry gridGeometry, FloatBuffer magnitude,
            FloatBuffer direction, Unit<?> dataUnit) {
        return new GeneralGridData(gridGeometry, magnitude, direction, null,
                null, dataUnit);
    }

    /**
     * Create gridData for a vector by providing the u and v components of the
     * vector
     * 
     * @param uComponent
     * @param vComponent
     * @param dataUnit
     * @return
     */
    public static GeneralGridData createVectorDataUV(
            GeneralGridGeometry gridGeometry, FloatBuffer uComponent,
            FloatBuffer vComponent, Unit<?> dataUnit) {
        return new GeneralGridData(gridGeometry, null, null, uComponent,
                vComponent, dataUnit);
    }

    private GeneralGridData(GeneralGridGeometry gridGeometry,
            FloatBuffer scalarData, Unit<?> dataUnit) {
        this.gridGeometry = GridGeometry2D.wrap(gridGeometry);
        this.scalarData = scalarData;
        this.dataUnit = dataUnit;
    }

    private GeneralGridData(GeneralGridGeometry gridGeometry,
            FloatBuffer magnitude, FloatBuffer direction,
            FloatBuffer uComponent, FloatBuffer vComponent, Unit<?> dataUnit) {
        this.gridGeometry = GridGeometry2D.wrap(gridGeometry);
        this.scalarData = magnitude;
        this.direction = direction;
        this.uComponent = uComponent;
        this.vComponent = vComponent;
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
        if (scalarData != null) {
            scalarData.rewind();
            FloatBuffer newData = FloatBuffer.allocate(scalarData.capacity());
            while (scalarData.hasRemaining()) {
                newData.put((float) converter.convert(scalarData.get()));
            }
            newData.rewind();
            scalarData = newData;
        }
        if (uComponent != null) {
            uComponent.rewind();
            FloatBuffer newData = FloatBuffer.allocate(uComponent.capacity());
            while (uComponent.hasRemaining()) {
                newData.put((float) converter.convert(uComponent.get()));
            }
            newData.rewind();
            uComponent = newData;
        }
        if (vComponent != null) {
            vComponent.rewind();
            FloatBuffer newData = FloatBuffer.allocate(vComponent.capacity());
            while (vComponent.hasRemaining()) {
                newData.put((float) converter.convert(vComponent.get()));
            }
            newData.rewind();
            vComponent = newData;
        }
        dataUnit = unit;
        return true;
    }

    public GeneralGridData reproject(GeneralGridGeometry newGridGeometry,
            Interpolation interpolation) throws FactoryException,
            TransformException {
        GridGeometry2D newGeom = GridGeometry2D.wrap(newGridGeometry);
        GridReprojection reproj = new GridReprojection(gridGeometry, newGeom);
        GridSampler sampler = new GridSampler(interpolation);
        if (isVector()) {
            sampler.setSource(new FloatBufferWrapper(getUComponent(),
                    gridGeometry));
            float[] udata = reproj.reprojectedGrid(sampler,
                    new FloatArrayWrapper(newGeom)).getArray();
            sampler.setSource(new FloatBufferWrapper(getVComponent(),
                    gridGeometry));
            float[] vdata = reproj.reprojectedGrid(sampler,
                    new FloatArrayWrapper(newGeom)).getArray();
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
                        /* 
                         * When code calls into this method, the observed state
                         * of things is that u and v represent the direction
                         * the vector is going while mag and dir represent
                         * the direction the vector is coming from. The extra
                         * 180 here makes everything consistently represent the
                         * direction the vector is coming from so that when the
                         * barbs or arrows are rendered the mag and dir are 
                         * calculated as expected. Overall this is a completely 
                         * rediculous way of doing things. During construction
                         * everything should be forced to represent the vector
                         * consistently and we should only be keeping either
                         * u/v or mag/dir to minimize memory consumption.
                         * Unfortunately that is a significant change which is
                         * made high risk by the fact no one documents which 
                         * areas are expecting vectors oriented to vs from. So
                         * for now I(bsteffen) have chosen to simply add in 180
                         * so that the behavior will be exactly as it was before
                         * 2287 because even though it is rediculous it is a well
                         * tested rediculous(theoretically).
                         */ 
                        double cos = Math.cos(Math.toRadians(rot - rot2 + 180));
                        double sin = Math.sin(Math.toRadians(rot - rot2 + 180));
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
            sampler.setSource(new FloatBufferWrapper(getScalarData(),
                    gridGeometry));
            FloatBuffer data = reproj.reprojectedGrid(sampler,
                    new FloatBufferWrapper(newGeom)).getBuffer();
            return createScalarData(newGridGeometry, data, dataUnit);
        }

    }

    public GridGeometry2D getGridGeometry() {
        return gridGeometry;
    }

    public boolean isVector() {
        return (scalarData != null && direction != null)
                || (uComponent != null && vComponent != null);
    }

    public FloatBuffer getMagnitude() {
        if (scalarData == null && uComponent != null && vComponent != null) {
            uComponent.rewind();
            vComponent.rewind();
            scalarData = FloatBuffer.allocate(uComponent.capacity());
            while (vComponent.hasRemaining()) {
                scalarData.put((float) Math.hypot(uComponent.get(),
                        vComponent.get()));
            }
            uComponent.rewind();
            vComponent.rewind();
            scalarData.rewind();
        }
        return scalarData;
    }

    public FloatBuffer getScalarData() {
        return getMagnitude();
    }

    public FloatBuffer getDirection() {
        if (direction == null && uComponent != null && vComponent != null) {
            uComponent.rewind();
            vComponent.rewind();
            direction = FloatBuffer.allocate(uComponent.capacity());
            while (vComponent.hasRemaining()) {
                direction.put((float) Math.toDegrees(Math.atan2(
                        uComponent.get(), vComponent.get())));
            }
            uComponent.rewind();
            vComponent.rewind();
            direction.rewind();
        }
        return direction;
    }

    public FloatBuffer getUComponent() {
        if (uComponent == null && scalarData != null && direction != null) {
            scalarData.rewind();
            direction.rewind();
            uComponent = FloatBuffer.allocate(scalarData.capacity());
            while (scalarData.hasRemaining()) {
                double angle = Math.toRadians(direction.get());
                uComponent.put((float) (Math.sin(angle) * scalarData.get()));
            }
            scalarData.rewind();
            direction.rewind();
            uComponent.rewind();
        }
        return uComponent;
    }

    public FloatBuffer getVComponent() {
        if (vComponent == null && scalarData != null && direction != null) {
            scalarData.rewind();
            direction.rewind();
            vComponent = FloatBuffer.allocate(scalarData.capacity());
            while (scalarData.hasRemaining()) {
                double angle = Math.toRadians(direction.get());
                vComponent.put((float) (Math.cos(angle) * scalarData.get()));
            }
            scalarData.rewind();
            direction.rewind();
            vComponent.rewind();
        }
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
            FloatBuffer newU = FloatBuffer.allocate(nx * ny);
            mergeData(data1.getUComponent(), range1, data2.getUComponent(),
                    range2, newU, range);
            FloatBuffer newV = FloatBuffer.allocate(nx * ny);
            mergeData(data1.getVComponent(), range1, data2.getVComponent(),
                    range2, newV, range);
            return createVectorDataUV(geometry, newU, newV, data1.getDataUnit());
        } else {
            FloatBuffer newData = FloatBuffer.allocate(nx * ny);
            mergeData(data1.getScalarData(), range1, data2.getScalarData(),
                    range2, newData, range);
            return createScalarData(geometry, newData, data1.getDataUnit());
        }
    }

    private static void mergeData(FloatBuffer data1, GridEnvelope2D env1,
            FloatBuffer data2, GridEnvelope2D env2, FloatBuffer destData,
            GridEnvelope2D destEnv) {
        data1.rewind();
        data2.rewind();
        destData.rewind();
        for (int y = 0; y < destEnv.height; y++) {
            for (int x = 0; x < destEnv.width; x++) {
                float v1 = Float.NaN;
                float v2 = Float.NaN;
                if (env1.contains(x, y)) {
                    v1 = data1.get();
                }
                if (env2.contains(x, y)) {
                    v2 = data2.get();
                }
                if (Float.isNaN(v1) && Float.isNaN(v2)) {
                    destData.put(Float.NaN);
                } else if (Float.isNaN(v1)) {
                    destData.put(v2);
                } else if (Float.isNaN(v2)) {
                    destData.put(v1);
                } else {
                    destData.put((v1 + v2) / 2);
                }
            }
        }
    }
}
