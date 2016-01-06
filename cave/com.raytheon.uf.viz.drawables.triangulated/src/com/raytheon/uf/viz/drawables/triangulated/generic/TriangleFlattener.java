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
package com.raytheon.uf.viz.drawables.triangulated.generic;

import java.nio.ByteBuffer;
import java.nio.DoubleBuffer;
import java.nio.FloatBuffer;
import java.nio.IntBuffer;
import java.nio.ShortBuffer;
import java.util.Arrays;

import com.raytheon.uf.common.colormap.image.ColorMapData;
import com.raytheon.uf.common.colormap.image.ColorMapData.ColorMapDataType;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.drawables.triangulated.ITriangleLocationCallback;
import com.raytheon.uf.viz.drawables.triangulated.ITriangulatedImageExtension;
import com.raytheon.uf.viz.drawables.triangulated.TriangleMath;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Triangle;

/**
 * Uses an {@link IColorMapDataRetrievalCallback} and
 * {@link ITriangleLocationCallback} to get triangulated data and projects it
 * onto a grid, returning the result as a {@link ColorMapData}.
 * 
 * This is intended to be a minimally complex reference implementation of the
 * {@link ITriangulatedImageExtension}. Most targets would benefit from a custom
 * implementation that more efficiently renders the triangles into the target.
 * The biggest limitation of this implementation is that it uses a constant
 * image size of 1024x1024 to render the entire triangulation. When there are
 * very few triangles this can inflate the size of the data and waste space.
 * When there are very many triangles this will cause a loss of detail in the
 * resulting image. When the distribution and/or size of the triangles is
 * inconsistent then the consistent spacing of a grid will lead to variable
 * levels of distortion. For triangulated areas that are non-rectangular a
 * considerable amount of the image space is NaN filled to pad a rectangular
 * area. For triangulated areas with dramatically skewed aspect ratios a square
 * grid is not an efficient distribution of the data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- ----------------------------
 * Aug 18, 2015  4709     bsteffen  Initial creation
 * Dec 04, 2015  5146     bsteffen  Limit the size of the image
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class TriangleFlattener implements IColorMapDataRetrievalCallback {

    private final IColorMapDataRetrievalCallback dataSource;

    private final ITriangleLocationCallback locationSource;

    private final int[] dimensions = { 1024, 1024 };

    private Envelope envelope = new Envelope();

    /*
     * Limit the size of the image, this is general the size of the displayed
     * area.
     */
    private Envelope maximumArea;

    public TriangleFlattener(IColorMapDataRetrievalCallback dataSource,
            ITriangleLocationCallback locationSource, Envelope maximumArea) {
        this.dataSource = dataSource;
        this.locationSource = locationSource;
        this.maximumArea = maximumArea;
    }


    protected void calculateEnvelope(double[][] coordinates) {
        for (double[] coordinate : coordinates) {
            envelope.expandToInclude(coordinate[0], coordinate[1]);
        }
        if (maximumArea != null) {
            envelope = envelope.intersection(maximumArea);
        }
    }

    @Override
    public ColorMapData getColorMapData() throws VizException {
        double[][] coordinates = locationSource.getCoordinates();
        calculateEnvelope(coordinates);
        double dx = envelope.getWidth() / dimensions[0];
        double dy = envelope.getHeight() / dimensions[1];


        int[] indices = locationSource.getTriangleIndices();
        IntBuffer indicesBuffer = IntBuffer.wrap(indices);
        ColorMapData data = dataSource.getColorMapData();
        float[] grid = new float[dimensions[0] * dimensions[1]];
        Arrays.fill(grid, Float.NaN);
        while (indicesBuffer.hasRemaining()) {
            int index = indicesBuffer.get();
            double x = coordinates[index][0];
            double y = coordinates[index][1];
            double z = extractDataValue(data, index);
            Coordinate p0 = new Coordinate(x, y, z);
            index = indicesBuffer.get();
            x = coordinates[index][0];
            y = coordinates[index][1];
            z = extractDataValue(data, index);
            Coordinate p1 = new Coordinate(x, y, z);
            index = indicesBuffer.get();
            x = coordinates[index][0];
            y = coordinates[index][1];
            z = extractDataValue(data, index);
            Coordinate p2 = new Coordinate(x, y, z);

            Envelope triEnv = new Envelope(p0);
            triEnv.expandToInclude(p1);
            triEnv.expandToInclude(p2);

            triEnv = triEnv.intersection(envelope);
            if (triEnv.isNull()) {
                continue;
            }

            int minX = (int) ((triEnv.getMinX() - envelope.getMinX()) / dx);
            int maxX = (int) ((triEnv.getMaxX() - envelope.getMinX()) / dx);
            int minY = (int) ((triEnv.getMinY() - envelope.getMinY()) / dy);
            int maxY = (int) ((triEnv.getMaxY() - envelope.getMinY()) / dy);
            for (int i = minX; i <= maxX; i += 1) {
                x = envelope.getMinX() + i * dx;
                for (int j = minY; j <= maxY; j += 1) {
                    y = envelope.getMinY() + j * dy;
                    Coordinate t = new Coordinate(x, y);
                    if (TriangleMath.isInTriangle(t, p0, p1, p2)) {
                        grid[j * dimensions[0] + i] = (float) Triangle
                                .interpolateZ(t, p0, p1, p2);
                    }
                }
            }
        }
        return new ColorMapData(FloatBuffer.wrap(grid), dimensions,
                ColorMapDataType.FLOAT, data.getDataUnit());
    }

    public double extractDataValue(ColorMapData data, int index) {
        switch (data.getDataType()) {
        case BYTE:
            return ((ByteBuffer) data.getBuffer()).get(index) & 0xFF;
        case SIGNED_BYTE:
            return ((ByteBuffer) data.getBuffer()).get(index);
        case UNSIGNED_SHORT:
            return ((ShortBuffer) data.getBuffer()).get(index) & 0xFFFF;
        case SHORT:
            return ((ShortBuffer) data.getBuffer()).get(index);
        case INT:
            return ((IntBuffer) data.getBuffer()).get(index);
        case FLOAT:
            return ((FloatBuffer) data.getBuffer()).get(index);
        case DOUBLE:
            return ((DoubleBuffer) data.getBuffer()).get(index);
        default:
            throw new IllegalStateException(
                    "Unsupported dataType has been used for a triangulated image: "
                            + data.getDataType());
        }
    }

    public PixelCoverage getPixelCoverage() throws VizException {
        if (envelope.isNull()) {
            double[][] coordinates = locationSource.getCoordinates();
            calculateEnvelope(coordinates);
        }
        Coordinate ul = new Coordinate(envelope.getMinX(), envelope.getMinY());
        Coordinate ur = new Coordinate(envelope.getMaxX(), envelope.getMinY());
        Coordinate lr = new Coordinate(envelope.getMaxX(), envelope.getMaxY());
        Coordinate ll = new Coordinate(envelope.getMinX(), envelope.getMaxY());
        return new PixelCoverage(ul, ur, lr, ll);
    }

    public int[] convertToImageSpace(double x, double y) {
        double dx = envelope.getWidth() / dimensions[0];
        double dy = envelope.getHeight() / dimensions[1];
        int[] result = new int[2];
        result[0] = (int) ((x - envelope.getMinX()) / dx);
        result[1] = (int) ((y - envelope.getMinY()) / dy);
        if (result[0] < 0 || result[1] < 0) {
            return null;
        } else if (result[0] > dimensions[0] || result[1] > dimensions[1]) {
            return null;
        }
        return result;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((dataSource == null) ? 0 : dataSource.hashCode());
        result = prime * result
                + ((locationSource == null) ? 0 : locationSource.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        TriangleFlattener other = (TriangleFlattener) obj;
        if (dataSource == null) {
            if (other.dataSource != null)
                return false;
        } else if (!dataSource.equals(other.dataSource))
            return false;
        if (locationSource == null) {
            if (other.locationSource != null)
                return false;
        } else if (!locationSource.equals(other.locationSource))
            return false;
        return true;
    }

}
