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
package com.raytheon.uf.viz.pointset.image;

import java.io.FileNotFoundException;
import java.lang.ref.SoftReference;
import java.nio.FloatBuffer;
import java.nio.IntBuffer;
import java.util.ArrayList;
import java.util.List;

import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.pointset.PointSetLocation;
import com.raytheon.uf.common.dataplugin.pointset.PointSetRecord;
import com.raytheon.uf.common.dataplugin.pointset.triangulate.DelauneyTriangulator;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.geospatial.util.WorldWrapChecker;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.drawables.triangulated.ITriangleLocationCallback;

/**
 * {@link IColorMapDataRetrievalCallback} implementation for loading point set
 * location data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Aug 28, 2015  4709     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class PointSetLocationCallback implements
        ITriangleLocationCallback {

    private final IDescriptor descriptor;

    private final PointSetRecord record;

    /*
     * Kept as soft reference because both methods from the interface need the
     * location but after they are both called we probably won't need it again
     * so let it get reclaimed when the memory is needed.
     */
    private SoftReference<PointSetLocation> locationRef;

    public PointSetLocationCallback(IDescriptor descriptor,
            PointSetRecord record) {
        this.descriptor = descriptor;
        this.record = record;
    }

    protected PointSetLocation getLocation() throws VizException {
        PointSetLocation location = null;
        if (locationRef != null) {
            location = locationRef.get();
        }
        if (location == null) {
            try {
                location = PointSetLocation.load(record.getStoragePath()
                        .toFile(), record.getLocationId());
            } catch (FileNotFoundException | StorageException e) {
                throw new VizException(e);
            }
        }
        return location;
    }

    @Override
    public double[][] getCoordinates() throws VizException {
        PointSetLocation location = getLocation();
        FloatBuffer longitude = location.getLongitudes().duplicate();
        longitude.rewind();
        FloatBuffer latitude = location.getLatitudes().duplicate();
        latitude.rewind();
        int numPoints = longitude.capacity();

        double[][] coordinates = new double[numPoints][];
        double[] in = new double[2];
        for (int i = 0; i < numPoints; i += 1) {
            in[0] = longitude.get();
            in[1] = latitude.get();
            coordinates[i] = descriptor.worldToPixel(in);
        }
        return coordinates;
    }

    @Override
    public int[] getTriangleIndices() throws VizException {
        PointSetLocation location = getLocation();
        IntBuffer triangles = location.getTriangles();
        if (triangles != null) {
            WorldWrapChecker wwc = new WorldWrapChecker(descriptor
                    .getGridGeometry().getEnvelope());
            if (wwc.needsChecking()) {
                List<Integer> invalidTriangles = new ArrayList<>();
                FloatBuffer longitude = location.getLongitudes().duplicate();
                triangles = triangles.duplicate();
                triangles.rewind();
                while (triangles.hasRemaining()) {
                    float l0 = longitude.get(triangles.get());
                    float l1 = longitude.get(triangles.get());
                    float l2 = longitude.get(triangles.get());

                    if (wwc.check(l0, l1) || wwc.check(l1, l2)
                            || wwc.check(l2, l0)) {
                        invalidTriangles.add(triangles.position() - 3);
                    }
                }
                if (!invalidTriangles.isEmpty()) {
                    IntBuffer goodTriangles = IntBuffer.allocate(triangles
                            .capacity() - invalidTriangles.size() * 3);
                    triangles.rewind();
                    for (Integer invalidTriangle : invalidTriangles) {
                        if (invalidTriangle.intValue() == triangles.position()) {
                            triangles.position(triangles.position() + 3);
                        } else {
                            triangles.limit(invalidTriangle.intValue());
                            goodTriangles.put(triangles);
                            triangles.limit(triangles.capacity());
                            triangles.position(triangles.position() + 3);
                        }
                    }
                    if (triangles.hasRemaining()) {
                        goodTriangles.put(triangles);
                    }

                    triangles = goodTriangles;
                }
            }
            return triangles.array();
        }
        try {
            triangles = new DelauneyTriangulator(descriptor.getCRS(), true)
                    .triangulate(location);
            return triangles.array();
        } catch (TransformException | FactoryException e) {
            throw new VizException(e);
        }
    }

}