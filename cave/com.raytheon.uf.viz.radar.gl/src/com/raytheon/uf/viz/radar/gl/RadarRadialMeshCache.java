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
package com.raytheon.uf.viz.radar.gl;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IMesh;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.hdf5.ImageTile;

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
 * Jul 28, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class RadarRadialMeshCache {

    private static class CacheKey {

        private final float latitude;

        private final float longitude;

        private final int hashCode;

        private final int numBins;

        private final int numRadials;

        private final int gateResolution;

        private final float trueElevationAngle;

        private final int jStart;

        private final float[] angleData;

        private final GeneralGridGeometry gridGeometry;

        public CacheKey(float latitude, float longitude, int numBins,
                int numRadials, int gateResolution, float trueElevationAngle,
                int jStart, float[] angleData, GeneralGridGeometry gridGeometry) {
            this.latitude = latitude;
            this.longitude = longitude;
            this.numBins = numBins;
            this.numRadials = numRadials;
            this.gateResolution = gateResolution;
            this.trueElevationAngle = trueElevationAngle;
            this.jStart = jStart;
            this.angleData = angleData;
            this.gridGeometry = gridGeometry;
            final int prime = 31;
            int hashCode = 1;
            hashCode = prime * hashCode + Arrays.hashCode(angleData);
            hashCode = prime * hashCode + gateResolution;
            hashCode = prime * hashCode + jStart;
            hashCode = prime * hashCode + Float.floatToIntBits(latitude);
            hashCode = prime * hashCode + Float.floatToIntBits(longitude);
            hashCode = prime * hashCode + numBins;
            hashCode = prime * hashCode + numRadials;
            hashCode = prime * hashCode
                    + Float.floatToIntBits(trueElevationAngle);
            hashCode = prime * hashCode + gridGeometry.hashCode();
            this.hashCode = hashCode;
        }

        @Override
        public int hashCode() {
            return hashCode;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            CacheKey other = (CacheKey) obj;
            if (hashCode != other.hashCode)
                return false;
            if (gateResolution != other.gateResolution)
                return false;
            if (jStart != other.jStart)
                return false;
            if (latitude != other.latitude)
                return false;
            if (longitude != other.longitude)
                return false;
            if (numBins != other.numBins)
                return false;
            if (numRadials != other.numRadials)
                return false;
            if (Float.floatToIntBits(trueElevationAngle) != Float
                    .floatToIntBits(other.trueElevationAngle))
                return false;
            if (!Arrays.equals(angleData, other.angleData))
                return false;
            if (gridGeometry != null && other.gridGeometry == null) {
                return false;
            }
            if (gridGeometry == null && other.gridGeometry != null) {
                return false;
            }
            if (gridGeometry != null
                    && !gridGeometry.equals(other.gridGeometry)) {
                return false;
            }
            return true;
        }
    }

    public static class RadarSharedMesh implements IMesh {

        private final CacheKey key;

        private final IMesh mesh;

        private int refCount = 0;

        private boolean calculated = false;

        private RadarSharedMesh(IMesh mesh, CacheKey key) {
            this.mesh = mesh;
            this.key = key;
        }

        @Override
        public void paint(IGraphicsTarget target, PaintProperties paintProps)
                throws VizException {
            mesh.paint(target, paintProps);
        }

        @Override
        public synchronized void calculateMesh(PixelCoverage pc,
                ImageTile tile, MathTransform toLatLon) {
            if (!calculated) {
                mesh.calculateMesh(pc, tile, toLatLon);
                calculated = true;
            }

        }

        @Override
        public synchronized void calculateMesh(PixelCoverage pc,
                GridGeometry2D gg) {
            if (!calculated) {
                mesh.calculateMesh(pc, gg);
                calculated = true;
            }

        }

        @Override
        public void dispose() {
            refCount -= 1;
            synchronized (cache) {
                if (refCount == 0) {
                    mesh.dispose();
                    cache.remove(key);
                }
            }
        }

        private void use() {
            refCount += 1;
        }

        @Override
        public boolean intersects(IExtent extent) {
            return mesh.intersects(extent);
        }
    }

    private static Map<CacheKey, RadarSharedMesh> cache = new HashMap<CacheKey, RadarSharedMesh>();

    public static RadarSharedMesh getMesh(RadarRecord radarData,
            IDescriptor descriptor) throws VizException {
        float latitude = radarData.getLatitude();
        float longitude = radarData.getLongitude();
        int numBins = radarData.getNumBins();
        int numRadials = radarData.getNumRadials();
        int gateResolution = radarData.getGateResolution();
        float trueElevationAngle = radarData.getTrueElevationAngle();
        Integer jStart = radarData.getJstart();
        if (jStart == null) {
            jStart = 0;
        }
        float[] angleData = radarData.getAngleData();
        CacheKey key = new CacheKey(latitude, longitude, numBins, numRadials,
                gateResolution, trueElevationAngle, jStart, angleData,
                descriptor.getGridGeometry());
        synchronized (cache) {
            RadarSharedMesh mesh = cache.get(key);
            if (mesh == null) {
                // System.out.println("Mesh Cache miss");
                IMesh baseMesh = new RadarRadialMesh(
                        (IMapDescriptor) descriptor, radarData);
                mesh = new RadarSharedMesh(baseMesh, key);
                cache.put(key, mesh);
            } else {
                // System.out.println("Mesh Cache hit");
            }
            mesh.use();
            return mesh;
        }
    }

}
