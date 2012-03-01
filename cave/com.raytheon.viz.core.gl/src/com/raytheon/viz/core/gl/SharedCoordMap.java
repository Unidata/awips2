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
package com.raytheon.viz.core.gl;

import java.util.HashMap;
import java.util.Map;

import javax.media.opengl.GL;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.gl.GLGeometryObject2D.GLGeometryObjectData;

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
 * Jun 9, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class SharedCoordMap {

    public static class SharedCoordinateKey {
        public final int verticalDivisions;

        public final int horizontalDivisions;

        /**
         * @param verticalDivisions
         * @param horizontalDivisions
         */
        public SharedCoordinateKey(int verticalDivisions,
                int horizontalDivisions) {
            this.verticalDivisions = verticalDivisions;
            this.horizontalDivisions = horizontalDivisions;
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + horizontalDivisions;
            result = prime * result + verticalDivisions;
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
            SharedCoordinateKey other = (SharedCoordinateKey) obj;
            if (horizontalDivisions != other.horizontalDivisions)
                return false;
            if (verticalDivisions != other.verticalDivisions)
                return false;
            return true;
        }
    }

    public static class SharedCoordinates {
        private GLGeometryObject2D textureCoords;

        private int refCount = 0;

        private SharedCoordinates(SharedCoordinateKey key, IGLTarget glTarget)
                throws VizException {
            GLGeometryObjectData data = new GLGeometryObjectData(
                    GL.GL_TRIANGLE_STRIP, GL.GL_TEXTURE_COORD_ARRAY);
            data.manageIndicies = false;
            textureCoords = new GLGeometryObject2D(data);
            populateTextureGeom(key, textureCoords);
            textureCoords.compile(glTarget.getGl());
        }

        private void incRef() {
            ++refCount;
        }

        private void decRef() {
            --refCount;
            if (refCount <= 0) {
                refCount = 0;
                textureCoords.dispose();
            }
        }

        public GLGeometryObject2D getTextureCoords() {
            return textureCoords;
        }
    }

    private static SharedCoordMap instance = new SharedCoordMap();

    private Map<SharedCoordinateKey, SharedCoordinates> map = new HashMap<SharedCoordinateKey, SharedCoordinates>();

    private SharedCoordMap() {

    }

    public static synchronized SharedCoordinates get(SharedCoordinateKey key,
            IGLTarget target) throws VizException {
        SharedCoordinates coords = instance.map.get(key);
        if (coords == null) {
            coords = new SharedCoordinates(key, target);
            instance.map.put(key, coords);
        }
        coords.incRef();
        return coords;
    }

    public static synchronized void remove(SharedCoordinateKey key) {
        SharedCoordinates coords = instance.map.get(key);
        if (coords != null) {
            coords.decRef();
            if (coords.refCount <= 0) {
                instance.map.remove(key);
            }
        }
    }

    /**
     * Create our static shared texture coordinate array
     */
    private static void populateTextureGeom(SharedCoordinateKey key,
            GLGeometryObject2D geom) {
        int height = 2 * (key.verticalDivisions + 1);
        int width = key.horizontalDivisions;
        geom.allocate(width * height * 2);

        // get dx and dy for texture points
        float dX = (1.0f / (key.horizontalDivisions));
        float dY = (1.0f / (key.verticalDivisions));

        float xLow, xHigh = 0;

        double[][] texCoords = new double[height][2];

        for (int i = 0; i < width; ++i) {
            xLow = xHigh;
            xHigh = (i + 1) * dX;

            for (int j = 0; j < height;) {
                // Get x/y in terms of 0-1
                float y = ((j / 2) * dY);

                texCoords[j][0] = xLow;
                texCoords[j++][1] = y;

                texCoords[j][0] = xHigh;
                texCoords[j++][1] = y;
            }

            geom.addSegment(texCoords);
        }
    }
}
