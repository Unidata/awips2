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
package com.raytheon.viz.radar.rsc.image;

import java.util.Arrays;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.referencing.FactoryException;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.projection.RadarProjectionFactory;
import com.raytheon.uf.viz.core.IMesh;
import com.raytheon.uf.viz.core.drawables.ext.GraphicsExtension.IGraphicsExtensionInterface;
import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Interface for constructing radial meshes for radar records
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Nov 16, 2011           mschenke    Initial creation
 * Jun 24, 2014  3072     bsteffen    Remove RadarRecord dependency for Radial
 *                                    Mesh
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public interface IRadialMeshExtension extends IGraphicsExtensionInterface {

    /**
     * Construct a mesh to be used for the radial data on the targetGeometry
     * 
     * @param radarData
     * @param descriptor
     * @return
     * @throws VizException
     */
    public IMesh constructMesh(RadialMeshData meshData,
            GeneralGridGeometry targetGeometry) throws VizException;

    public static class RadialMeshData{

        /** longitude of center point of radial data. */
        private float longitude;

        /** latitude of center point of radial data. */
        private float latitude;

        /** angle(in degrees) of each radial */
        private float[] angleData;

        /** number of bins */
        private int numBins;

        /** number of radials */
        private int numRadials;

        /** width of the bins(in meters) */
        private int binWidth;

        /** Angle above ground of the tilt of the mesh */
        private float tiltAngle;

        /**
         * When 0 the mesh will start in the center, a larger number will cause
         * the mesh to begin firstBin*binWidth meters from the center.
         */
        private int firstBin = 0;

        public RadialMeshData() {

        }

        public RadialMeshData(RadarRecord record) {
            longitude = record.getLongitude();
            latitude = record.getLatitude();
            angleData = record.getAngleData();
            numBins = record.getNumBins();
            numRadials = record.getNumRadials();
            binWidth = record.getGateResolution();
            tiltAngle = record.getTrueElevationAngle();
            Integer jStart = record.getJstart();
            if (jStart != null) {
                firstBin = jStart;
            }
        }

        public float getLongitude() {
            return longitude;
        }

        public void setLongitude(float longitude) {
            this.longitude = longitude;
        }

        public float getLatitude() {
            return latitude;
        }

        public void setLatitude(float latitude) {
            this.latitude = latitude;
        }

        public float[] getAngleData() {
            return angleData;
        }

        public void setAngleData(float[] angleData) {
            this.angleData = angleData;
        }

        public int getNumBins() {
            return numBins;
        }

        public void setNumBins(int numBins) {
            this.numBins = numBins;
        }

        public int getNumRadials() {
            return numRadials;
        }

        public void setNumRadials(int numRadials) {
            this.numRadials = numRadials;
        }

        public int getBinWidth() {
            return binWidth;
        }

        public void setBinWidth(int binWidth) {
            this.binWidth = binWidth;
        }

        public float getTiltAngle() {
            return tiltAngle;
        }

        public void setTiltAngle(float tiltAngle) {
            this.tiltAngle = tiltAngle;
        }

        public int getFirstBin() {
            return firstBin;
        }

        public void setFirstBin(int firstBin) {
            this.firstBin = firstBin;
        }

        public GridGeometry2D getGridGeometry() throws FactoryException {
            return RadarProjectionFactory.constructGridGeometry(new Coordinate(
                    longitude, latitude), angleData, binWidth, tiltAngle,
                    numBins, true);

        }

        @Override
        public RadialMeshData clone() {
            RadialMeshData clone = new RadialMeshData();
            clone.setLongitude(longitude);
            clone.setLatitude(latitude);
            clone.setAngleData(angleData);
            clone.setNumBins(numBins);
            clone.setNumRadials(numRadials);
            clone.setBinWidth(binWidth);
            clone.setTiltAngle(tiltAngle);
            clone.setFirstBin(firstBin);
            return clone;
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + Arrays.hashCode(angleData);
            result = prime * result + binWidth;
            result = prime * result + firstBin;
            result = prime * result + Float.floatToIntBits(latitude);
            result = prime * result + Float.floatToIntBits(longitude);
            result = prime * result + numBins;
            result = prime * result + numRadials;
            result = prime * result + Float.floatToIntBits(tiltAngle);
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
            RadialMeshData other = (RadialMeshData) obj;
            if (!Arrays.equals(angleData, other.angleData))
                return false;
            if (binWidth != other.binWidth)
                return false;
            if (firstBin != other.firstBin)
                return false;
            if (Float.floatToIntBits(latitude) != Float
                    .floatToIntBits(other.latitude))
                return false;
            if (Float.floatToIntBits(longitude) != Float
                    .floatToIntBits(other.longitude))
                return false;
            if (numBins != other.numBins)
                return false;
            if (numRadials != other.numRadials)
                return false;
            if (Float.floatToIntBits(tiltAngle) != Float
                    .floatToIntBits(other.tiltAngle))
                return false;
            return true;
        }

        @Override
        public String toString() {
            return "RadialMeshData [longitude=" + longitude + ", latitude="
                    + latitude + ", numBins=" + numBins + ", numRadials="
                    + numRadials + ", binWidth=" + binWidth + ", tiltAngle="
                    + tiltAngle + ", firstBin=" + firstBin + "]";
        }

    }
    
}
