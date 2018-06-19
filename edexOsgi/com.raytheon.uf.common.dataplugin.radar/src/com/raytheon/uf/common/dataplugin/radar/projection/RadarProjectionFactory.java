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
package com.raytheon.uf.common.dataplugin.radar.projection;

import org.geotools.coverage.grid.GridEnvelope2D;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.Envelope2D;
import org.geotools.referencing.operation.DefaultMathTransformFactory;
import org.geotools.referencing.operation.builder.GridToEnvelopeMapper;
import org.geotools.referencing.operation.projection.MapProjection.AbstractProvider;
import org.opengis.parameter.ParameterValueGroup;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.crs.ProjectedCRS;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Convenient location for building CRS and GridGeometries for radar radial
 * data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 7, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class RadarProjectionFactory {

    private static DefaultMathTransformFactory dmtFactory = new DefaultMathTransformFactory();

    public static synchronized ProjectedCRS constructAzRan(
            Coordinate centerLatLon) throws FactoryException {
        ParameterValueGroup group = dmtFactory
                .getDefaultParameters("Azimuth_Range");
        group.parameter(AbstractProvider.SEMI_MAJOR.getName().getCode())
                .setValue(MapUtil.AWIPS_EARTH_RADIUS);
        group.parameter(AbstractProvider.SEMI_MINOR.getName().getCode())
                .setValue(MapUtil.AWIPS_EARTH_RADIUS);
        group.parameter(AbstractProvider.CENTRAL_MERIDIAN.getName().getCode())
                .setValue(centerLatLon.x);
        group.parameter(AbstractProvider.LATITUDE_OF_ORIGIN.getName().getCode())
                .setValue(centerLatLon.y);
        return MapUtil.constructProjection("Azimuth Range", group);
    }

    public static synchronized ProjectedCRS constructRadialBin(
            Coordinate centerLatLon, float[] angleData, double binWidth,
            double tiltAngle) throws FactoryException {
        ParameterValueGroup group = dmtFactory
                .getDefaultParameters("Radial_Bin");
        group.parameter(AbstractProvider.SEMI_MAJOR.getName().getCode())
                .setValue(MapUtil.AWIPS_EARTH_RADIUS);
        group.parameter(AbstractProvider.SEMI_MINOR.getName().getCode())
                .setValue(MapUtil.AWIPS_EARTH_RADIUS);
        group.parameter(AbstractProvider.CENTRAL_MERIDIAN.getName().getCode())
                .setValue(centerLatLon.x);
        group.parameter(AbstractProvider.LATITUDE_OF_ORIGIN.getName().getCode())
                .setValue(centerLatLon.y);
        group.parameter(
                RadialBinMapProjection.Provider.ANGLE_DATA.getName().getCode())
                .setValue(angleData);
        group.parameter(
                RadialBinMapProjection.Provider.BIN_LENGTH.getName().getCode())
                .setValue(binWidth);
        group.parameter(
                RadialBinMapProjection.Provider.TILT_ANGLE.getName().getCode())
                .setValue(tiltAngle);
        return MapUtil.constructProjection("Radial Bin", group);
    }

    /**
     * Construct a grid geometry with a radial bin projection.
     * 
     * @param centerLatLon
     *            - the location at the center of the projection.
     * @param angleData
     *            - The angles of the various radials
     * @param binWidth
     *            - the width of bins in meters.
     * @param tiltAngle
     *            - the elevation angle of radar tilt, 0 if there is no tilt.
     * @param numBins
     *            - the number of bins for the geometry
     * @param binRadial
     *            - If true then the geometry will be constructed for data in
     *            bin,radial(bin-major) format as opposed to
     *            radial,bin(radial-major). This can be good becauase radar data
     *            is currently stored in bin,radial alignment in hdf5. Use this
     *            option with caution because not all code using GridGeometries
     *            will correctly use the result, for example the switch is lost
     *            in serialization. When this is set to false then the resulting
     *            GridGeometry should work with any code using GridGeometries
     *            but the raw data will need to be realigned to radial,bin
     *            format.
     * 
     * @return
     * @throws FactoryException
     */
    public static GridGeometry2D constructGridGeometry(Coordinate centerLatLon,
            float[] angleData, double binWidth, double tiltAngle, int numBins,
            boolean binRadial) throws FactoryException {
        CoordinateReferenceSystem crs = constructRadialBin(centerLatLon,
                angleData, binWidth, tiltAngle);
        GridEnvelope2D gridRange = null;
        if (binRadial) {
            gridRange = new GridEnvelope2D(0, 0, numBins, angleData.length);
        } else {
            gridRange = new GridEnvelope2D(0, 0, angleData.length, numBins);
        }
        Envelope2D envelope = new Envelope2D(crs, 0, 0, angleData.length,
                numBins);
        GridToEnvelopeMapper mapper = new GridToEnvelopeMapper(gridRange,
                envelope);
        mapper.setSwapXY(binRadial);
        if (binRadial) {
            mapper.setReverseAxis(new boolean[] { false, false });
        }
        return new GridGeometry2D(gridRange, mapper.createTransform(), crs);
    }

}
