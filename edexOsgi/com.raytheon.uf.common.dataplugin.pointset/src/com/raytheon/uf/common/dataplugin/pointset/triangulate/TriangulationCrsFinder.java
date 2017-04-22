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
package com.raytheon.uf.common.dataplugin.pointset.triangulate;

import org.geotools.geometry.DirectPosition2D;
import org.geotools.geometry.DirectPosition3D;
import org.geotools.referencing.CRS;
import org.geotools.referencing.crs.DefaultGeocentricCRS;
import org.geotools.referencing.crs.DefaultGeographicCRS;
import org.geotools.referencing.datum.DefaultEllipsoid;
import org.geotools.referencing.operation.DefaultMathTransformFactory;
import org.opengis.parameter.ParameterValueGroup;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.datum.Ellipsoid;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

/**
 * Attempt to find the most appropriate CRS to perform a delauney triangulation
 * on a scattered set of points. Uses stereographic projection centered over the
 * data as long as the data is all contained within a hemisphere. For data
 * larger than a hemisphere it just uses an Equidistant Cylindrical Projection
 * centered over the data. Using stereographic avoids problem that are common
 * near the poles and the antimeridian.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Aug 24, 2015  4709     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class TriangulationCrsFinder {

    /**
     * Based off a spherical earth with a radius of 6371229 meters this is the
     * squared length of a chord that covers 1/4 of the diameter of the sphere
     * of the earth. In a 3D geocentric space it is useful for quickly
     * determining if points are within the same hemisphere.
     * 
     * <pre>
     *             ** P **                           ** P **                           ** P **               
     *        **             **                 **   ,'        **                 **   ,'   ',   **          
     *                                             ,'                                ,'       ',             
     *    **                     **         **   ,'                **         **   ,'           ',   **      
     *                                         ,'                                ,'               ',         
     *  **                         **     ** ,'                      **     ** ,'                   ', **    
     *                                    C '                               C '-----------------------' C    
     *  **                         **     **                         **     
     *                                                                      
     *    **                     **         **                     **        
     *                                                                       
     *        **             **                 **             **           
     *             **   **                           **   **                      
     * 
     *      The earth as a circle          Point C is ¼ circle from P         Extending the chord in all 
     *      P is the center of the         The line from P to C is the      directions creates a semicircle
     *         area of interest             chord this value measures        in 3D it creates a hemisphere
     * </pre>
     * 
     * The reason this is the squared chord length instead of the actual chord
     * length is so that we can skip the square root operation when calculating
     * distances to compare to this value.
     * 
     * The math to calculate the value is (6371229 * √2)²
     */
    private static final long MAX_STEREOGRAPHIC_CHORD_LENGTH_SQUARED = 81185117940882l;

    protected final MathTransform lonLatTo3D;

    public TriangulationCrsFinder() throws FactoryException {
        CoordinateReferenceSystem threeD = DefaultGeocentricCRS.CARTESIAN;
        CoordinateReferenceSystem lonLat = DefaultGeographicCRS.WGS84;
        lonLatTo3D = CRS.findMathTransform(lonLat, threeD);
    }

    protected float[] transform3D(float[] interleavedLonLats)
            throws TransformException {
        int numPoints = interleavedLonLats.length / 2;
        float[] xyz = new float[numPoints * 3];
        lonLatTo3D.transform(interleavedLonLats, 0, xyz, 0, numPoints);
        return xyz;
    }

    protected DirectPosition2D findLonLatCenter(float[] xyz)
            throws TransformException {
        int numPoints = xyz.length / 3;

        double x = 0;
        double y = 0;
        double z = 0;
        for (int i = 0; i < xyz.length; i += 3) {
            x += xyz[i];
            y += xyz[i + 1];
            z += xyz[i + 2];
        }

        /*
         * Its important to realize that this center3D may very well be within
         * the surface of the planet and when it is transformed to LonLat, the
         * depth component is lost, essentially moving the coordinate to the
         * surface. So even though the center3D used in createTransform() looks
         * like it is the same as this point, it is not.
         */
        DirectPosition3D center3D = new DirectPosition3D(x / numPoints, y
                / numPoints, z / numPoints);
        DirectPosition2D centerLonLat = new DirectPosition2D();
        lonLatTo3D.inverse().transform(center3D, centerLonLat);

        return centerLonLat;
    }

    protected MathTransform createTransform(float[] xyz)
            throws FactoryException, TransformException {
        DirectPosition2D centerLonLat = findLonLatCenter(xyz);
        DirectPosition3D center3D = new DirectPosition3D();
        lonLatTo3D.transform(centerLonLat, center3D);

        boolean isStereographicOK = true;
        for (int i = 0; i < xyz.length; i += 3) {
            double xd = center3D.x - xyz[i];
            double yd = center3D.y - xyz[i + 1];
            double zd = center3D.z - xyz[i + 2];
            double chord_squared = xd * xd + yd * yd + zd * zd;
            if (chord_squared > MAX_STEREOGRAPHIC_CHORD_LENGTH_SQUARED) {
                isStereographicOK = false;
                break;
            }
        }
        xyz = null;
        if (isStereographicOK) {
            DefaultMathTransformFactory dmtFactory = new DefaultMathTransformFactory();
            Ellipsoid ellipsoid = DefaultEllipsoid.WGS84;
            ParameterValueGroup parameters = dmtFactory
                    .getDefaultParameters("Stereographic");
            parameters.parameter("semi_major").setValue(
                    ellipsoid.getSemiMajorAxis());
            parameters.parameter("semi_minor").setValue(
                    ellipsoid.getSemiMinorAxis());
            parameters.parameter("central_meridian").setValue(centerLonLat.x);
            parameters.parameter("latitude_of_origin").setValue(centerLonLat.y);

            return dmtFactory.createParameterizedTransform(parameters);

        } else {
            DefaultMathTransformFactory dmtFactory = new DefaultMathTransformFactory();
            Ellipsoid ellipsoid = DefaultEllipsoid.WGS84;
            ParameterValueGroup parameters = dmtFactory
                    .getDefaultParameters("Equidistant_Cylindrical");
            parameters.parameter("semi_major").setValue(
                    ellipsoid.getSemiMajorAxis());
            parameters.parameter("semi_minor").setValue(
                    ellipsoid.getSemiMinorAxis());
            parameters.parameter("central_meridian").setValue(centerLonLat.x);

            return dmtFactory.createParameterizedTransform(parameters);
        }
    }

    public MathTransform getTransfromFromLonLat(float[] interleavedLonLats)
            throws TransformException, FactoryException {
        float[] xyz = transform3D(interleavedLonLats);
        return createTransform(xyz);

    }

}
