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
package com.raytheon.uf.viz.xy.crosssection;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.geotools.geometry.DirectPosition2D;
import org.geotools.referencing.GeodeticCalculator;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.xy.crosssection.CrossSectionRotationsFile.RotationMode;
import com.raytheon.uf.viz.xy.scales.HeightScale;
import org.locationtech.jts.geom.Coordinate;

/**
 * Method for calculating wind relative to the baseline.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Dec 01, 2011           bsteffen    Initial creation
 * Feb 17, 2014  2661     bsteffen    Use only u,v for vectors.
 * 
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class CrossSectionRotation {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(CrossSectionRotation.class);

    public static List<float[]> rotateVector(String parameter,
            List<Coordinate> linePoints, List<float[]> floatData,
            int lineLengthInMeters, HeightScale heightScale,
            CoordinateReferenceSystem dataCRS) {
        if (floatData.size() < 2) {
            return floatData;
        }
        RotationMode mode = CrossSectionRotationsFile
                .getRotationMode(parameter);
        if (mode == RotationMode.VR_NO_ROTATION) {
            return floatData;
        }
        float[] u = floatData.get(0);
        float[] v = floatData.get(1);
        for (int i = 0; i < u.length; i += 1) {
            if (u[i] <= -9999) {
                u[i] = Float.NaN;
            }
            if (v[i] <= -9999) {
                v[i] = Float.NaN;
            }
        }
        float[] result = new float[u.length];
        int width = linePoints.size();
        int height = result.length / width;
        float[] cosRot = new float[width];
        float[] sinRot = new float[width];
        getRotationCoefs(linePoints, cosRot, sinRot, dataCRS);
        switch (mode) {
        case VR_NO_ROTATION: {
            return floatData;
        }
        case VR_COMPONENT_INTO: {
            for (int j = 0; j < height; j++) {
                for (int i = 0; i < width; i++) {
                    int idx = j * width + i;
                    result[idx] = -(u[idx] * sinRot[i]) + (v[idx] * cosRot[i]);
                }
            }
            return Arrays.asList(result);
        }
        case VR_GEO_MOMENTUM: {
            float[] fx = new float[width];
            GeodeticCalculator gc = new GeodeticCalculator();
            Coordinate c = linePoints.get(width / 2);
            gc.setStartingGeographicPoint(c.x, c.y);
            for (int i = 0; i < fx.length; i++) {
                c = linePoints.get(i);
                gc.setDestinationGeographicPoint(c.x, c.y);
                fx[i] = (float) Math.sin(Math.toRadians(c.y)) * 0.1458f;
                // times the distance to the center point in meters
                if (i < width / 2) {
                    fx[i] *= (float) -gc.getOrthodromicDistance() / 1000;
                } else {
                    fx[i] *= (float) gc.getOrthodromicDistance() / 1000;
                }
            }
            for (int j = 0; j < height; j++) {
                for (int i = 0; i < width; i++) {
                    int idx = j * width + i;
                    result[idx] = -(u[idx] * sinRot[i]) + (v[idx] * cosRot[i])
                            + fx[i];
                }
            }
            return Arrays.asList(result);
        }
        case VR_COMPONENT_ALONG: {
            for (int j = 0; j < height; j++) {
                for (int i = 0; i < width; i++) {
                    int idx = j * width + i;
                    result[idx] = (u[idx] * cosRot[i]) + (v[idx] * sinRot[i]);
                }
            }
            return Arrays.asList(result);
        }
        case VR_VERT_CIRC: {
            if (floatData.size() < 3) {
                return floatData;
            }
            float umult = 86400.0f / (lineLengthInMeters);
            float vmult = 86400.0f / ((heightScale.getDifference()) * 100);
            float[] pvv = floatData.get(2);
            float[] resultU = new float[result.length];
            float[] resultV = new float[result.length];
            for (int j = 0; j < height; j++) {
                for (int i = 0; i < width; i++) {
                    int idx = j * width + i;
                    resultU[idx] = umult
                            * ((u[idx] * cosRot[i]) + (v[idx] * sinRot[i]));
                    resultV[idx] = vmult * pvv[idx];
                }
            }
            return Arrays.asList(resultU, resultV);
        }
        default: {
            float[] resultU = new float[result.length];
            float[] resultV = new float[result.length];
            for (int j = 0; j < height; j++) {
                for (int i = 0; i < width; i++) {
                    int idx = j * width + i;
                    resultU[idx] = (u[idx] * cosRot[i]) + (v[idx] * sinRot[i]);
                    resultV[idx] = -(u[idx] * sinRot[i]) + (v[idx] * cosRot[i]);
                }
            }
            return Arrays.asList(resultU, resultV);
        }
        }

    }

    private static void getRotationCoefs(List<Coordinate> linePoints,
            float[] cosRot, float[] sinRot, CoordinateReferenceSystem crs) {
        List<Coordinate> convertedPoints = new ArrayList<Coordinate>();
        try {
            MathTransform transform = MapUtil.getTransformFromLatLon(crs);
            for (Coordinate c : linePoints) {
                DirectPosition2D dp = new DirectPosition2D(c.x, c.y);
                transform.transform(dp, dp);
                convertedPoints.add(new Coordinate(dp.x, dp.y));
            }
        } catch (Throwable e) {
            statusHandler.handle(Priority.VERBOSE,
                    "Error reprojecting points for cross section rotation", e);
            convertedPoints = linePoints;
        }
        for (int i = 0; i < convertedPoints.size() - 1; i++) {
            Coordinate c1 = convertedPoints.get(i);
            Coordinate c2 = convertedPoints.get(i + 1);
            double x = c2.x - c1.x;
            double y = c2.y - c1.y;
            double m = Math.sqrt(x * x + y * y);
            cosRot[i] = (float) (x / m);
            sinRot[i] = (float) (y / m);
        }
        cosRot[cosRot.length - 1] = Float.NaN;
        sinRot[sinRot.length - 1] = Float.NaN;

    }
}
