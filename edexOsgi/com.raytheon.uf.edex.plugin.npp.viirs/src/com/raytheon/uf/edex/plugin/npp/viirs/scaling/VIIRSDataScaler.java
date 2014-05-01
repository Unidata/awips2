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
package com.raytheon.uf.edex.plugin.npp.viirs.scaling;

import java.awt.Rectangle;
import java.util.ArrayList;
import java.util.List;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridEnvelope2D;
import org.geotools.geometry.Envelope2D;
import org.geotools.referencing.crs.DefaultEngineeringCRS;

import com.raytheon.uf.common.geospatial.interpolation.BilinearInterpolation;
import com.raytheon.uf.common.geospatial.interpolation.GridReprojection;
import com.raytheon.uf.common.geospatial.interpolation.data.AbstractDataWrapper;
import com.raytheon.uf.common.geospatial.interpolation.data.FloatArrayWrapper;
import com.raytheon.uf.common.geospatial.interpolation.data.UnsignedShortArrayWrapper;

/**
 * This class is responsible for scaling VIIRS data
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 7, 2011             mschenke     Initial creation
 * Feb 21, 2012 #30        mschenke     Changed missing value to float
 * 
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class VIIRSDataScaler {

    private static final int DESIRED_SIZE = 512 * 512;

    /**
     * This method will return a list of recommended sizes to scale the data
     * down to
     * 
     * @param width
     * @param height
     * @return
     */
    public static List<Rectangle> getScaleSizes(int width, int height) {
        List<Rectangle> scaleSizes = new ArrayList<Rectangle>();
        int size = 0;
        do {
            scaleSizes.add(new Rectangle(width, height));
            size = width * height;
            width /= 2;
            height /= 2;
        } while (size > DESIRED_SIZE);

        return scaleSizes;
    }

    /**
     * Given a full dataset, this method will scale the data down to the desired
     * size. Objects supported are 1D java primitive arrays (floats and unsigned
     * shorts)
     * 
     * @param fullData
     * @param fullSize
     * @param desiredSize
     * @return
     */
    public static Object scaleData(Object fullData, Rectangle fullSize,
            Rectangle desiredSize, float missingValue) {
        // Early return check
        if (fullSize.equals(desiredSize)) {
            return fullData;
        }

        GeneralGridGeometry sourceGeom = new GeneralGridGeometry(
                new GridEnvelope2D(fullSize.x, fullSize.y, fullSize.width,
                        fullSize.height), new Envelope2D(
                        DefaultEngineeringCRS.CARTESIAN_2D, fullSize));
        GeneralGridGeometry targetGeom = new GeneralGridGeometry(
                new GridEnvelope2D(desiredSize.x, desiredSize.y,
                        desiredSize.width, desiredSize.height), new Envelope2D(
                        DefaultEngineeringCRS.CARTESIAN_2D, fullSize));

        // Construct reprojection object
        GridReprojection reprojection = new GridReprojection(sourceGeom,
                targetGeom);

        AbstractDataWrapper source = null;
        AbstractDataWrapper destination = null;
        Object returnValue = null;
        if (fullData instanceof short[]) {
            source = new UnsignedShortArrayWrapper((short[]) fullData,
                    sourceGeom);
            destination = new UnsignedShortArrayWrapper(targetGeom);
            returnValue = ((UnsignedShortArrayWrapper) destination).getArray();
        } else if (fullData instanceof float[]) {
            source = new FloatArrayWrapper((float[]) fullData, sourceGeom);
            destination = new FloatArrayWrapper(targetGeom);
            returnValue = ((FloatArrayWrapper) destination).getArray();
        }

        if (returnValue != null) {
            source.setFillValue(missingValue);
            destination.setFillValue(missingValue);

            // Create interpolation object
            BilinearInterpolation interpolation = new BilinearInterpolation();
            interpolation.setMissingThreshold(0.0f);

            try {
                reprojection
                        .reprojectedGrid(interpolation, source, destination);
                return returnValue;
            } catch (Exception e) {
                // TODO: Log
            }
        }
        return null;
    }

    public static int getValidHeight(float[] data, float missing, int width,
            int height) {
        // Find first row with bad data
        for (int h = 0; h < height; ++h) {
            float value = data[h * width];
            if (value == missing) {
                return h;
            }
        }

        return height;
    }
}
