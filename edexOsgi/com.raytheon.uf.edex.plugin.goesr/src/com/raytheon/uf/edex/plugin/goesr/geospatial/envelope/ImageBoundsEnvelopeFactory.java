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
package com.raytheon.uf.edex.plugin.goesr.geospatial.envelope;

import java.io.IOException;

import org.opengis.referencing.crs.CoordinateReferenceSystem;

import ucar.ma2.Array;
import ucar.nc2.NetcdfFile;
import ucar.nc2.Variable;

import com.raytheon.uf.edex.plugin.goesr.exception.GoesrProjectionException;

/**
 * 
 * Uses the x and y variables to determine nx and ny, then use the
 * x_image_bounds and y_image_bounds to find the min/max extents of the data and
 * divide by nx or ny to find dx or dy. Sectorized CMI data does not include the
 * image_bounds and will not work with this image factory.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Apr 17, 2015  4336     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class ImageBoundsEnvelopeFactory extends
        AbstractDimensionEnvelopeFactory {

    @Override
    public GoesrEnvelope getEnvelopeFromDimensions(NetcdfFile cdfFile,
            CoordinateReferenceSystem crs, Variable x, Variable y)
            throws GoesrProjectionException {
        int nx = x.getDimension(0).getLength();
        int ny = y.getDimension(0).getLength();

        double minx;
        double maxx;
        double miny;
        double maxy;

        Variable xBounds = cdfFile.findVariable("x_image_bounds");
        if (xBounds != null && xBounds.getSize() == 2) {
            try {
                Array xData = xBounds.read();
                minx = xData.getFloat(0);
                maxx = xData.getFloat(1);
            } catch (IOException e) {
                throw new GoesrProjectionException(
                        "Error reading x_image_bounds");
            }
        } else {
            return null;
        }

        Variable yBounds = cdfFile.findVariable("y_image_bounds");
        if (yBounds != null && yBounds.getSize() == 2) {
            try {
                Array yData = yBounds.read();
                miny = yData.getFloat(0);
                maxy = yData.getFloat(1);
            } catch (IOException e) {
                throw new GoesrProjectionException(
                        "Error reading y_image_bounds");
            }
        } else {
            return null;
        }

        double dx_native = (maxx - minx) / nx;
        double dy_native = (maxy - miny) / ny;

        double dx = findDistance(crs, dx_native, x);
        double dy = findDistance(crs, dy_native, y);

        minx = minx * dx / dx_native;
        maxx = maxx * dx / dx_native;
        miny = miny * dy / dy_native;
        maxy = maxy * dy / dy_native;

        GoesrEnvelope envelope = new GoesrEnvelope();
        envelope.setNx(nx);
        envelope.setNy(ny);
        envelope.setDx(dx);
        envelope.setDy(dy);
        envelope.setMinX(minx);
        envelope.setMinY(miny);
        return envelope;
    }

}
