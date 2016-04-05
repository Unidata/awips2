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

import ucar.nc2.NetcdfFile;
import ucar.nc2.Variable;

import com.raytheon.uf.edex.plugin.goesr.exception.GoesrProjectionException;

/**
 * 
 * Uses the scale and offset and size of the x and y variables to determine the
 * size of the product. This factory should work for all GOES-R products.
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
public class DimensionEnvelopeFactory extends AbstractDimensionEnvelopeFactory {

    @Override
    public GoesrEnvelope getEnvelopeFromDimensions(NetcdfFile cdfFile,
            CoordinateReferenceSystem crs, Variable x, Variable y)
            throws GoesrProjectionException {
        int nx = x.getDimension(0).getLength();
        int ny = y.getDimension(0).getLength();

        double scalex = x.findAttribute("scale_factor").getNumericValue()
                .doubleValue();
        double offsetx = x.findAttribute("add_offset").getNumericValue()
                .doubleValue();
        double scaley = y.findAttribute("scale_factor").getNumericValue()
                .doubleValue();
        double offsety = y.findAttribute("add_offset").getNumericValue()
                .doubleValue();
        double dx = findDistance(crs, scalex, x);
        double dy = findDistance(crs, scaley, y);

        double minx;
        double miny;
        try {

            minx = x.read().getInt(0);
            miny = y.read().getInt(0);
        } catch (IOException e) {
            throw new GoesrProjectionException(
                    "Unable to read min values of x or y dimesnions.", e);
        }

        minx = (minx + offsetx / scalex) * dx;
        miny = (miny + offsety / scaley) * dy;

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
