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

import org.geotools.geometry.DirectPosition2D;
import org.geotools.referencing.CRS;
import org.geotools.referencing.crs.DefaultGeographicCRS;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.TransformException;

import ucar.ma2.DataType;
import ucar.nc2.Attribute;
import ucar.nc2.NetcdfFile;

import com.raytheon.uf.edex.plugin.goesr.exception.GoesrProjectionException;

/**
 * 
 * Calculates envelope based off the tile center defined in the global
 * attributes of a {@link NetcdfFile}. This is known to fail for Himawari data
 * where the center of the tiles is not on the disk.
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
public class TileCenterEnvelopeFactory extends AbstractCenterEnvelopeFactory {

    @Override
    public GoesrEnvelope getEnvelope(NetcdfFile cdfFile,
            CoordinateReferenceSystem crs) throws GoesrProjectionException {
        GoesrEnvelope envelope = loadDistanceNumber(cdfFile, crs);
        if (envelope == null) {
            return null;
        }
        DirectPosition2D center = new DirectPosition2D();
        Attribute attr = cdfFile.findGlobalAttribute("tile_center_longitude");
        /*
         * The empty string is used by himawari when the center of a tile is off
         * the world, since all valid data is numeric assume any string is
         * invalid.
         */
        if (attr == null || attr.getDataType() == DataType.STRING) {
            return null;
        }else{
            center.x = attr.getNumericValue().doubleValue();
        }
        attr = cdfFile.findGlobalAttribute("tile_center_latitude");
        if (attr == null || attr.getDataType() == DataType.STRING) {
            return null;
        } else {
            center.y = attr.getNumericValue().doubleValue();
        }

        try {
            CRS.findMathTransform(DefaultGeographicCRS.WGS84, crs, true)
                    .transform(center, center);
        } catch (TransformException | FactoryException e) {
            throw new GoesrProjectionException(
                    "Error transforming product center point to CRS space", e);
        }

        double minX = center.x - envelope.getWidth() / 2.0;
        double minY = center.y - envelope.getHeight() / 2.0;

        envelope.setMinX(minX);
        envelope.setMinY(minY);
        return envelope;
    }

}
