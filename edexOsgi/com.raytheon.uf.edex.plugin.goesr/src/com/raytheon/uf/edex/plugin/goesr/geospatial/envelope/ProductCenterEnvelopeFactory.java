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

import ucar.nc2.Attribute;
import ucar.nc2.NetcdfFile;

import com.raytheon.uf.edex.plugin.goesr.exception.GoesrProjectionException;

/**
 * 
 * Calculate an envelope by offseting a tile withn a product using the tile
 * offsets and the product center. This method should work for any sectorized
 * CMI data.
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
public class ProductCenterEnvelopeFactory extends AbstractCenterEnvelopeFactory {

    @Override
    public GoesrEnvelope getEnvelope(NetcdfFile cdfFile,
            CoordinateReferenceSystem crs) throws GoesrProjectionException {
        GoesrEnvelope envelope = loadDistanceNumber(cdfFile, crs);
        if (envelope == null) {
            return null;
        }
        DirectPosition2D center = new DirectPosition2D();
        int productNx;
        int productNy;
        int offsetx;
        int offsety;
        Attribute attr = cdfFile.findGlobalAttribute("product_center_longitude");
        if(attr == null){
            return null;
        }else{
            center.x = attr.getNumericValue().doubleValue();
        }
        attr = cdfFile.findGlobalAttribute("product_center_latitude");
        if (attr == null) {
            return null;
        } else {
            center.y = attr.getNumericValue().doubleValue();
        }
        attr = cdfFile.findGlobalAttribute("product_columns");
        if (attr == null) {
            return null;
        } else {
            productNx = attr.getNumericValue().intValue();
        }
        attr = cdfFile.findGlobalAttribute("product_rows");
        if (attr == null) {
            return null;
        } else {
            productNy = attr.getNumericValue().intValue();
        }
        attr = cdfFile.findGlobalAttribute("tile_column_offset");
        if (attr == null) {
            return null;
        } else {
            offsetx = attr.getNumericValue().intValue();
        }
        attr = cdfFile.findGlobalAttribute("tile_row_offset");
        if (attr == null) {
            return null;
        } else {
            offsety = attr.getNumericValue().intValue();
        }
        
        try {
            CRS.findMathTransform(DefaultGeographicCRS.WGS84, crs, true)
                    .transform(center, center);
        } catch (TransformException | FactoryException e) {
            throw new GoesrProjectionException(
                    "Error transforming product center point to CRS space", e);
        }

        double left = center.x + (offsetx - productNx / 2.0) * envelope.getDx();

        double top = center.y - (offsety - productNy / 2.0) * envelope.getDy();
        double bottom = top - envelope.getHeight();

        envelope.setMinX(left);
        envelope.setMinY(bottom);
        return envelope;
    }

}
