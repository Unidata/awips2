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
package com.raytheon.edex.util;

import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.geospatial.CRSCache;

/**
 * Deserializer/Serializer implementation used during JiBX compilation of a
 * org.opengis.referencing.crs.CoordinateReferenceSystem object.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 				380			bphillip	Initial creation
 * 
 * </pre>
 * 
 */
public class CrsSerializer {

    /**
     * Converts a CRS WKT String into a CoordinateReferenceSystem object
     * 
     * @param wkt
     *            A CRS WKT String
     * @return The CoordinateReferenceSystem object represented by the provided
     *         WKT
     * @throws FactoryException
     *             If errors occur when parsing the wkt
     */
    public static CoordinateReferenceSystem deserializer(String wkt)
            throws FactoryException {
        return CRSCache.getInstance().getCoordinateReferenceSystem(wkt);

    }

    /**
     * Converts a CoordinateReferenceSystem object into a CRS WKT
     * 
     * @param crs
     *            The CoordinateReferenceSystem object to convert
     * @return The WKT representation of the CoordinateReferenceSystem object
     */
    public static String serializer(CoordinateReferenceSystem crs) {
        return crs.toWKT();
    }
}
