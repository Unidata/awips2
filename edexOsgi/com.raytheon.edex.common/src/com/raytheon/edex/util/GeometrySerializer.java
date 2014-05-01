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

import java.sql.SQLException;

import org.postgis.Geometry;
import org.postgis.PGgeometry;

/**
 * Deserializer/Serializer implementation used during JiBX compilation of a
 * org.postgis.Geometry object.
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
public class GeometrySerializer {

	/**
	 * Converts a geometry WKT into a org.postgis.Geometry object
	 * 
	 * @param wkt
	 *            The geometry WKT
	 * @return The org.postgis.Geometry object represented by the WKT
	 */
	public static Geometry deserializer(String wkt) {
		Geometry geom = null;
		try {
			geom = PGgeometry.geomFromString(wkt);
		} catch (SQLException e) {
		}
		return geom;
	}

	/**
	 * Converts a org.postgis.Geometry object into a WKT string
	 * 
	 * @param geom
	 *            The org.postgis.Geometry object
	 * @return the WKT representation of the geoemtry object
	 */
	public static String serializer(Geometry geom) {
		return geom.toString();
	}
}
