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
package com.raytheon.uf.edex.wfs.util;

import java.util.Arrays;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.TimeZone;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;

import net.opengis.gml.v_3_1_1.DirectPositionType;
import net.opengis.gml.v_3_1_1.PointType;

import com.vividsolutions.jts.geom.Point;

/**
 * Utility methods for creating JAXB objects for GML
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 15, 2011            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class JaxbTransUtil {

	protected final static TimeZone GMT = TimeZone.getTimeZone("GMT");

	public static XMLGregorianCalendar getCalendar(Date dt)
			throws DatatypeConfigurationException {
		GregorianCalendar gcal = new GregorianCalendar();
		gcal.setTimeZone(GMT);
		gcal.setTimeInMillis(dt.getTime());
		return DatatypeFactory.newInstance().newXMLGregorianCalendar(gcal);
	}

	public static PointType getGmlPoint(Point p) {
		Double lat = p.getY();
		Double lon = p.getX();
		PointType point = new PointType();
		DirectPositionType pos = new DirectPositionType();
		pos.setValue(Arrays.asList(lon, lat));
		point.setPos(pos);
		return point;
	}
}
