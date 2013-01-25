/*
 * The following software products were developed by Raytheon:
 *
 * ADE (AWIPS Development Environment) software
 * CAVE (Common AWIPS Visualization Environment) software
 * EDEX (Environmental Data Exchange) software
 * uFrame™ (Universal Framework) software
 *
 * Copyright (c) 2010 Raytheon Co.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/org/documents/epl-v10.php
 *
 *
 * Contractor Name: Raytheon Company
 * Contractor Address:
 * 6825 Pine Street, Suite 340
 * Mail Stop B8
 * Omaha, NE 68106
 * 402.291.0100
 *
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 15, 2011            bclement     Initial creation
 *
 */
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
