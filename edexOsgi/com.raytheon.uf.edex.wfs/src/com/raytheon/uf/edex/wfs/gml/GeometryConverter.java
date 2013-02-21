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
 * Apr 25, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.edex.wfs.gml;

import net.opengis.gml.v_3_1_1.AbstractGeometryType;

import org.geotools.geometry.jts.JTS;
import org.jvnet.jaxb2_commons.locator.DefaultRootObjectLocator;
import org.jvnet.ogc.gml.v_3_1_1.jts.ConversionFailedException;
import org.jvnet.ogc.gml.v_3_1_1.jts.GML311ToJTSGeometryConverter;

import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.io.ParseException;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public class GeometryConverter {

	public Geometry convert(Envelope env) throws ParseException {
		return JTS.toGeometry(env);
	}

	public Geometry convert(AbstractGeometryType value)
			throws ConversionFailedException {
		GML311ToJTSGeometryConverter converter = new GML311ToJTSGeometryConverter();
		return converter.createGeometry(new DefaultRootObjectLocator(value),
				value);
	}

}
