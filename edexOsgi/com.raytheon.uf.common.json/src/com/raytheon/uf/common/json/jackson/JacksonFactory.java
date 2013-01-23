/**********************************************************************
 *
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
 **********************************************************************/
/**
 * 
 */
package com.raytheon.uf.common.json.jackson;

import java.util.LinkedList;
import java.util.List;

import org.apache.commons.pool.KeyedPoolableObjectFactory;
import org.codehaus.jackson.Version;
import org.codehaus.jackson.map.AnnotationIntrospector;
import org.codehaus.jackson.map.ObjectMapper;
import org.codehaus.jackson.xc.JaxbAnnotationIntrospector;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.MultiLineString;
import com.vividsolutions.jts.geom.MultiPoint;
import com.vividsolutions.jts.geom.MultiPolygon;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;

/**
 * @author bclement
 * 
 */
public class JacksonFactory implements KeyedPoolableObjectFactory {

	protected static final List<Class<? extends Geometry>> supportedGeoms;

	static {
		supportedGeoms = new LinkedList<Class<? extends Geometry>>();
		supportedGeoms.add(Geometry.class);
		// supportedGeoms.add(Point.class);
		supportedGeoms.add(Polygon.class);
		supportedGeoms.add(MultiLineString.class);
		supportedGeoms.add(MultiPoint.class);
		supportedGeoms.add(MultiPolygon.class);
		supportedGeoms.add(LinearRing.class);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.apache.commons.pool.KeyedPoolableObjectFactory#activateObject(java
	 * .lang.Object, java.lang.Object)
	 */
	@Override
	public void activateObject(Object arg0, Object arg1) throws Exception {

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.apache.commons.pool.KeyedPoolableObjectFactory#destroyObject(java
	 * .lang.Object, java.lang.Object)
	 */
	@Override
	public void destroyObject(Object arg0, Object arg1) throws Exception {

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.apache.commons.pool.KeyedPoolableObjectFactory#makeObject(java.lang
	 * .Object)
	 */
	@Override
	public Object makeObject(Object arg0) throws Exception {
		FlexibleModule m = new FlexibleModule("FlexibleModule", new Version(0,
				0, 1, null));
		m.addSerializer(new GeometrySerializer());
		m.addDeserializers(supportedGeoms, new GeometryDeserializer());
		m.addSerializer(new EnvelopeSerializer());
		m.addDeserializer(Envelope.class, new EnvelopeDeserializer());
		m.addSerializer(new PointGeomSerialization.Serializer());
		m.addDeserializer(Point.class,
				new PointGeomSerialization.Deserializer());
		m.addSerializer(new CrsSerializer());
		m.addDeserializer(CoordinateReferenceSystem.class,
				new CrsDeserializer());
		m.addSerializer(new RefEnvelopeSerialization.Serializer());
		m.addDeserializer(ReferencedEnvelope.class,
				new RefEnvelopeSerialization.Deserializer());
		ObjectMapper mapper = new ObjectMapper();
		mapper.enableDefaultTyping();
		AnnotationIntrospector introspector = new JaxbAnnotationIntrospector();
		// make deserializer use JAXB annotations (only)
		mapper.getDeserializationConfig().setAnnotationIntrospector(
				introspector);
		// make serializer use JAXB annotations (only)
		mapper.getSerializationConfig().setAnnotationIntrospector(introspector);
		mapper.registerModule(m);
		return mapper;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.apache.commons.pool.KeyedPoolableObjectFactory#passivateObject(java
	 * .lang.Object, java.lang.Object)
	 */
	@Override
	public void passivateObject(Object arg0, Object arg1) throws Exception {

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.apache.commons.pool.KeyedPoolableObjectFactory#validateObject(java
	 * .lang.Object, java.lang.Object)
	 */
	@Override
	public boolean validateObject(Object arg0, Object arg1) {
		return true;
	}

}
