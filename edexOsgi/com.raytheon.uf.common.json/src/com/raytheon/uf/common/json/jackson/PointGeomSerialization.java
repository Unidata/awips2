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
 * Jul 21, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.common.json.jackson;

import java.io.IOException;

import org.codehaus.jackson.JsonGenerator;
import org.codehaus.jackson.JsonParser;
import org.codehaus.jackson.JsonProcessingException;
import org.codehaus.jackson.map.DeserializationContext;
import org.codehaus.jackson.map.JsonDeserializer;
import org.codehaus.jackson.map.JsonSerializer;
import org.codehaus.jackson.map.SerializerProvider;

import com.raytheon.uf.common.json.jackson.util.ArrayDecoder;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public class PointGeomSerialization {

	public static class Deserializer extends JsonDeserializer<Point> {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * org.codehaus.jackson.map.JsonDeserializer#deserialize(org.codehaus
		 * .jackson.JsonParser, org.codehaus.jackson.map.DeserializationContext)
		 */
		@Override
		public Point deserialize(JsonParser jp, DeserializationContext ctxt)
				throws IOException, JsonProcessingException {
			double[] v = ArrayDecoder.decodeDbl(jp, ctxt);
			return new GeometryFactory()
					.createPoint(new Coordinate(v[0], v[1]));
		}

	}

	public static class Serializer extends JsonSerializer<Point> {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * org.codehaus.jackson.map.JsonSerializer#serialize(java.lang.Object,
		 * org.codehaus.jackson.JsonGenerator,
		 * org.codehaus.jackson.map.SerializerProvider)
		 */
		@Override
		public void serialize(Point value, JsonGenerator jgen,
				SerializerProvider provider) throws IOException,
				JsonProcessingException {
			jgen.writeStartArray();
			jgen.writeNumber(value.getX());
			jgen.writeNumber(value.getY());
			jgen.writeEndArray();

		}

		@Override
		public Class<Point> handledType() {
			return Point.class;
		}

	}
}
