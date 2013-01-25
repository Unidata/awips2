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

import java.io.IOException;

import org.codehaus.jackson.JsonParser;
import org.codehaus.jackson.JsonProcessingException;
import org.codehaus.jackson.JsonToken;
import org.codehaus.jackson.map.DeserializationContext;
import org.codehaus.jackson.map.JsonDeserializer;
import org.codehaus.jackson.map.TypeDeserializer;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.io.ParseException;
import com.vividsolutions.jts.io.WKTReader;

/**
 * @author bclement
 * 
 */
public class GeometryDeserializer extends JsonDeserializer<Geometry> {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.codehaus.jackson.map.JsonDeserializer#deserialize(org.codehaus.jackson
	 * .JsonParser, org.codehaus.jackson.map.DeserializationContext)
	 */
	@Override
	public Geometry deserialize(JsonParser jp, DeserializationContext ctxt)
			throws IOException, JsonProcessingException {
		if (jp.getCurrentToken() != JsonToken.VALUE_STRING) {
			throw ctxt.mappingException(Geometry.class);
		}
		WKTReader reader = new WKTReader();
		try {
			return reader.read(jp.getText());
		} catch (ParseException e) {
			throw new IOException(e);
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.codehaus.jackson.map.JsonDeserializer#deserializeWithType(org.codehaus
	 * .jackson.JsonParser, org.codehaus.jackson.map.DeserializationContext,
	 * org.codehaus.jackson.map.TypeDeserializer)
	 */
	@Override
	public Object deserializeWithType(JsonParser jp,
			DeserializationContext ctxt, TypeDeserializer typeDeserializer)
			throws IOException, JsonProcessingException {
		return super.deserializeWithType(jp, ctxt, typeDeserializer);
	}

}
