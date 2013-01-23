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
 *
 */
package com.raytheon.uf.common.json.jackson;

import java.io.IOException;

import org.codehaus.jackson.JsonGenerator;
import org.codehaus.jackson.JsonProcessingException;
import org.codehaus.jackson.map.JsonSerializer;
import org.codehaus.jackson.map.SerializerProvider;

import com.vividsolutions.jts.geom.Envelope;

public class EnvelopeSerializer extends JsonSerializer<Envelope> {

	@Override
	public void serialize(Envelope value, JsonGenerator jgen,
			SerializerProvider provider) throws IOException,
			JsonProcessingException {
		// this serializes as a 2D array, the xml serializer can only do 1d
		// arrays.
		// jgen.writeStartArray();
		// jgen.writeStartArray();
		// jgen.writeNumber(value.getMinX());
		// jgen.writeNumber(value.getMinY());
		// jgen.writeEndArray();
		// jgen.writeStartArray();
		// jgen.writeNumber(value.getMaxX());
		// jgen.writeNumber(value.getMaxY());
		// jgen.writeEndArray();
		// jgen.writeEndArray();

		// xml serializer compatible. Downside is that mongo spatial index works
		// on 2d arrays
		jgen.writeStartArray();
		jgen.writeNumber(value.getMinX());
		jgen.writeNumber(value.getMinY());
		jgen.writeNumber(value.getMaxX());
		jgen.writeNumber(value.getMaxY());
		jgen.writeEndArray();
	}

	@Override
	public Class<Envelope> handledType() {
		return Envelope.class;
	}

}
