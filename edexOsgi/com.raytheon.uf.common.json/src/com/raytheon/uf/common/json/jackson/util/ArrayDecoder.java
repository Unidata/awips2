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
package com.raytheon.uf.common.json.jackson.util;

import java.io.IOException;
import java.util.ArrayList;

import org.codehaus.jackson.JsonParseException;
import org.codehaus.jackson.JsonParser;
import org.codehaus.jackson.JsonToken;
import org.codehaus.jackson.map.DeserializationContext;
import org.codehaus.jackson.map.JsonMappingException;

import com.vividsolutions.jts.geom.Envelope;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public class ArrayDecoder {

	public static double[][] decodeDbl2D(JsonParser jp,
			DeserializationContext ctxt) throws JsonParseException, IOException {
		ArrayList<double[]> rval = new ArrayList<double[]>();
		JsonToken tok = jp.getCurrentToken();
		checkArrayStart(tok, ctxt);
		tok = jp.nextToken();
		while (tok == JsonToken.START_ARRAY) {
			rval.add(decodeDbl(jp, ctxt));
			tok = jp.nextToken();
		}
		checkArrayEnd(tok, ctxt);
		return toPrimitive2D(rval);
	}

	public static double[] decodeDbl(JsonParser jp, DeserializationContext ctxt)
			throws JsonParseException, IOException {
		ArrayList<Double> rval = new ArrayList<Double>();
		JsonToken tok = jp.getCurrentToken();
		checkArrayStart(tok, ctxt);
		tok = jp.nextToken();
		while (tok == JsonToken.VALUE_NUMBER_FLOAT) {
			rval.add(jp.getDoubleValue());
			tok = jp.nextToken();
		}
		checkArrayEnd(tok, ctxt);
		return toPrimitive(rval);
	}

	protected static double[] toPrimitive(ArrayList<Double> list) {
		double[] rval = new double[list.size()];
		for (int i = 0; i < rval.length; ++i) {
			rval[i] = list.get(i);
		}
		return rval;
	}

	protected static double[][] toPrimitive2D(ArrayList<double[]> list) {
		double[][] rval = new double[list.size()][];
		for (int i = 0; i < rval.length; ++i) {
			rval[i] = list.get(i);
		}
		return rval;
	}

	public static void checkArrayStart(JsonToken tok,
			DeserializationContext ctxt) throws JsonMappingException {
		if (tok != JsonToken.START_ARRAY) {
			throw ctxt.mappingException(Envelope.class);
		}
	}

	public static void checkArrayEnd(JsonToken tok,
			DeserializationContext ctxt) throws JsonMappingException {
		if (tok != JsonToken.END_ARRAY) {
			throw ctxt.mappingException(Envelope.class);
		}
	}
}
