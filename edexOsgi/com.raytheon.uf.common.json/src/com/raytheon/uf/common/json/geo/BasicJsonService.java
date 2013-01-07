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
 * Aug 10, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.common.json.geo;

import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Map;
import java.util.TreeMap;

import org.codehaus.jackson.map.AnnotationIntrospector;
import org.codehaus.jackson.map.ObjectMapper;
import org.codehaus.jackson.map.ObjectWriter;
import org.codehaus.jackson.xc.JaxbAnnotationIntrospector;

import com.raytheon.uf.common.json.JsonException;
import com.raytheon.uf.common.json.JsonService;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public class BasicJsonService implements JsonService {

	protected ObjectMapper mapper;

	public BasicJsonService() {
		mapper = new ObjectMapper();
		AnnotationIntrospector introspector = new JaxbAnnotationIntrospector();
		// make deserializer use JAXB annotations (only)
		mapper.getDeserializationConfig().setAnnotationIntrospector(
				introspector);
		// make serializer use JAXB annotations (only)
		mapper.getSerializationConfig().setAnnotationIntrospector(introspector);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.uf.common.json.JsonService#serialize(java.lang.Object)
	 */
	@Override
	public String serialize(Object obj, boolean pretty) throws JsonException {
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		serialize(obj, baos, pretty);
		return baos.toString();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.uf.common.json.JsonService#extract(java.lang.Object)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public Map<String, Object> extract(Object obj) throws JsonException {
		try {
			return mapper.convertValue(obj, TreeMap.class);
		} catch (Exception e) {
			throw new JsonException("Problem extracting object to map", e);
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.uf.common.json.JsonService#populate(java.util.Map,
	 * java.lang.Class)
	 */
	@Override
	public Object populate(Map<String, Object> map, Class<?> c)
			throws JsonException {
		try {
			return mapper.convertValue(map, c);
		} catch (Exception e) {
			throw new JsonException("Problem extracting object to map", e);
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.uf.common.json.JsonService#serialize(java.lang.Object,
	 * java.io.OutputStream)
	 */
	@Override
	public void serialize(Object obj, OutputStream out, boolean pretty)
			throws JsonException {
		try {
			ObjectWriter writer = pretty ? mapper.defaultPrettyPrintingWriter()
					: mapper.writer();
			writer.writeValue(out, obj);
		} catch (Exception e) {
			throw new JsonException("Problem serializing object to JSON", e);
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.common.json.JsonService#deserialize(java.lang.String,
	 * java.lang.Class)
	 */
	@Override
	public Object deserialize(String json, Class<?> c) throws JsonException {
		try {
			return mapper.readValue(json, c);
		} catch (Exception e) {
			throw new JsonException("Problem deserializing object to JSON", e);
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.common.json.JsonService#deserialize(java.io.InputStream,
	 * java.lang.Class)
	 */
	@Override
	public Object deserialize(InputStream in, Class<?> c) throws JsonException {
		try {
			return mapper.readValue(in, c);
		} catch (Exception e) {
			throw new JsonException("Problem deserializing object to JSON", e);
		}
	}

}
