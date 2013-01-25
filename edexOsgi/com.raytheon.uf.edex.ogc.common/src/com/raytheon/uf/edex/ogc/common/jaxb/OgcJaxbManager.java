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
 * Mar 30, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.edex.ogc.common.jaxb;

import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.StringReader;
import java.util.Map;
import java.util.Queue;
import java.util.concurrent.ConcurrentLinkedQueue;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.sun.xml.bind.marshaller.NamespacePrefixMapper;

/**
 * Cache and utility class for OGC JAXB
 * 
 * @author bclement
 * @version 1.0
 */
public class OgcJaxbManager {

	protected JAXBContext jaxbContext = null;

	protected int QUEUE_SIZE = 10;

	protected Queue<Unmarshaller> unmarshallers = new ConcurrentLinkedQueue<Unmarshaller>();

	protected Queue<Marshaller> marshallers = new ConcurrentLinkedQueue<Marshaller>();

	protected int unmarshallersCreated = 0;

	protected int marshallersCreated = 0;

	protected Log log = LogFactory.getLog(this.getClass());

	protected Map<String, String> prefixMap = null;

	private NamespacePrefixMapper mapper;

	protected static final String JAXB_NAMESPACE_MAPPER = "com.sun.xml.bind.namespacePrefixMapper";

	public OgcJaxbManager(Class<?>[] classes) throws JAXBException {
		jaxbContext = JAXBContext.newInstance(classes);
	}

	protected Unmarshaller getUnmarshaller() throws JAXBException {
		Unmarshaller m = unmarshallers.poll();
		if (m == null) {
			if (unmarshallersCreated < QUEUE_SIZE) {
				m = jaxbContext.createUnmarshaller();
				++unmarshallersCreated;
			} else {
				int tries = 0;
				do {
					try {
						Thread.sleep(50);
					} catch (InterruptedException e) {
						// ignore
					}
					m = unmarshallers.poll();
					tries++;
					if (tries >= 20) {
						log.debug("Unable to get jaxb unmarshaller from pool after "
								+ tries + " tries. Growing pool size.");
						m = jaxbContext.createUnmarshaller();
						++unmarshallersCreated;
					}
				} while (m == null);
			}
		}

		return m;
	}

	protected Marshaller getMarshaller() throws JAXBException {
		Marshaller m = marshallers.poll();
		if (m == null) {
			if (marshallersCreated < QUEUE_SIZE) {
				m = jaxbContext.createMarshaller();
				++marshallersCreated;
			} else {
				int tries = 0;
				do {
					try {
						Thread.sleep(50);
					} catch (InterruptedException e) {
						// ignore
					}
					m = marshallers.poll();
					tries++;
					if (tries >= 20) {
						log.debug("Unable to get jaxb marshaller from pool after "
								+ tries + " tries. Growing pool size.");
						m = jaxbContext.createMarshaller();
						++marshallersCreated;
					}
				} while (m == null);
			}
		}

		return m;
	}

	public Object unmarshal(String xml) throws JAXBException {
		Unmarshaller msh = null;
		try {
			msh = getUnmarshaller();
			StringReader reader = new StringReader(xml);
			Object obj = msh.unmarshal(reader);
			if (obj instanceof JAXBElement<?>) {
				obj = ((JAXBElement<?>) obj).getValue();
			}
			return obj;
		} finally {
			if (msh != null) {
				unmarshallers.add(msh);
			}
		}
	}

	public Object unmarshal(InputStream xml) throws JAXBException {
		Unmarshaller msh = null;
		try {
			msh = getUnmarshaller();
			Object obj = msh.unmarshal(xml);
			if (obj instanceof JAXBElement<?>) {
				obj = ((JAXBElement<?>) obj).getValue();
			}
			return obj;
		} finally {
			if (msh != null) {
				unmarshallers.add(msh);
			}
		}
	}

	public String marshal(Object obj) throws JAXBException {
		return marshal(obj, true);
	}

	public String marshal(Object obj, boolean formatted) throws JAXBException {
		return marshal(obj, null, formatted);
	}

	public void marshal(Object obj, OutputStream out, String schemaLocation,
			boolean formatted) throws JAXBException {
		Marshaller msh = getMarshaller();
		try {

			msh.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, formatted);
			if (mapper != null) {
				msh.setProperty(JAXB_NAMESPACE_MAPPER, mapper);
			}
			if (schemaLocation != null && !schemaLocation.isEmpty()) {
				msh.setProperty(Marshaller.JAXB_SCHEMA_LOCATION, schemaLocation);
			}
			msh.marshal(obj, out);
		} finally {
			marshallers.add(msh);
		}
	}

	public String marshal(Object obj, String schemaLocation, boolean formatted)
			throws JAXBException {
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		marshal(obj, out, schemaLocation, formatted);
		return out.toString();
	}

	public Map<String, String> getPrefixMap() {
		return prefixMap;
	}

	public void setPrefixMap(final Map<String, String> prefixMap) {
		this.prefixMap = prefixMap;
		this.mapper = new NamespacePrefixMapper() {
			@Override
			public String getPreferredPrefix(String namespaceUri,
					String suggestion, boolean requirePrefix) {
				return prefixMap.get(namespaceUri);
			}
		};
	}

	public JAXBContext getJaxbContext() {
		return jaxbContext;
	}

}
