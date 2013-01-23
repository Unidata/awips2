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
 * Aug 9, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.edex.wms.format;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.xml.transform.TransformerException;

import org.geotools.data.memory.MemoryFeatureCollection;
import org.geotools.feature.FeatureCollection;
import org.geotools.gml.producer.FeatureTransformer;
import org.geotools.gml3.GMLConfiguration;
import org.geotools.referencing.CRS;
import org.geotools.xml.Configuration;
import org.geotools.xml.Encoder;
import org.opengis.feature.simple.SimpleFeature;
import org.opengis.feature.simple.SimpleFeatureType;
import org.opengis.feature.type.Name;

import com.raytheon.uf.edex.ogc.common.OgcNamespace;
import com.raytheon.uf.edex.ogc.common.OgcPrefix;
import com.raytheon.uf.edex.ogc.common.OgcResponse;
import com.raytheon.uf.edex.ogc.common.OgcResponse.TYPE;
import com.raytheon.uf.edex.ogc.common.feature.SimpleFeatureFormatter;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public class GmlFeatureFormatter implements SimpleFeatureFormatter {

	public static final String mimeType = "text/xml; subtype=\"gml/3.1.1\"";

	public static final String empty = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
			+ "<wfs:FeatureCollection xmlns:edex=\"http://edex.uf.raytheon.com\"\n"
			+ "xmlns:ogc=\"http://www.opengis.net/ogc\"\n"
			+ "xmlns:gml=\"http://www.opengis.net/gml\"\n"
			+ "xmlns:xlink=\"http://www.w3.org/1999/xlink\"\n"
			+ "xmlns:ows=\"http://www.opengis.net/ows\" xmlns:wfs=\"http://www.opengis.net/wfs\">\n"
			+ "</wfs:FeatureCollection>";

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.wms.format.SimpleFeatureFormatter#format(java.util
	 * .List)
	 */
	@Override
	public OgcResponse format(List<List<SimpleFeature>> features)
			throws Exception {

		if (features == null || features.isEmpty()) {
			return new OgcResponse(empty, mimeType, TYPE.TEXT);
		}
		List<FeatureCollection<SimpleFeatureType, SimpleFeature>> colls = getAsCollections(features);
		StringBuilder rval = new StringBuilder();
		populate(rval, colls);
		return new OgcResponse(rval.toString(), mimeType, TYPE.TEXT);
	}

	protected void populate(StringBuilder sb,
			List<FeatureCollection<SimpleFeatureType, SimpleFeature>> colls)
			throws IOException {
		Configuration conf = new GMLConfiguration();
		Encoder encoder = new Encoder(conf);
		Iterator<FeatureCollection<SimpleFeatureType, SimpleFeature>> i = colls
				.iterator();
		String xml = toXml(i.next(), encoder);
		populate(sb, xml, true, !i.hasNext());
		while (i.hasNext()) {
			xml = toXml(i.next(), encoder);
			populate(sb, xml, false, !i.hasNext());
		}
	}

	protected String toXml(FeatureCollection<SimpleFeatureType, SimpleFeature> coll, Encoder encoder)
			throws IOException {
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		FeatureTransformer trans = new FeatureTransformer();
		trans.setIndentation(4);
		trans.setGmlPrefixing(true);
		trans.getFeatureTypeNamespaces().declareDefaultNamespace(OgcPrefix.GML,
				OgcNamespace.GML);
		SimpleFeatureType schema = coll.getSchema();
		Name name = schema.getName();
		trans.getFeatureTypeNamespaces().declareNamespace(schema,
				name.getLocalPart(), name.getNamespaceURI());
		trans.setCollectionNamespace(null);
		trans.setCollectionPrefix(null);
		trans.setCollectionBounding(true);
		trans.getFeatureNamespaces();
		String srs = CRS.toSRS(schema.getCoordinateReferenceSystem());
		if (srs != null) {
			trans.setSrsName(srs);
		}
		try {
			trans.transform(coll, out);
		} catch (TransformerException e) {
			throw new IOException(e);
		}
		return new String(out.toByteArray(), encoder.getEncoding());
	}

	protected void populate(StringBuilder sb, String xml, boolean header,
			boolean footer) {
		String[] parts = split(xml);
		if (header) {
			sb.append(parts[0]);
			sb.append('\n');
		}
		sb.append(parts[1]);
		sb.append('\n');
		if (footer) {
			sb.append(parts[2]);
			sb.append('\n');
		}
	}

	protected String[] split(String xml) {
		String[] rval = new String[3];
		int endHeader;
		int beginBody;
		int endBody;
		int beginFooter;
		// FIXME xml should not be parsed like this
		int i = xml.indexOf("FeatureCollection");
		endHeader = xml.indexOf('>', i) + 1;
		i = xml.indexOf("featureMember");
		beginBody = xml.lastIndexOf('<', i);
		i = xml.lastIndexOf("featureMember");
		endBody = xml.indexOf('>', i) + 1;
		i = xml.lastIndexOf("FeatureCollection");
		beginFooter = xml.lastIndexOf('<', i);
		rval[0] = xml.substring(0, endHeader);
		rval[1] = xml.substring(beginBody, endBody);
		rval[2] = xml.substring(beginFooter);
		return rval;
	}

	protected List<FeatureCollection<SimpleFeatureType, SimpleFeature>> getAsCollections(
			List<List<SimpleFeature>> features) {
		List<FeatureCollection<SimpleFeatureType, SimpleFeature>> colls = new ArrayList<FeatureCollection<SimpleFeatureType, SimpleFeature>>(
				features.size());
		SimpleFeature sample;
		for (List<SimpleFeature> l : features) {
			if (l == null || l.isEmpty()) {
				continue;
			}
			sample = l.get(0);
			MemoryFeatureCollection coll = new MemoryFeatureCollection(
					sample.getFeatureType());
			coll.addAll(l);
			// ListFeatureCollection coll = new ListFeatureCollection(
			// sample.getFeatureType(), l);
			colls.add(coll);
		}
		return colls;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.uf.edex.wms.format.SimpleFeatureFormatter#getMimeType()
	 */
	@Override
	public String getMimeType() {
		return mimeType;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.ogc.common.feature.SimpleFeatureFormatter#matchesFormat
	 * (java.lang.String)
	 */
	@Override
	public boolean matchesFormat(String format) {
		if ( mimeType.equalsIgnoreCase(format)){
			return true;
		}
		if (format.toLowerCase().contains("gml")) {
			return true;
		}
		return false;
	}

}
