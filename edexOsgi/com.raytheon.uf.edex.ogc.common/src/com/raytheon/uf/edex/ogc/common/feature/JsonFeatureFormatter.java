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
package com.raytheon.uf.edex.ogc.common.feature;

import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.List;

import org.geotools.data.memory.MemoryFeatureCollection;
import org.geotools.feature.FeatureCollection;
import org.opengis.feature.simple.SimpleFeature;
import org.opengis.feature.simple.SimpleFeatureType;

import com.raytheon.uf.common.json.geo.GeoJsonUtil;
import com.raytheon.uf.common.json.geo.GeoJsonUtilSimpleImpl;
import com.raytheon.uf.common.json.geo.MixedFeatureCollection;
import com.raytheon.uf.edex.ogc.common.OgcResponse;
import com.raytheon.uf.edex.ogc.common.OgcResponse.TYPE;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public class JsonFeatureFormatter implements SimpleFeatureFormatter {

	public static String mimeType = "application/json";

	protected GeoJsonUtil jsonUtil = new GeoJsonUtilSimpleImpl();

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
		List<FeatureCollection<SimpleFeatureType, SimpleFeature>> colls = new ArrayList<FeatureCollection<SimpleFeatureType, SimpleFeature>>(
				features.size());
		for (List<SimpleFeature> l : features) {
			if (l != null && !l.isEmpty()) {
				SimpleFeatureType t = l.get(0).getFeatureType();
				MemoryFeatureCollection c = new MemoryFeatureCollection(t);
				c.addAll(l);
				colls.add(c);
			}
		}
		MixedFeatureCollection mixed = new MixedFeatureCollection(colls);
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		jsonUtil.serialize(mixed, baos);
		return new OgcResponse(baos.toString(), mimeType, TYPE.TEXT);
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
		return mimeType.equalsIgnoreCase(format);
	}

}
