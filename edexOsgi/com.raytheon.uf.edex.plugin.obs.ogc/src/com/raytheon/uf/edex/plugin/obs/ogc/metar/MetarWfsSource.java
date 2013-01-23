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
 * Apr 27, 2011            bclement     Initial creation
 * 
 *
 */
package com.raytheon.uf.edex.plugin.obs.ogc.metar;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.PluginProperties;
import com.raytheon.uf.common.dataplugin.obs.metar.MetarRecord;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.plugin.obs.ogc.metar.feature.ObjectFactory;
import com.raytheon.uf.edex.wfs.WfsFeatureType;
import com.raytheon.uf.edex.wfs.reg.DefaultWfsSource;
import com.raytheon.uf.edex.wfs.request.QualifiedName;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public class MetarWfsSource extends DefaultWfsSource {

	private static final String schemaloc = "META-INF/schema/metar.xsd";

	private WfsFeatureType feature;

	private static String schemaXml = null;

	private static final String spatialKey = "location.location";

	private static final String KEY_NAME = "metar";

	private static final String METAR_NS = "http://metar.edex.uf.raytheon.com";

	private static final IUFStatusHandler statusHandler = UFStatus
			.getHandler(MetarWfsSource.class);
	
	private static final Map<String,String> fieldMap = new HashMap<String, String>(1);
	
	static {
		fieldMap.put("obsLocation.location", spatialKey);
		fieldMap.put("obsLocation.stationId", "location.stationId");
		fieldMap.put("obsLocation.elevation", "location.elevation");
		Collections.unmodifiableMap(fieldMap);
	}

	public MetarWfsSource(PluginProperties props) {
		super(props, KEY_NAME, new MetarTranslator(), new MetarFeatureFactory());
		feature = new WfsFeatureType(new QualifiedName(METAR_NS, key, key),
				key, defaultCRS, fullBbox);
	}

	@Override
	public Map<String, String> getFieldMap() {
		return fieldMap;
	}
	
	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.uf.edex.wfs.reg.WfsSource#listFeatureTypes()
	 */
	@Override
	public List<WfsFeatureType> listFeatureTypes() {
		return Arrays.asList(feature);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.wfs.reg.WfsSource#describeFeatureType(com.raytheon
	 * .uf.edex.wfs.request.QualifiedName)
	 */
	@Override
	public String describeFeatureType(QualifiedName feature) {
		// we only advertise one feature
		String rval;
		try {
			if (schemaXml == null) {
				ClassLoader loader = MetarWfsSource.class.getClassLoader();
				schemaXml = getResource(loader, schemaloc);
			}
			rval = schemaXml;
		} catch (Exception e) {
			statusHandler.error("Problem reading metar schema", e);
			rval = "Internal Error"; // TODO better default
		}
		return rval;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.wfs.reg.WfsSource#getFeatureSpatialField(com.raytheon
	 * .uf.edex.wfs.request.QualifiedName)
	 */
	@Override
	public String getFeatureSpatialField(QualifiedName feature) {
		// we only advertise one feature
		return spatialKey;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.wfs.reg.WfsSource#getFeatureEntity(com.raytheon.
	 * uf.edex.wfs.request.QualifiedName)
	 */
	@Override
	public Class<?> getFeatureEntity(QualifiedName feature) {
		// we only advertise one feature
		return MetarRecord.class;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.uf.edex.wfs.reg.WfsSource#getJaxbClasses()
	 */
	@Override
	public Class<?>[] getJaxbClasses() {
		return new Class<?>[] { ObjectFactory.class };
	}

}
