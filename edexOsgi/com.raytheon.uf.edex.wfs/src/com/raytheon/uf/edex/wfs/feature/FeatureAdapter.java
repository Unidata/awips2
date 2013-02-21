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
 * Jul 19, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.edex.wfs.feature;

import java.util.Collection;
import java.util.Map;

import org.geotools.feature.simple.SimpleFeatureBuilder;
import org.opengis.feature.Feature;
import org.opengis.feature.simple.SimpleFeatureType;
import org.opengis.feature.type.FeatureType;
import org.opengis.feature.type.PropertyDescriptor;

import com.raytheon.uf.common.json.JsonException;
import com.raytheon.uf.common.json.JsonService;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public class FeatureAdapter {

	protected JsonService jsonService;

	public FeatureAdapter(JsonService json) {
		this.jsonService = json;
	}

	public Feature convert(Object obj, FeatureType ft) throws JsonException {
		Collection<PropertyDescriptor> descriptors = ft.getDescriptors();
		Map<String, Object> map = jsonService.extract(obj);
		SimpleFeatureBuilder builder = new SimpleFeatureBuilder(
				(SimpleFeatureType) ft);
		for (PropertyDescriptor pd : descriptors) {
			String name = pd.getName().getLocalPart();
			builder.set(name, map.get(name));
		}
		return builder.buildFeature(null);
	}
}
