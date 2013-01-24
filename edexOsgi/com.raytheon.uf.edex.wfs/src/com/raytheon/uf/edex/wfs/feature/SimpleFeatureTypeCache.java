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

import java.lang.reflect.Field;
import java.util.concurrent.ConcurrentHashMap;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.geotools.feature.simple.SimpleFeatureTypeBuilder;
import org.opengis.feature.type.FeatureType;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.NoSuchAuthorityCodeException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.edex.ogc.common.spatial.CrsLookup;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public class SimpleFeatureTypeCache implements FeatureTypeCache {

	protected ConcurrentHashMap<FeatureTypeConfig, FeatureType> cache = new ConcurrentHashMap<FeatureTypeConfig, FeatureType>();

	protected Log log = LogFactory.getLog(this.getClass());

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.wfs.feature.FeatureTypeCache#getFeatureType(java
	 * .lang.Class)
	 */
	@Override
	public FeatureType getFeatureType(FeatureTypeConfig config)
			throws NoSuchAuthorityCodeException, FactoryException {
		if (config == null) {
			return null;
		}
		FeatureType rval = cache.get(config);
		if (rval == null) {
			rval = extractFeatureType(config);
			cache.put(config, rval);
		}

		return rval;
	}

	protected FeatureType extractFeatureType(FeatureTypeConfig config)
			throws NoSuchAuthorityCodeException, FactoryException {
		switch (config.method) {
		case JAXB:
			return extractFromJaxb(config);
		default:
			throw new RuntimeException("Invalid featuretype extraction method");
		}
	}

	/**
	 * Naive implementation. Does not account for embedded types, lists (arrays)
	 * or adapters.
	 * 
	 * @param config
	 * @return
	 * @throws NoSuchAuthorityCodeException
	 * @throws FactoryException
	 */
	protected FeatureType extractFromJaxb(FeatureTypeConfig config)
			throws NoSuchAuthorityCodeException, FactoryException {
		SimpleFeatureTypeBuilder builder = new SimpleFeatureTypeBuilder();
		builder.setName(config.getName());
		builder.setNamespaceURI(config.getNamespace());
		CoordinateReferenceSystem crs = CrsLookup.lookup(config.getCrs());
		builder.setDefaultGeometry(config.getGeomName());
		builder.setCRS(crs);
		Class<?> binding = config.getBinding();
		Field[] fields = binding.getFields();
		for (Field f : fields) {
			if (hasJaxb(f)) {
				String name = f.getName();
				Class<?> c = f.getDeclaringClass();
				builder.add(name, c);
			}
		}
		return builder.buildFeatureType();
	}

	protected boolean hasJaxb(Field f) {
		XmlElement xmle = f.getAnnotation(XmlElement.class);
		if (xmle != null) {
			return true;
		}
		XmlAttribute xmla = f.getAnnotation(XmlAttribute.class);
		if (xmla != null) {
			return true;
		}
		return false;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.wfs.feature.FeatureTypeCache#removeClass(java.lang
	 * .Class)
	 */
	@Override
	public FeatureTypeCache removeClass(FeatureTypeConfig config) {
		if (config != null) {
			cache.remove(config);
		}
		return this;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.uf.edex.wfs.feature.FeatureTypeCache#clear()
	 */
	@Override
	public FeatureTypeCache clear() {
		cache.clear();
		return this;
	}

}
