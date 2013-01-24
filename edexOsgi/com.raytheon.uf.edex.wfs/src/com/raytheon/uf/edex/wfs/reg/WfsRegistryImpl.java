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
 * Apr 11, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.edex.wfs.reg;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javax.xml.bind.JAXBException;

import net.opengis.gml.v_3_1_1.ObjectFactory;

import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.util.registry.RegistryException;
import com.raytheon.uf.edex.ogc.common.OgcNamespace;
import com.raytheon.uf.edex.ogc.common.OgcPrefix;
import com.raytheon.uf.edex.ogc.common.jaxb.OgcJaxbManager;
import com.raytheon.uf.edex.wfs.WfsException;
import com.raytheon.uf.edex.wfs.WfsFeatureType;
import com.raytheon.uf.edex.wfs.request.QualifiedName;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public class WfsRegistryImpl implements WfsRegistry {

	protected Map<String, WfsSource> byKey = new HashMap<String, WfsSource>();

	protected Map<QualifiedName, WfsSource> byFeature = new HashMap<QualifiedName, WfsSource>();

	protected Class<?>[] jaxbClasses = new Class<?>[] { ObjectFactory.class,
			net.opengis.wfs.v_1_1_0.ObjectFactory.class,
			net.opengis.filter.v_1_1_0.ObjectFactory.class, Unique.class };

	protected OgcJaxbManager jaxbManager;

	protected Log log = LogFactory.getLog(this.getClass());

	protected long currentVersion = 0;

	protected long jaxbContextVersion = 0;

	protected String prefix = "wfs";

	protected static final Map<String, String> NS_MAP = new ConcurrentHashMap<String, String>();

	static {
		NS_MAP.put(OgcNamespace.EDEX, OgcPrefix.EDEX);
		NS_MAP.put(OgcNamespace.GML, OgcPrefix.GML);
		NS_MAP.put(OgcNamespace.OGC, OgcPrefix.OGC);
		NS_MAP.put(OgcNamespace.OWS, OgcPrefix.OWS);
		NS_MAP.put(OgcNamespace.WFS, OgcPrefix.WFS);
		NS_MAP.put(OgcNamespace.XSI, OgcPrefix.XSI);
		NS_MAP.put(OgcNamespace.XLINK, OgcPrefix.XLINK);
	}

	public WfsRegistryImpl() {

	}

	@Override
	public synchronized WfsRegistry register(final WfsSource source)
			throws RegistryException {
		addByKey(source);
		addByFeature(source);
		jaxbClasses = (Class<?>[]) ArrayUtils.addAll(jaxbClasses,
				source.getJaxbClasses());
		currentVersion++;

		return this;
	}

	protected OgcJaxbManager getManager() throws JAXBException {
		if (jaxbManager == null || jaxbContextVersion < currentVersion) {
			jaxbContextVersion = currentVersion;
			jaxbManager = new OgcJaxbManager(jaxbClasses);
			jaxbManager.setPrefixMap(NS_MAP);
		}
		return jaxbManager;
	}

	public Object unmarshal(String xml) throws JAXBException {
		return getManager().unmarshal(xml);
	}

	public String marshal(Object obj) throws JAXBException {
		return getManager().marshal(obj);
	}

	protected void addByKey(WfsSource source) throws RegistryException {
		String key = source.getKey();
		if (byKey.containsKey(key)) {
			throw new RegistryException("WFS Source already exists with key: "
					+ key);
		}
		byKey.put(source.getKey(), source);
	}

	protected void addByFeature(WfsSource source) throws RegistryException {
		for (WfsFeatureType f : source.listFeatureTypes()) {
			QualifiedName feature = f.getName();
			if (byFeature.containsKey(feature)) {
				throw new RegistryException(
						"Already providing a feature with name: " + feature);
			}
			String prefix = (feature.getPrefix() != null ? feature.getPrefix()
					: feature.getName());
			NS_MAP.put(feature.getNamespace(), prefix);
			byFeature.put(f.getName(), source);
		}
	}

	protected void removeAll(WfsSource source) {
		WfsSource removed = byKey.remove(source.getKey());
		if (removed != null) {
			for (WfsFeatureType f : removed.listFeatureTypes()) {
				byFeature.remove(f.getName());
			}
		}
	}

	@Override
	public synchronized WfsRegistry unregister(WfsSource source) {
		removeAll(source);
		return this;
	}

	public WfsSource getSource(QualifiedName feature) throws WfsException {
		WfsSource rval = byFeature.get(feature);
		return rval;
	}

	public List<WfsFeatureType> getFeatures() {
		List<WfsFeatureType> rval = new LinkedList<WfsFeatureType>();
		for (WfsSource source : byKey.values()) {
			rval.addAll(source.listFeatureTypes());
		}
		return rval;
	}

	/**
	 * @return the prefix
	 */
	public String getPrefix() {
		return prefix;
	}

	/**
	 * @param prefix
	 *            the prefix to set
	 */
	public void setPrefix(String prefix) {
		this.prefix = prefix;
	}


}
