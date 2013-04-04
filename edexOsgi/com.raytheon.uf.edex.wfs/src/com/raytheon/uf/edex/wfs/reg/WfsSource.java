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
package com.raytheon.uf.edex.wfs.reg;

import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBElement;

import net.opengis.gml.v_3_1_1.AbstractFeatureType;

import org.opengis.feature.simple.SimpleFeature;

import com.raytheon.uf.edex.wfs.WfsException;
import com.raytheon.uf.edex.wfs.WfsFeatureType;
import com.raytheon.uf.edex.wfs.request.QualifiedName;

/**
 * @author bclement
 * 
 */
public interface WfsSource {

	public List<WfsFeatureType> listFeatureTypes();

	public String describeFeatureType(QualifiedName feature)
			throws WfsException;

	public String getFeatureSpatialField(QualifiedName feature);

	public Class<?> getFeatureEntity(QualifiedName feature);

	public Map<String, String> getFieldMap();

	public List<JAXBElement<? extends AbstractFeatureType>> query(
			QualifiedName feature, WfsQuery q) throws WfsException;

	public List<SimpleFeature> querySimple(QualifiedName feature, WfsQuery q)
			throws WfsException;

	public List<String> distinct(QualifiedName feature, WfsQuery q)
			throws WfsException;

	public long count(QualifiedName feature, WfsQuery q) throws WfsException;

	public String getKey();

	public Class<?>[] getJaxbClasses();
}
