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
 * Apr 22, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.edex.wfs.provider;

import java.util.Map;


/**
 * 
 * @author bclement
 * @version 1.0
 */
public class VisitorBag {

	protected Class<?> rootEntity;

	protected String spatialField;

	protected Map<String, String> fieldMap;

	/**
	 * @param converter
	 * @param rootEntity
	 * @param spatialField
	 */
	public VisitorBag(Class<?> rootEntity,
			String spatialField) {
		super();
		this.rootEntity = rootEntity;
		this.spatialField = spatialField;
	}

	public String filterField(String field) {
		if (fieldMap == null || field == null) {
			return field;
		}
		String rval = fieldMap.get(field);
		return rval != null ? rval : field;
	}

	public Class<?> getRootEntity() {
		return rootEntity;
	}

	public void setRootEntity(Class<?> rootEntity) {
		this.rootEntity = rootEntity;
	}

	public String getSpatialField() {
		return spatialField;
	}

	public void setSpatialField(String spatialField) {
		this.spatialField = spatialField;
	}

	public Map<String, String> getFieldMap() {
		return fieldMap;
	}

	public void setFieldMap(Map<String, String> fieldMap) {
		this.fieldMap = fieldMap;
	}

}
