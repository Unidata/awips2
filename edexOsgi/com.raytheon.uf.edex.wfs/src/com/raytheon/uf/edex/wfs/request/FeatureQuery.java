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
package com.raytheon.uf.edex.wfs.request;

import java.util.LinkedList;
import java.util.List;

/**
 * @author bclement
 * 
 */
public class FeatureQuery {

	public enum QFilterType {
		XML, BBOX, FIDS, XMLOBJ, NONE
	}

	protected String srsName;

	protected List<QualifiedName> typeNames = new LinkedList<QualifiedName>();

	protected QFilterType filterType = QFilterType.NONE;

	protected Object filter;

	protected List<SortBy> sortBys = new LinkedList<SortBy>();

	protected List<String> propertyNames = new LinkedList<String>();

	public void addPropertyName(String propertyName) {
		this.propertyNames.add(propertyName);
	}

	public void addTypeName(QualifiedName typeName) {
		this.typeNames.add(typeName);
	}

	public void addSortBy(SortBy sortBy) {
		this.sortBys.add(sortBy);
	}

	/**
	 * @return the propertyNames
	 */
	public List<String> getPropertyNames() {
		return propertyNames;
	}

	/**
	 * @param propertyNames
	 *            the propertyNames to set
	 */
	public void setPropertyNames(List<String> propertyNames) {
		this.propertyNames = propertyNames;
	}

	/**
	 * @return the srsName
	 */
	public String getSrsName() {
		return srsName;
	}

	/**
	 * @param srsName
	 *            the srsName to set
	 */
	public void setSrsName(String srsName) {
		this.srsName = srsName;
	}

	/**
	 * @return the typeNames
	 */
	public List<QualifiedName> getTypeNames() {
		return typeNames;
	}

	/**
	 * @param typeNames
	 *            the typeNames to set
	 */
	public void setTypeNames(List<QualifiedName> typeNames) {
		this.typeNames = typeNames;
	}

	/**
	 * @return the filter
	 */
	public Object getFilter() {
		return filter;
	}

	/**
	 * @param filter
	 *            the filter to set
	 */
	public void setFilter(Object filter, QFilterType type) {
		this.filter = filter;
		this.filterType = type;
	}

	/**
	 * @return the filterType
	 */
	public QFilterType getFilterType() {
		return filterType;
	}

	/**
	 * @return the sortBys
	 */
	public List<SortBy> getSortBys() {
		return sortBys;
	}

	/**
	 * @param sortBys
	 *            the sortBys to set
	 */
	public void setSortBys(List<SortBy> sortBys) {
		this.sortBys = sortBys;
	}
}
