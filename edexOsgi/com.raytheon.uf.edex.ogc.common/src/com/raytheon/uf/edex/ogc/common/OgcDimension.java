/**********************************************************************
*
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
**********************************************************************/
/**
 * 
 */
package com.raytheon.uf.edex.ogc.common;

import java.util.List;

/**
 * @author bclement
 * 
 */
public class OgcDimension {

	protected String name;

	protected String units;

	protected String unitSymbol;

	protected List<String> values;

	protected String defaultVal;

	/**
	 * 
	 */
	public OgcDimension() {
	}

	public OgcDimension(String name, List<String> values) {
		this(name, "", values);
	}

	public OgcDimension(String name, String units, List<String> values) {
		this(name, units, null, values);
	}

	public OgcDimension(String name, String units, String unitSymbol,
			List<String> values) {
		this.name = name;
		this.units = units;
		this.unitSymbol = unitSymbol;
		this.values = values;
	}

	/**
	 * @return the name
	 */
	public String getName() {
		return name;
	}

	/**
	 * @param name
	 *            the name to set
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * @return the units
	 */
	public String getUnits() {
		return units;
	}

	/**
	 * @param units
	 *            the units to set
	 */
	public void setUnits(String units) {
		this.units = units;
	}

	/**
	 * @return the unitSymbol
	 */
	public String getUnitSymbol() {
		return unitSymbol;
	}

	/**
	 * @param unitSymbol
	 *            the unitSymbol to set
	 */
	public void setUnitSymbol(String unitSymbol) {
		this.unitSymbol = unitSymbol;
	}

	/**
	 * @return the values
	 */
	public List<String> getValues() {
		return values;
	}

	/**
	 * @param values
	 *            the values to set
	 */
	public void setValues(List<String> values) {
		this.values = values;
	}

	/**
	 * @return the defaultVal
	 */
	public String getDefaultVal() {
		return defaultVal;
	}

	/**
	 * @param defaultVal
	 *            the defaultVal to set
	 */
	public void setDefaultVal(String defaultVal) {
		this.defaultVal = defaultVal;
	}

}
