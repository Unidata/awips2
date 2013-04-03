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
package com.raytheon.uf.edex.ogc.common;

public class OgcStyle {

	protected String name;

	protected String title;

	protected String abs;

	protected String legendUrl;

	protected boolean isDefault = false;

	public OgcStyle() {
	}

	public OgcStyle(String name, String title, String abs) {
		this.name = name;
		this.title = title;
		this.abs = abs;
	}

	public OgcStyle(String name, String title) {
		this(name, title, null);
	}

	public OgcStyle(String name) {
		this(name, null, null);
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public String getAbs() {
		return abs;
	}

	public void setAbs(String abs) {
		this.abs = abs;
	}

	/**
	 * @return the legendUrl
	 */
	public String getLegendUrl() {
		return legendUrl;
	}

	/**
	 * @param legendUrl
	 *            the legendUrl to set
	 */
	public void setLegendUrl(String legendUrl) {
		this.legendUrl = legendUrl;
	}

	/**
	 * @return the isDefault
	 */
	public boolean isDefault() {
		return isDefault;
	}

	/**
	 * @param isDefault
	 *            the isDefault to set
	 */
	public void setDefault(boolean isDefault) {
		this.isDefault = isDefault;
	}

}
