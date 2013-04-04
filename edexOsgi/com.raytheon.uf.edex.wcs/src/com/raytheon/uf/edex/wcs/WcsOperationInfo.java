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
package com.raytheon.uf.edex.wcs;

import java.util.LinkedList;
import java.util.List;

public class WcsOperationInfo {

	public enum Type {
		GetCapabilities, GetCoverage, DescribeCoverage
	}

	protected Type type;
	
	protected String httpPostRes;

	protected String httpGetRes;

	protected List<String> formats = new LinkedList<String>();

	/**
	 * 
	 */
	public WcsOperationInfo(Type type) {
		this.type = type;
	}

	public WcsOperationInfo(Type type, String httpPostRes, String httpGetRes) {
		this(type);
		this.httpGetRes = httpGetRes;
		this.httpPostRes = httpPostRes;
	}

	public void addFormat(String format) {
		formats.add(format);
	}

	public boolean hasHttpPost() {
		return httpPostRes != null;
	}

	public boolean hasHttpGet() {
		return httpGetRes != null;
	}

	/**
	 * @return the httpPostRes
	 */
	public String getHttpPostRes() {
		return httpPostRes;
	}

	/**
	 * @param httpPostRes
	 *            the httpPostRes to set
	 */
	public void setHttpPostRes(String httpPostRes) {
		this.httpPostRes = httpPostRes;
	}

	/**
	 * @return the httpGetRes
	 */
	public String getHttpGetRes() {
		return httpGetRes;
	}

	/**
	 * @param httpGetRes
	 *            the httpGetRes to set
	 */
	public void setHttpGetRes(String httpGetRes) {
		this.httpGetRes = httpGetRes;
	}

	/**
	 * @return the type
	 */
	public Type getType() {
		return type;
	}

	/**
	 * @param type
	 *            the type to set
	 */
	public void setType(Type type) {
		this.type = type;
	}

	/**
	 * @return the formats
	 */
	public List<String> getFormats() {
		return formats;
	}

	/**
	 * @param formats
	 *            the formats to set
	 */
	public void setFormats(List<String> formats) {
		this.formats = formats;
	}
}
