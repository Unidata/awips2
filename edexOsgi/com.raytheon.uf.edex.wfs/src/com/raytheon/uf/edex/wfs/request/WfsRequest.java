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
package com.raytheon.uf.edex.wfs.request;

import com.raytheon.uf.edex.ogc.common.OgcResponse;

public class WfsRequest {

	public enum Type {
		GetFeature, DescribeFeature, GetCapabilities, Transaction, ERROR
	}

	protected Object rawrequest;

	protected Type type;

	protected String username;

	protected String[] roles;

	private String exceptionFormat = OgcResponse.TEXT_XML_MIME;

	public WfsRequest(Type type) {
		super();
		this.type = type;
	}

	public Type getType() {
		return type;
	}

	public void setType(Type type) {
		this.type = type;
	}

	/**
	 * @return the rawrequest
	 */
	public Object getRawrequest() {
		return rawrequest;
	}

	/**
	 * @param rawrequest
	 *            the rawrequest to set
	 */
	public void setRawrequest(Object rawrequest) {
		this.rawrequest = rawrequest;
	}

	/**
	 * @return the username
	 */
	public String getUsername() {
		return username;
	}

	/**
	 * @param username
	 *            the username to set
	 */
	public void setUsername(String username) {
		this.username = username;
	}

	/**
	 * @return the roles
	 */
	public String[] getRoles() {
		return roles;
	}

	/**
	 * @param roles
	 *            the roles to set
	 */
	public void setRoles(String[] roles) {
		this.roles = roles;
	}

	/**
	 * @param exceptionFormat the exceptionFormat to set
	 */
	public void setExceptionFormat(String exceptionFormat) {
		this.exceptionFormat = exceptionFormat;
	}

	/**
	 * @return the exceptionFormat
	 */
	public String getExceptionFormat() {
		return exceptionFormat;
	}

}
