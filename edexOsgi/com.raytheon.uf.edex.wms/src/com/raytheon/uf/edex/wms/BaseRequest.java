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
 * Sep 7, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.edex.wms;

import com.raytheon.uf.edex.ogc.common.OgcResponse;
import com.raytheon.uf.edex.ogc.common.OgcServiceInfo;
import com.raytheon.uf.edex.ogc.common.http.MimeType;
import com.raytheon.uf.edex.wms.IWmsProvider.WmsOpType;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public class BaseRequest<T> {

	protected String version;

    protected MimeType format;

	protected String userName;

	protected String[] roles;

    protected MimeType exceptionFormat = OgcResponse.TEXT_XML_MIME;

	protected String updateSequence;

	protected OgcServiceInfo<T> serviceinfo;

	/**
	 * 
	 */
	public BaseRequest() {
		// TODO Auto-generated constructor stub
	}

    public BaseRequest(String version, MimeType format, String userName,
			String[] roles) {
		super();
		this.version = version;
		this.format = format;
		this.userName = userName;
		this.roles = roles;
	}

	@SuppressWarnings("unchecked")
	public OgcResponse execute(IWmsProvider provider) {
		return provider.getCapabilities((BaseRequest<WmsOpType>) this);
	}

	/**
	 * @return the version
	 */
	public String getVersion() {
		return version;
	}

	/**
	 * @param version
	 *            the version to set
	 */
	public void setVersion(String version) {
		this.version = version;
	}

	/**
	 * @return the format
	 */
    public MimeType getFormat() {
		return format;
	}

	/**
	 * @param format
	 *            the format to set
	 */
    public void setFormat(MimeType format) {
		this.format = format;
	}

	/**
	 * @return the exceptionFormat
	 */
    public MimeType getExceptionFormat() {
		return exceptionFormat;
	}

	/**
	 * @param exceptionFormat
	 *            the exceptionFormat to set
	 */
    public void setExceptionFormat(MimeType exceptionFormat) {
		this.exceptionFormat = exceptionFormat;
	}

	/**
	 * @return the updateSequence
	 */
	public String getUpdateSequence() {
		return updateSequence;
	}

	/**
	 * @param updateSequence
	 *            the updateSequence to set
	 */
	public void setUpdateSequence(String updateSequence) {
		this.updateSequence = updateSequence;
	}

	/**
	 * @return the serviceinfo
	 */
	public OgcServiceInfo<T> getServiceinfo() {
		return serviceinfo;
	}

	/**
	 * @param serviceinfo
	 *            the serviceinfo to set
	 */
	public void setServiceinfo(OgcServiceInfo<T> serviceinfo) {
		this.serviceinfo = serviceinfo;
	}

	/**
	 * @return the userName
	 */
	public String getUserName() {
		return userName;
	}

	/**
	 * @param userName
	 *            the userName to set
	 */
	public void setUserName(String userName) {
		this.userName = userName;
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

}
